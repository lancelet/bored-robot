#!/usr/bin/env bash
#
# docker-fu.sh: tools to build docker images on teamcity
#
# Essential commands:
# - pull:  pulls all required base images for the build
# - build: builds all specified images
# - push:  pushes all specified images (assumes they have been built)
# - clean: removes all images that should have been built, and unnamed images
# - info:  prints out infromation about all specified images
#
#  As requested by the product owner, this is functional bash.
######################################################################

set -e

if [ ${BASH_VERSINFO[0]} -lt 4 ]; then
    echo "The version of bash that you are using is too old." >&2
    exit 1
fi

######################################################################
# ugly globals

# some globals that we inherited from the non-functional ancestors
DOCKER_REGISTRY=${DOCKER_REGISTRY:-omnia.docker.dev.cba}
PUSH_IMAGES=${PUSH_IMAGES:-true}
TAG_WITH_COMMIT=${TAG_WITH_COMMIT:-false}

# some globals that we considered useful by ourselves
commit=$(git rev-parse HEAD | cut -c 1-7)
timestamp=$(date "+%Y%m%d%H%M%S")
common=$PWD/common

######################################################################
# old code that we keep for the time being

# Renders a templated Dockerfile to stdout
function render_template() {
    tmpl=${1:-Dockerfile.tmpl}
    sed -e "s/##DOCKER_REGISTRY##/$DOCKER_REGISTRY/g" $tmpl
}

declare -A CACHED

function is_cached() {
    local dir
    for dir in "${!CACHED[@]}"; do
        if [ "$1" = "$dir" ]; then
            # $dir is in $CACHED
            return 0
        fi
    done
    return 1
}

function docker_tag() {
    SRC=$1
    DST=$2

    set +e
    docker rmi $DST > /dev/null 2>&1
    set -e

    docker tag $SRC $DST
}

######################################################################
# worker functions

# pull required base images for build -- unless reference is local
function pull() {
    img=$1
    dir=$2
    
    # retrieve FROM in the Dockerfile (assume there is exactly one)
      base_image=$(grep FROM $dir/Dockerfile | awk '{print $2}')

      if [[ -z ${base_image} ]]; then
          printf "no base image for %s" "$dir/Dockerfile"
          exit 1
      fi

      printf "Base image %s\n" "$base_image"
      
      # if it refers to docker hub (mirrored) -> pull
      if [[ ${base_image} =~ ^hub.docker.dev.cba/.* ]]; then
          printf "External source. Pulling...\n"
          docker pull ${base_image}

      # otherwise, if it refers to DOCKER_REGISTRY from the start, and
      # if the result does not occur in the manifest -> pull
      elif [[ ${base_image} =~ ${DOCKER_REGISTRY}/(.*) ]]; then
          if grep -e "^${BASH_REMATCH[1]}" build.manifest; then
              printf "...found in manifest, skipping.\n"
          else 
              printf "Not locally built. Pulling...\n"
              docker pull ${base_image}
          fi
      fi
      printf "\n"
}
    
# builds from the Dockerfile in $dir, tagging as $img and as $img-$commit
# NOTE: should we have an add_tag_suffix function?
function build() {
    local img
    img=$1
    local dir
    dir=$2

    pushd $dir > /dev/null

    if [ -e "Dockerfile.tmpl" ]; then
        render_template Dockerfile.tmpl > Dockerfile
    fi

    echo "Building..."
    if [ -d $common ]; then
        find $common -mindepth 1 -maxdepth 1 -exec cp -a '{}' . \;
    fi
    docker build --no-cache -t $img .

    docker_tag $img $img-$commit

    popd > /dev/null
}

# removes the image (with and without commit suffix)
function push() {
    local img
    img=$1

    # push the more specific tag first to avoid breaking the previous
    # one if there is a temporary problem.
    docker push $img-$commit
    docker push $img
    
}

# removes the image (with and without commit suffix)
# gracefully proceeds when images cannot be removed
function clean() {
    local img
    img=$1

    set +e
    docker rmi $img
    docker rmi $img-$commit
    set -e
    
}


# prints out some statistics and information
function info() {
    img=$1
    dir=$2

    pushd $dir > /dev/null

    printf "\nDocker Image %s\n\t(state as per commit %s)\n" \
           "$img" "$(git rev-parse HEAD | cut -c 1-7 )"
    printf "Source: \t%s\n"          "$dir"
    printf "Based on\t%s\n"          "$(grep FROM Dockerfile | cut -d' ' -f2)"
    printf "Dockerfile:\t%d lines\n" "$(cat Dockerfile | wc -l)"
    printf "\t\t%d build steps (%d potentially large)\n" \
           "$(grep -E -e "^[A-Z]" Dockerfile | wc -l )" \
           "$(grep -E -e "^RUN" Dockerfile | wc -l )"
    printf -- "---------------------------------------------\n"

    popd > /dev/null
}

######################################################################
#
# (statefully) map a function over all image def.s in the manifest
# The argument should be a function defined in the file, or a well-known
# built-in function, and takes two arguments: $image and $subdir
function for_manifest() {
    
    fun_name=$1
    echo "Running ${fun_name} on manifest"
    
    # strip line comments (starting by #), then ignore empty lines
    sed -e 's/#.*$//g' build.manifest | grep -v -E -e "^\s*$" | \
        while read manifest || [[ -n $manifest ]]; do
            pushd . >/dev/null

            # e.g. manifest:
            #
            # omnia/example:1.0 version-1
            # ^^^^^^^^^^^^^---------------- repo
            #               ^^^------------ tag
            #                   ^^^^^^^^^-- subdir
            repo=$(echo $manifest | cut -d' ' -f1 | cut -d':' -f1)
            tag=$(echo $manifest | cut -d' ' -f1 | cut -d':' -f2)
            subdir=$(echo $manifest | cut -d' ' -f2)
            image=$DOCKER_REGISTRY/$repo:$tag

            echo "==> $image"

            ${fun_name} $image $subdir

        done
    echo "Done with ${fun_name} on manifest."
}

######################################################################
# main: processing a command

for_manifest ${1:-info}
