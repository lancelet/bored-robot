#!/bin/bash
#
# Cleaning agent for the cleaning agent
#
# The script tries to clean up containers and images from
# a docker-enabled teamcity build agent
#
#######################################

aiaa_prefix="omnia.docker.dev.cba/omnia/aiaa"

dangling_images=$(docker images -f "dangling=true" -q)

aiaa_images=$(docker images | \
                     grep -e "${aiaa_prefix}" | \
                     awk '{printf "%s:%s ", $1, $2}')

exited_containers=$(docker ps -a -f status=exited | \
                           grep -E -e "(hours|days|weeks) ago" | \
                           awk '{print $1}')

printf "\nContainers:\n------------------\n"
docker ps -a
printf "\nImages:\n------------------\n"
docker images

case ${1:-report} in
    report)
        printf "%d AIAA images\n"  "$(echo ${aiaa_images} | wc -w)"
        printf "%d dangling images\n" "$(echo ${dangling_images} | wc -w)"
        printf "%d exited conainers\n"  "$(echo ${exited_containers} | wc -w)"
        ;;
    delete-aiaa)
        printf "Deleting %d AIAA images\n" "$(echo ${aiaa_images} | wc -w)"
        [ -z "${aiaa_images}" ] || docker rmi ${aiaa_images}
        ;;
    delete-dangling)
        printf "Deleting %d dangling images\n" "$(echo ${dangling_images} | wc -w)"
        [ -z "${dangling_images}" ] || docker rmi ${dangling_images}
        ;;
    delete-exited)
        printf "Exited containers:\n------------------\n"
        docker ps -a -f status=exited
        printf "IDs to delete: %s\n" "${exited_containers}"
        [ -z "${exited_containers}" ] || docker rm ${exited_containers}
        ;;
    *)
        printf "Unknown command $1\n"
        ;;
esac

