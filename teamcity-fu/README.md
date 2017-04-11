Teamcity-fu
-----------
-- __Build-fu for the awesome enterprise__ --

<!-- insert here: a gnu eating a lambda -->

# `docker-fu`

One-for-all script to build and manage docker images on teamcity.  
All functionality is based on a file `build.manifest` and iterates over all its
non-empty lines (after discarding line comments starting with `#`).
Each line in `build.manifest` specifies the build of one docker image, by
providing a __target tag__ (`tag`) and a __working directory__ (`dir`).

<!-- TODO knock off a manifest description from the `tooling.docker` repo -->

The supported operations are

Invocation       | Description
-----------------|------------------------------------------------
`docker-fu info` | Display essential information about the image defined in the `Dockerfile` inside `dir` (such as base image, number of build steps)
`docker-fu pull` | Pull the necessary base images for all image builds specified in the manifest (based on `FROM` steps in the `Dockerfile`s found).  Note that images later in the manifest might be based on those specified earlier, `docker-fu pull` will recognise this and not try to pull.
`docker-fu build`| Build all images specified in the manifest, in top-down order
`docker-fu push` | Execute `docker push` for all images specified in the manifest, in top-down order
`docker-fu clean`| Remove all images specified in the manifest, in top-down order, if possible. If an image cannot be removed, report and proceed.
`docker-fu <something>`| You can execute any shell command that takes two arguments (a string constituting a docker `tag` and a relative path `dir`) and can reasonably be expected to exist. Try `docker-fu echo`!

