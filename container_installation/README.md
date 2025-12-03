# Running harp in a container in atos using apptainer (formerly known as singularity)

These instructions explain how to compile the `harp` libraries
to create an image that can be uploaded to dockerhub.

For testing purposes, the author has created a harp installation
in [this image uploaded to dockerhub](https://hub.docker.com/r/carlos9917/harp).

The `Dockerfile` is included in this path.
We use podman in this example, but you can also use docker
(simply replace podman by docker in all the commands below)

## Creating the image

For creating the image, run the script `build_image.sh`, that
will run these commands:

```
podman build --build-arg GITHUB_PAT=$GITHUB_PAT -t harp:0.3 .
```
You will need to export your github personal access token [PAT](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) into the variable `GITHUB_PAT`:
```
export GITHUB_PAT=your_personal_access_token_from_github

```

This will build an image like this:
```
[cap@pf5eeefl container_installation] (master)$ podman images
REPOSITORY                 TAG         IMAGE ID      CREATED      SIZE
localhost/carlos9917/harp  0.3         96cf16bc9304  4 hours ago  2.47 GB
localhost/harp             0.3         96cf16bc9304  4 hours ago  2.47 GB
<none>                     <none>      11acbe11e640  4 hours ago  1.86 GB
<none>                     <none>      b45e0eb8a86f  4 hours ago  2.27 GB
docker.io/library/ubuntu   24.04       c3a134f2ace4  6 weeks ago  80.6 MB

```

where the image you are going to push is `localhost/harp`.

You can test a container based on this image running the script `test_container.sh`, that
simply runs
```
podman run -it --rm localhost/harp:0.3 /bin/bash
```
This will open a container where you can run R and load harp and then
it will remove the container after you logout.


## Pushing the image to dockerhub
Once you built the image in your laptop you can use it to dockerhub
using the scripts `push_image.sh`, that includes the commands below:

```
podman login docker.io
podman tag localhost/harp:0.3 {your_user_name_in_dockerhub}/harp:0.3
podman push {your_user_name_in_dockerhub}/harp:0.3

```
Replace your user name in the script.


## Building a container in atos
Before being able to use the image in atos
an run a container there you need to create a Singularity Image Format (SIF).
The script `submit_pull_harp.sh` will help you do that.
Simply submit it as an SLURM job (it might take a bit long the first time)
and wait (took me about 1 hr in my case). Then you will get a file
`harp_0.3.sif` in the path where you submitted the job (`$PERM/apptainer_build` in the example).

## Running a container in ator using apptainer

You can test a container in atos using
```
apptainer shell ./harp_0.3.sif
```

This will open a window where you are in container with
a local installation of harp based on the image above. 
You can run commands on the files in the path you are sitting on
or simply run R and harp interactively.
Logout as usual to leave the container.

To use the harp installation non-interactively you can use the `apptainer run` command.
For example:
```
apptainer run /perm/tratos2/apptainer_build/harp_0.3.sif Rscript scripts/panel_main.R
```
will the R installation that includes harp in a container created with the image in `harp_0.3.sif`
and run the script under `scripts/panel_main.R`.
