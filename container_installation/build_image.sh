#/usr/bin/env bash
#podman build -t harp:0.3 .
# export your github personal access token into
# the GITHUB_PAT variable before doing this!

if [ -z $GITHUB_PAT ]; then
echo "Please define GITHUB_PAT with your github access token"
echo "ie, export GITHUB_PAT=your_personal_access_token"
exit 1
else
podman build --build-arg GITHUB_PAT=$GITHUB_PAT -t harp:0.3 .
fi

