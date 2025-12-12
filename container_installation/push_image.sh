#/usr/bin/env bash
podman login docker.io
podman tag localhost/harp:0.3 carlos9917/harp:0.3
podman push carlos9917/harp:0.3


