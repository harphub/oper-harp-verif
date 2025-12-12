#!/usr/bin/env bash
#this one starts container
# podman run -it localhost/carlos9917/harp:0.3 /bin/bash
# # this one starts container and removes it after 
podman run -it --rm localhost/harp:0.3 /bin/bash

