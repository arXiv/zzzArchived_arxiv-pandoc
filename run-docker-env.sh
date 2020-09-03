#!/usr/bin/env bash

#
# You can find more information there about this script.
#


: "${IMG_NAME:=arxiv-pandoc}"
: "${IMG_VER:=latest}"

# Set this to the empty string to use locally built image:
if ! [[ -v "DHUB_PREFIX" ]]; then
  : "${DHUB_PREFIX:=arxiv/}"
fi

# If really an empty string, then we interpret as using a local image
# and do not pull:
if ! [ -z "$DHUB_PREFIX" ]; then
  docker pull "${DHUB_PREFIX}${IMG_NAME}:${IMG_VER}"
fi

docker run --rm -ti \
       --volume "$PWD":/opt/project \
       "${DHUB_PREFIX}${IMG_NAME}:${IMG_VER}" "$@"

# Add this before the last line (image name) for debugging:
#        --entrypoint "/bin/bash" \