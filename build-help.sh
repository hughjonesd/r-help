#!/bin/bash

set -eo pipefail

mkdir site

{
  printf '%s\n' pre 0.x 1.x 2.x
  curl -fsSL \
    https://raw.githubusercontent.com/r-hub/evercran/main/containers/versions-bookworm.txt
  curl -fsSL \
    https://raw.githubusercontent.com/r-hub/evercran/main/containers/versions-trixie.txt
} | while read -r IMAGE; do
  CONTAINER="r-help-${IMAGE//./-}"
  PLATFORM=""
  case "$IMAGE" in
    pre | 0.* | 1.* | 2.* ) PLATFORM="--platform=linux/i386" ;;
  esac

  docker pull "ghcr.io/r-hub/evercran/$IMAGE"
  docker create --name "$CONTAINER" $PLATFORM -i -t \
    "ghcr.io/r-hub/evercran/$IMAGE"
  docker cp guest-build-help.sh "$CONTAINER:/root/"
  docker cp guest-render-help.R "$CONTAINER:/root/"
  docker start "$CONTAINER"
  docker exec "$CONTAINER" chmod a+x /root/guest-build-help.sh
  docker exec "$CONTAINER" mkdir /root/site

  ENTRYPOINT=""
  case "$IMAGE" in
    0.* | 1.* ) ENTRYPOINT="entrypoint.sh" ;;
  esac
  docker exec "$CONTAINER" $ENTRYPOINT /root/guest-build-help.sh
  docker cp "$CONTAINER:/root/site/." site

  docker rm -f "$CONTAINER"
  docker image rm "ghcr.io/r-hub/evercran/$IMAGE"
done

Rscript build-indexes.R
