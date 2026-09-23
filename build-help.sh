#!/bin/bash

set -e

mkdir site

while read -r IMAGE; do
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
done < evercran-images.txt

Rscript build-indexes.R
