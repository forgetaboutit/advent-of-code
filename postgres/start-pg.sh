#!/bin/sh
docker run \
-e POSTGRES_PASSWORD=foobar \
-e POSTGRES_HOST_AUTH_METHOD=trust \
--name postgres-aoc \
--mount type=bind,source="$(pwd)",target=/code \
-p 5432:5432/tcp \
postgres:15.1-alpine3.17
