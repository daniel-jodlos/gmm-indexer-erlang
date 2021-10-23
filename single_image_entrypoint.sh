#!/bin/sh

redis-server /redis.conf --port 6379 2>&1 | sed 's/^/[redis] /' >&2 &

/prod/bin/prod console
