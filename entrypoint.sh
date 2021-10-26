#!/bin/sh

REDIS="$(redis)"

if [ $REDIS != "false" ]
then
    redis-server /redis.conf --port 6379 2>&1 | sed 's/^/[redis] /' >&2 &
fi

/prod/bin/prod console
