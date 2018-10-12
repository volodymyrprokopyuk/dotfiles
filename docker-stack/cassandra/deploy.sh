#!/usr/bin/env bash

set -eu

HOST=localhost
PORT=9042
TIMEOUT=30

wait_for_port() {
    local host=${1:?"ERROR: host is not defined"}
    local port=${2:?"ERROR: port is not defined"}
    local timeout=${3:-$TIMEOUT}
    while ! nc -z $host $port; do
        sleep 1
        (( timeout-- ))
        if [[ $timeout -eq 0 ]]; then
            echo "ERROR: conainer has not started correctly"
            exit 1
        fi
    done
}

wait_for_cqlsh() {
    local container_id=${1:?"ERROR: container id is not definec"}
    local host=${2:?"ERROR: host is not defined"}
    local port=${3:?"ERROR: port is not defined"}
    local timeout=${4:-$TIMEOUT}
    while ! docker exec -it $container_id cqlsh $host $port -e quit; do
        sleep 1
        (( timeout-- ))
        if [[ $timeout -eq 0 ]]; then
            echo "ERROR: Cassandra server has not started correctly"
            exit 1
        fi
    done
}

docker stack deploy -c cassandra-stack.yml cassandra
wait_for_port $HOST $PORT

CONTAINER_ID=$(docker ps --filter "name=cassandra" --quiet)
echo "CONTAINER_ID=$CONTAINER_ID"
wait_for_cqlsh $CONTAINER_ID $HOST $PORT

echo "Creating keyspace"
docker exec -it $CONTAINER_ID cqlsh $HOST $PORT -e "SOURCE '/home/001_create_keyspace.cql'"
echo "Creating keyspace schema"
docker exec -it $CONTAINER_ID cqlsh $HOST $PORT -k people -e "SOURCE '/home/002_create_schema.cql'"
echo "Testing DML"
docker exec -it $CONTAINER_ID cqlsh $HOST $PORT -k people -e "SOURCE '/home/003_test_dml.cql'"

docker logs -f $CONTAINER_ID

# docker stack rm cassandra
