#!/usr/bin/env bash

set -eu

HOST=localhost
PORT=5432
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

docker stack deploy -c postgres-stack.yml postgres
wait_for_port $HOST $PORT

CONTAINER_ID=$(docker ps --filter "name=postgres" --quiet)
echo "CONTAINER_ID=$CONTAINER_ID"

echo "Creating database and users with permissions"
docker exec -it $CONTAINER_ID psql -h $HOST -p $PORT -U postgres -d postgres -f /home/001_create_database_and_users.sql
echo "Creating database schema"
docker exec -it $CONTAINER_ID psql -h $HOST -p $PORT -U family_ddl_dml -d people -f /home/002_create_schema.sql
echo "Testing DML"
docker exec -it $CONTAINER_ID psql -h $HOST -p $PORT -U family_ddl_dml -d people -f /home/003_test_dml.sql

docker logs -f $CONTAINER_ID

# docker stack rm postgres
