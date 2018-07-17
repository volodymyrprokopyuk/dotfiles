#!/usr/bin/env bash

set -eu

POSTGRES_HOST=localhost
POSTGRES_PORT=5432
KEYCLOAK_HOST=localhost
KEYCLOAK_PORT=9090
TIMEOUT=10

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

docker stack deploy -c postgres-compose.yml postgres
wait_for_port $POSTGRES_HOST $POSTGRES_PORT

POSTGRES_CONTAINER_ID=$(docker ps --filter "name=postgres" --quiet)
echo "POSTGRES_CONTAINER_ID=$POSTGRES_CONTAINER_ID"

echo "Creating database and users with permissions"
docker exec -it $POSTGRES_CONTAINER_ID psql -h $POSTGRES_HOST -p $POSTGRES_PORT -U postgres -d postgres -f /home/001_create_database_and_users.sql

# docker logs -f $POSTGRES_CONTAINER_ID

# docker stack rm postgres

docker stack deploy -c keycloak-compose.yml keycloak
wait_for_port $KEYCLOAK_HOST $KEYCLOAK_PORT

KEYCLOAK_CONTAINER_ID=$(docker ps --filter "name=keycloak" --quiet)
echo "KEYCLOAK_CONTAINER_ID=$KEYCLOAK_CONTAINER_ID"

docker logs -f $KEYCLOAK_CONTAINER_ID

# docker stack rm keycloak
