#!/usr/bin/env bash

set -eu

wait_for_port() {
    local host=${1:?"ERROR: host is not defined"}
    local port=${2:?"ERROR: port is not defined"}
    local timeout=${3:-10}
    while ! nc -z $host $port; do
        sleep 1
        ((timeout--))
        if [[ $timeout -eq 0 ]]; then
            echo "ERROR: conainer has not started correctly"
            exit 1
        fi
    done
}

docker stack deploy -c docker-compose.yml sqlserver
wait_for_port localhost 1433

CONTAINER_ID=$(docker ps --filter "name=sqlserver" --quiet)
echo "CONTAINER_ID=$CONTAINER_ID"

echo "Creating database and user with permissions"
docker exec -it $CONTAINER_ID /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P 'Password!1' -d master -i /home/001_create_database_and_user.sql
echo "Creating database schema"
docker exec -it $CONTAINER_ID /opt/mssql-tools/bin/sqlcmd -S localhost -U family -P 'Password!1' -d people -i /home/002_create_schema.sql

docker logs -f $CONTAINER_ID

# docker stack rm sqlserver
