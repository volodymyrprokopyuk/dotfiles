#!/usr/bin/env bash

set -eu

docker stack deploy -c docker-compose.yml sqlserver

while ! nc -z localhost 1433; do
    sleep 1
done
CONTAINER_ID=$(docker ps --filter "name=sqlserver" --quiet)
echo "CONTAINER_ID=$CONTAINER_ID"

echo "Creating database and user with permissions"
docker exec -it $CONTAINER_ID /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P 'Password!1' -d master -i /home/001_create_database_and_user.sql
echo "Creating database schema"
docker exec -it $CONTAINER_ID /opt/mssql-tools/bin/sqlcmd -S localhost -U family -P 'Password!1' -d people -i /home/002_create_schema.sql

if [[ -n $CONTAINER_ID ]]; then
    docker logs -f $CONTAINER_ID
else
    echo "ERROR: conainer has not started correctly"
fi

# docker stack rm sqlserver
