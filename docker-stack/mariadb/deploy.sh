#!/usr/bin/env bash

set -eu

HOST=localhost
PORT=3306
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

wait_for_mysql() {
    local container_id=${1:?"ERROR: container id is not definec"}
    local host=${2:?"ERROR: host is not defined"}
    local port=${3:?"ERROR: port is not defined"}
    local user=${4:?"ERROR: user is not defined"}
    local password=${5:?"ERROR: password is not defined"}
    local timeout=${6:-$TIMEOUT}
    while ! docker exec -it $container_id mysql -h$host -P$port -u$user -p"$password" -e"\q"; do
        sleep 1
        (( timeout-- ))
        if [[ $timeout -eq 0 ]]; then
            echo "ERROR: MySQL server has not started correctly"
            exit 1
        fi
    done
}

docker stack deploy -c mariadb-stack.yml mariadb
wait_for_port $HOST $PORT

CONTAINER_ID=$(docker ps --filter "name=mariadb" --quiet)
echo "CONTAINER_ID=$CONTAINER_ID"
wait_for_mysql $CONTAINER_ID $HOST $PORT root 'Password1!'

echo "Creating database and users with permissions"
docker exec -it $CONTAINER_ID mysql -h$HOST -P$PORT -uroot -p'Password1!' -e"source /home/001_create_database_and_users.sql"
echo "Creating database schema"
docker exec -it $CONTAINER_ID mysql -h$HOST -P$PORT -uvld -p'Password1!' -e"source /home/002_create_schema.sql" people
echo "Testing DML"
docker exec -it $CONTAINER_ID mysql -h$HOST -P$PORT -uvld -p'Password1!' -e"source /home/003_test_dml.sql" people

docker logs -f $CONTAINER_ID

# docker stack rm mariadb
