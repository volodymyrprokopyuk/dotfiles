#!/usr/bin/env bash

set -eu

HOST=localhost
PORT=8999
DOCKER_PORT=8080
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

wait_for_jenkins() {
    local container_id=${1:?"ERROR: container id is not definec"}
    local host=${2:?"ERROR: host is not defined"}
    local port=${3:?"ERROR: port is not defined"}
    local timeout=${4:-$TIMEOUT}
    while ! docker exec -it $container_id curl "http://$host:$port"; do
        sleep 1
        (( timeout-- ))
        if [[ $timeout -eq 0 ]]; then
            echo "ERROR: Jenkins server has not started correctly"
            exit 1
        fi
    done
}

mkdir -p jenkins_home
docker stack deploy -c jenkins-stack.yml jenkins
wait_for_port $HOST $PORT

CONTAINER_ID=$(docker ps --filter "name=jenkins" --quiet)
echo "CONTAINER_ID=$CONTAINER_ID"
wait_for_jenkins $CONTAINER_ID $HOST $DOCKER_PORT

echo "Installing Python..."
docker exec -it -u root $CONTAINER_ID bash -c "apt-get update && apt-get install -y python3 python3-pip python3-venv"

docker logs -f $CONTAINER_ID

# docker stack rm jenkins
