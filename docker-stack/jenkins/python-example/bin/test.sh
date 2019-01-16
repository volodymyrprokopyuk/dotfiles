#!/usr/bin/env bash

set -e

VENV=venv
SOURCE=timedelta
export PYTHONPATH=$SOURCE

source $VENV/bin/activate

pytest -v -s -x --cov $SOURCE --cov-report term --cov-report html test
