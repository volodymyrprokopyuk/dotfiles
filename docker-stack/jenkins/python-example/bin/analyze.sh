#!/usr/bin/env bash

set -e

VENV=venv
SOURCE=timedelta

source $VENV/bin/activate

echo "flake8 code style check"
flake8 $SOURCE || true

echo "pylint code check"
pylint $SOURCE || true

echo "bandit security check"
bandit -r $SOURCE || true

echo "radon cyclomatic complexity"
radon cc $SOURCE || true

echo "radon raw metrics"
radon raw $SOURCE || true

echo "radon maintainability index"
radon mi $SOURCE || true

echo "radon Halsted metrics"
radon hal $SOURCE || true
