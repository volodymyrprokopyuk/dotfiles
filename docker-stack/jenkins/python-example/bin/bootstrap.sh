#!/usr/bin/env bash

set -e

VENV=venv

rm -rf $VENV
python3 -m venv $VENV
source $VENV/bin/activate

pip3 install -r requirements.txt
pip3 install -r requirements-test.txt
