#!/usr/bin/env bash

set -eu

/opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P 'Password!1' -d master -i /init.sql/init_database.sql
