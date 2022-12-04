#!/bin/sh
psql -h localhost -p 5432 -U postgres -f "scripts/day${1}.sql"