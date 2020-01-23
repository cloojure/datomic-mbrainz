#!/bin/bash -v

cd /opt/datomic

bin/datomic restore-db file:///opt/mbrainz-1968-1973  datomic:dev://localhost:4334/mbrainz-1968-1973

