#!/bin/sh

./.cabal-sandbox/bin/cauterize-test \
  crucible --build-cmd="../../dist/build/caut-javascript-ref-gen/caut-javascript-ref-gen --spec=%s --output=js" \
           --run-cmd="node js/test_client.js" \
           --schema-count=1 \
           --instance-count=100 \
           --type-count=100 \
           --prototypes=synonym,array,vector,record \
           --enc-size=1024
