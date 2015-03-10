#!/bin/sh

./.cabal-sandbox/bin/cauterize-test \
  crucible --build-cmd="../../.cabal-sandbox/bin/caut-javascript-ref-gen --spec=%s --meta=%m --output=js" \
           --run-cmd="node js/test_client.js" \
           --schema-count=1 \
           --instance-count=50 \
           --type-count=50 \
           --prototypes=synonym,array,vector,record \
           --enc-size=1024
