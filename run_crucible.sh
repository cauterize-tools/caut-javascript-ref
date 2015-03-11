#!/bin/sh

./.cabal-sandbox/bin/cauterize-test \
  crucible --build-cmd="../../.cabal-sandbox/bin/caut-javascript-ref-gen --spec=%s --meta=%m --output=js" \
           --run-cmd="node js/test_client.js" \
           --schema-count=1 \
           --instance-count=100 \
           --type-count=3 \
           --prototypes=combination \
           --enc-size=1024
