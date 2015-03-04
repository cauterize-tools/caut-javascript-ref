#!/bin/sh

# This is what John uses to test the caut-c11-sync generator locally. You may
# need to tweak this to fit your local configuration.

./.cabal-sandbox/bin/cauterize-test \
  crucible --build-cmd="../../.cabal-sandbox/bin/caut-javascript-ref-gen --spec=%s --meta=%m --output=js" \
           --run-cmd="node js/test_client.js" \
           --schema-count=1 \
           --instance-count=1 \
           --type-count=3 \
           --enc-size=1024
