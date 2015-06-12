{-# LANGUAGE QuasiQuotes #-}
module Cauterize.JavaScript.TestClientTemplate
  ( testClientFromSpec
  ) where

import Data.String.Interpolate
import Data.Text.Lazy (unpack)

import qualified Cauterize.Specification as S

testClientFromSpec :: S.Spec -> String
testClientFromSpec spec =
  let ln = unpack $ S.specName spec
  in [i|
/*global Uint8Array */
'use strict';

var lib = require('./lib#{ln}.js');
var c = require('./libcaut/cauterize.js');
var cb = require('./libcaut/buffer.js');

var buf = new cb.CautBuffer();

process.stdin.on('readable', function () {
  var chunk = process.stdin.read();
  if (null !== chunk) {
    buf.addU8Array(new Uint8Array(chunk));
  }
});

process.stdin.on('end', function () {
  var clib = new c.Cauterize(lib.SpecificationInfo);

  var t = clib.decode(buf);

  var e = clib.encode(t);
  var ad = e.allData();

  process.stdout.write(new Buffer(ad));
});
|]

