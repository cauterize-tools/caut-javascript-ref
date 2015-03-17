/*global ArrayBuffer */
'use strict';

function CType() {
  return;
}

CType.prototype.pack = function () {
  throw new Error("No pack function defined!");
};

CType.prototype.toJS = function () {
  throw new Error("No toSJ function defined!");
};

Object.freeze(CType);
exports.CType = CType;

function mkCType(f, typename, protoname, hash, size) {
  f.typename = typename;
  f.prototype = protoname;
  f.hash = hash;
  f.size = size;

  return f;
}

exports.mkCType = mkCType;
