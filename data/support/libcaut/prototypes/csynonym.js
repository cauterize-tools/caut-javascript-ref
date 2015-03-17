'use strict';

var ctype = require('./ctype.js');

function CSynonym(value) {
  if (!(value instanceof this.constructor.synType)) {
    throw new Error("Expected " + this.constructor.synType.name);
  }

  this.value = value;
}
CSynonym.prototype = Object.create(ctype.CType.prototype);

CSynonym.prototype.cautproto = 'synonym';
CSynonym.prototype.pack = function (cautBuffer) {
  return this.value.pack(cautBuffer);
};
CSynonym.prototype.toJS = function () {
  return this.value.toJS();
};

CSynonym.unpack = function (synCtor, cautBuffer) {
  var value = synCtor.synType.unpack(cautBuffer);
  return new synCtor(value);
};

exports.CSynonym = CSynonym;

function mkSyn(f, typename, synType, hash, size) {
  ctype.mkCType(f, typename, 'synonym', hash, size);

  f.prototype = Object.create(CSynonym.prototype);
  f.prototype.constructor = f;

  f.unpack = function (cautBuffer) {
    return CSynonym.unpack(f, cautBuffer);
  };

  f.synType = synType;

  return f;
}
exports.mkSyn = mkSyn;
