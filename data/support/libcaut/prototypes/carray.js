'use strict';

var ctype = require('./ctype.js');

function CArray(elements) {
  if (elements.length !== this.constructor.elemLength) {
    throw new Error("Unexpected number of elements. Expected " + this.constructor.elemLength.toString() +
                    " but got " + elements.length.toString());
  }

  this.elements = elements;
}
CArray.prototype = Object.create(ctype.CType.prototype);
CArray.prototype.cautproto = 'array';
CArray.prototype.pack = function (cautBuffer) {
  var i, sum = 0;
  for (i = 0; i < this.elements.length; i++) {
    sum += this.elements[i].pack(cautBuffer);
  }
  return sum;
};
CArray.prototype.toJS = function () {
  var jsElems = [];

  var i;
  for (i = 0; i < this.elements.length; i++) {
    jsElems.push(this.elements[i].toJS());
  }

  return jsElems;
};

CArray.unpack = function (arrayCtor, cautBuffer) {
  var i, elems = [];

  for (i = 0; i < arrayCtor.elemLength; i++) {
    elems.push(arrayCtor.elemType.unpack(cautBuffer));
  }

  return new arrayCtor(elems);
};
exports.CArray = CArray;

function mkArray(f, typename, elemType, length, hash, size) {
  ctype.mkCType(f, typename, 'array', hash, size);
  f.elemType = elemType;
  f.elemLength = length;

  f.prototype = Object.create(CArray.prototype);
  f.prototype.constructor = f;

  f.unpack = function (cautBuffer) {
    return CArray.unpack(f, cautBuffer);
  };
}
exports.mkArray = mkArray;
