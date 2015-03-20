/*global Uint8Array */
'use strict';

var ctype = require('./ctype.js');
var cast = require('../cast.js');

function calcIxs(ix) {
  return { byteIx: Math.floor(ix / 8), bitIx: ix % 8 };
}

function CCombination(fields) {
  this.fields = fields;
}
CCombination.prototype = Object.create(ctype.CType.prototype);
CCombination.prototype.cautproto = 'combination';
CCombination.prototype.pack = function (cautBuffer) {
  function fieldsToFlags(cfields, tfields, width) {
    var ix, ixs, cfield;
    var flags = new Uint8Array(width);

    for (ix = 0; ix < cfields.length; ix++) {
      cfield = cfields[ix];

      if (undefined !== tfields[cfield.name]) {
        ixs = calcIxs(cfield.index);
        flags[ixs.byteIx] |= (1 << ixs.bitIx);
      }
    }

    return flags;
  }

  var fieldIx, tfield, cfield;
  var flags = fieldsToFlags(this.constructor.fields, this.fields, this.constructor.flagsWidth);
  var sum = this.constructor.flagsWidth;

  cautBuffer.addU8Array(flags);

  for (fieldIx = 0; fieldIx < this.constructor.fields.length; fieldIx++) {
    cfield = this.constructor.fields[fieldIx];
    tfield = this.fields[cfield.name];

    if (undefined !== tfield && undefined !== cfield.ref) {
      sum += tfield.pack(cautBuffer);
    }
  }

  return sum;
};
CCombination.prototype.toJS = function () {
  var fieldIx, tfield, cfield;
  var jsFields = {};

  for (fieldIx = 0; fieldIx < this.constructor.fields.length; fieldIx++) {
    cfield = this.constructor.fields[fieldIx];
    tfield = this.fields[cfield.name];

    if (tfield !== undefined) {
      if (null === tfield) {
        jsFields[cfield.name] = null;
      } else {
        jsFields[cfield.name] = tfield.toJS();
      }
    }
  }

  return jsFields;
};
exports.CCombination = CCombination;

function unpack(combinationCtor, cautBuffer) {
  function flagsIncludeIndex(flags, index) {
    var ixs = calcIxs(index);
    return (flags[ixs.byteIx] & (1 << ixs.bitIx));
  }

  var flags = cast.someBytes(cautBuffer, combinationCtor.flagsWidth);
  var fieldIx, cfield;
  var fields = {};

  for (fieldIx = 0; fieldIx < combinationCtor.fields.length; fieldIx++) {
    cfield = combinationCtor.fields[fieldIx];
    if (flagsIncludeIndex(flags, cfield.index)) {
      if (undefined !== cfield.ref) {
        fields[cfield.name] = cfield.ref.unpack(cautBuffer);
      } else {
        fields[cfield.name] = null;
      }
    }
  }

  return new combinationCtor(fields);
}

function mkCombination(f, typename, fields, flagsWidth, hash, size) {
  ctype.mkCType(f, typename, 'combination', hash, size);
  f.fields = fields;
  f.flagsWidth = flagsWidth;

  f.prototype = Object.create(CCombination.prototype);
  f.prototype.constructor = f;

  f.unpack = function (cautBuffer) {
    return unpack(f, cautBuffer);
  };
}
exports.mkCombination = mkCombination;
