'use strict';

var ctype = require('./ctype.js');

function CRecord(fields) {
  this.fields = fields;
}
CRecord.prototype = Object.create(ctype.CType.prototype);
CRecord.prototype.cautproto = 'record';
CRecord.prototype.pack = function (cautBuffer) {
  var fieldIx, tfield, cfield;
  var sum = 0;

  for (fieldIx = 0; fieldIx < this.constructor.fields.length; fieldIx++) {
    cfield = this.constructor.fields[fieldIx];
    tfield = this.fields[cfield.name];

    if (undefined !== cfield.ctor) {
      sum += tfield.pack(cautBuffer);
    }
  }

  return sum;
};
CRecord.prototype.toJS = function () {
  var fieldIx, tfield, cfield;
  var jsFields = {};

  for (fieldIx = 0; fieldIx < this.constructor.fields.length; fieldIx++) {
    cfield = this.constructor.fields[fieldIx];
    tfield = this.fields[cfield.name];

    if (undefined !== cfield.ctor) {
      jsFields[cfield.name] = tfield.toJS();
    } else {
      jsFields[cfield.name] = null;
    }
  }

  return jsFields;
};
exports.CRecord = CRecord;

function unpack(recordCtor, cautBuffer) {
  var fieldIx, cfield;
  var fields = {};

  for (fieldIx = 0; fieldIx < recordCtor.fields.length; fieldIx++) {
    cfield = recordCtor.fields[fieldIx];
    fields[cfield.name] = cfield.ctor.unpack(cautBuffer);
  }

  return new recordCtor(fields);
}

function mkRecord(f, typename, fields, hash, size) {
  ctype.mkCType(f, typename, 'array', hash, size);
  f.fields = fields;

  f.prototype = Object.create(CRecord.prototype);
  f.prototype.constructor = f;

  f.unpack = function (cautBuffer) {
    return unpack(f, cautBuffer);
  };
}
exports.mkRecord = mkRecord;
