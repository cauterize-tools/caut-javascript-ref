/*global ArrayBuffer, Uint8Array, Uint16Array, Uint32Array */
'use strict';

var builtin = require('./builtin_lib.js');

function bytes1ToInt(u8array, offset) {
  return u8array[offset];
}

function bytes2ToInt(u8array, offset) {
  var sz = 2;
  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var u16view = new Uint16Array(buf);

  var i; for (i = 0; i < sz; i++) {
    u8view[i] = u8array[offset + i];
  }

  return u16view[0];
}

function bytes4ToInt(u8array, offset) {
  var sz = 4;
  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var u32view = new Uint32Array(buf);

  var i; for (i = 0; i < sz; i++) {
    u8view[i] = u8array[offset + i];
  }

  return u32view[0];
}

exports.DataInterface = function (info) {
  // Store the specification/meta information
  this.info = info;

  this.data = new Uint8Array(0);

  this.addData = function(ab) {
    var tmp = new Uint8Array(this.data.byteLength + ab.byteLength);
    tmp.set(this.data, 0);
    tmp.set(ab, this.data.byteLength);
    this.data = tmp;
  };

  this.addStrData = function (s) {
    var buf = new ArrayBuffer(s.length * 2);
    var view = new Uint16Array(buf);
    var slen = s.length;

    var i; for (i = 0; i < slen; i++) {
      view[i] = s.charCodeAt(i);
    }

    this.addData(new Uint8Array(buf));
  };

  this.allData = function() {
    return this.data;
  };

  this.metaLength = function () {
    var lw = this.info.meta.getLengthWidth();
    switch(lw) {
      case 1: return bytes1ToInt(this.data, 0);
      case 2: return bytes2ToInt(this.data, 0);
      case 4: return bytes4ToInt(this.data, 0);
      case 8: throw new Error("Can't unpack 64 bit length tags in JavaScript.");
      default:
        throw new Error("Invalid meta length width: " + lw.toString());
    }
  };

  this.metaType = function () {
    var lw = this.info.meta.getLengthWidth();
    var tw = this.info.meta.getTypeWidth();
    var t = [];
    var i; for (i = 0; i < tw; i++) {
      t.push(this.data[lw + i]);
    }

    return t;
  };

  this.decodeMeta = function() {
    var ml = this.metaLength();
    var mt = this.metaType();
    var offset = this.info.meta.getPayloadOffset();

    if (ml + offset > this.data.length) {
      throw new Error("Not enough data in buffer: " + this.data.length);
    }

    var ty = this.info.types.withTag(mt);
    if (undefined === ty) {
      throw new Error("Invalid type tag: " + mt.toString());
    }

    var ctor = ty.constructor;
    console.error("CTOR", inst);
    var inst = ctor.unpack(this.data, offset);
    console.error("INST", inst.constructor.name);
    console.error("INST", inst);

    return inst;
  };

  this.encodeMeta = function () {
    return "";
  };
};
