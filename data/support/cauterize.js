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

function intToBytes1(val, u8array, offset) {
  u8array[offset] = val;
}

function intToBytes2(val, u8array, offset) {
  var buf = Uint16Array(1);
  buf[0] = val;
  var view = new Uint8Array(buf.buffer);

  var i;
  for (i = 0; i < 2; i++) {
    u8array[offset + i] = view[i];
  }
}

function intToBytes4(val, u8array, offset) {
  var buf = Uint32Array(1);
  buf[0] = val;
  var view = new Uint8Array(buf.buffer);

  var i;
  for (i = 0; i < 4; i++) {
    u8array[offset + i] = view[i];
  }
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

    var inst = ty.unpack(this.data, offset);

    return inst;
  };

  this.encodeMeta = function (obj) {
    var i;
    var ctor = obj.constructor;

    var lw = this.info.meta.getLengthWidth();
    var tw = this.info.meta.getTypeWidth();
    var offset = this.info.meta.getPayloadOffset();

    // Create a buffer large enough for our type. Pack it into the buffer.
    var objBuf = new Uint8Array(ctor.size.max);
    var plen = obj.pack(objBuf, 0);

    // Create a new buffer that's just large enough for the header and the
    // payload. Copy the payload at the correct offset into the resized buffer.
    var resizedBuf = new Uint8Array(offset + plen);
    for (i = 0; i < plen; i++) {
      resizedBuf[offset + i] = objBuf[i];
    }

    // Pack the length into the rezied buffer.
    switch(lw) {
      case 1: intToBytes1(plen, resizedBuf, 0); break;
      case 2: intToBytes2(plen, resizedBuf, 0); break;
      case 4: intToBytes4(plen, resizedBuf, 0); break;
      case 8: throw new Error("Can't pack 64 bit length tags in JavaScript.");
      default:
        throw new Error("Invalid meta length width: " + lw.toString());
    }

    // Pack the tag into the resized buffer.
    for (i = 0; i < tw; i++) {
      resizedBuf[lw + i] = ctor.hash[i];
    }

    return resizedBuf;
  };
};
