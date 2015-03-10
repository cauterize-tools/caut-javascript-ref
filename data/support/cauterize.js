/*global ArrayBuffer, Uint8Array, Uint16Array, Uint32Array */
'use strict';

var builtin = require('./builtin_lib.js');
var cb = require('./caut_buffer.js');

function bytes1ToInt(cBuf) {
  return cBuf.nextE();
}

function bytes2ToInt(cBuf) {
  var sz = 2;
  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var u16view = new Uint16Array(buf);

  var i; for (i = 0; i < sz; i++) {
    u8view[i] = cBuf.nextE();
  }

  return u16view[0];
}

function bytes4ToInt(cBuf) {
  var sz = 4;
  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var u32view = new Uint32Array(buf);

  var i; for (i = 0; i < sz; i++) {
    u8view[i] = cBuf.nextE();
  }

  return u32view[0];
}

function intToBytes1(val, cBuf) {
  cBuf.addU8(val);
}

function intToBytes2(val, cBuf) {
  var buf = Uint16Array(1);
  buf[0] = val;
  var view = new Uint8Array(buf.buffer);
  cBuf.addU8Array(view);
}

function intToBytes4(val, cBuf) {
  var buf = Uint32Array(1);
  buf[0] = val;
  var view = new Uint8Array(buf.buffer);
  cBuf.addU8Array(view);
}

exports.DataInterface = function (info, buffer) {
  var self = this;

  function readMetaLength() {
    var lw = self.info.meta.getLengthWidth();
    switch(lw) {
      case 1: return bytes1ToInt(self.buffer);
      case 2: return bytes2ToInt(self.buffer);
      case 4: return bytes4ToInt(self.buffer);
      case 8: throw new Error("Can't unpack 64 bit length tags in JavaScript.");
      default:
        throw new Error("Invalid meta length width: " + lw.toString());
    }
  }

  function readMetaType() {
    var tw = self.info.meta.getTypeWidth();
    var t = [];
    var i; for (i = 0; i < tw; i++) {
      t.push(self.buffer.nextE());
    }

    return t;
  }

  function init() {
    self.info = info;
    self.buffer = buffer;

    self.metaLength = readMetaLength();
    self.metaType = readMetaType();
  }

  this.decodeMeta = function() {
    var rem = this.buffer.remaining();
    if (this.metaLength > rem) {
      throw new Error("Not enough data in buffer: " + rem);
    }

    var ty = this.info.types.withTag(this.metaType);
    if (undefined === ty) {
      throw new Error("Invalid type tag: " + this.metaType.toString());
    }

    var inst = ty.unpack(this.buffer);

    return inst;
  };

  this.encodeMeta = function (obj) {
    var i;
    var ctor = obj.constructor;

    var lw = this.info.meta.getLengthWidth();
    var tw = this.info.meta.getTypeWidth();

    // Pack the object into a CautBuffer.
    var oBuf = new cb.CautBuffer();
    var plen = obj.pack(oBuf);

    // Pack the meta header into a CautBuffer.
    var hBuf = new cb.CautBuffer();
    switch(lw) {
      case 1: intToBytes1(plen, hBuf); break;
      case 2: intToBytes2(plen, hBuf); break;
      case 4: intToBytes4(plen, hBuf); break;
      case 8: throw new Error("Can't pack 64 bit length tags in JavaScript.");
      default:
        throw new Error("Invalid meta length width: " + lw.toString());
    }

    // Pack the tag into the resized buffer.
    for (i = 0; i < tw; i++) {
      hBuf.addU8(ctor.hash[i]);
    }

    hBuf.append(oBuf);

    return hBuf.allData();
  };

  init();
};
