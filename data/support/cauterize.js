/*global ArrayBuffer, Uint8Array, Uint16Array, Uint32Array */
'use strict';

var builtin = require('./builtin_lib.js');
var cb = require('./caut_buffer.js');

/* Returns a constructor for a view of the provided size. */
function viewOfSize(sz) {
  var view;

  switch (sz) {
    case 1: view = Uint8Array; break;
    case 2: view = Uint16Array; break;
    case 4: view = Uint32Array; break;
    case 8: throw new Error("Can't unpack 64 bit length tags in JavaScript.");
    default: throw new Error("Invalid meta length width: " + sz.toString());
  }

  return view;
}

/* Converts `sz` bytes from `cBuf` into an unsinged value. */
function bytesToInt(cBuf, sz) {
  var view = viewOfSize(sz);

  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var dataView = new view(buf);

  var i; for (i = 0; i < sz; i++) {
    u8view[i] = cBuf.nextE();
  }

  return dataView[0];
}
exports.bytesToInt = bytesToInt;

/* Converts `val` into `sz` bytes appended to `cBuf`. */
function intToBytes(val, cBuf, sz) {
  var view = viewOfSize(sz);

  var buf = new view(1);
  buf[0] = val;
  var bView = new Uint8Array(buf.buffer);
  cBuf.addU8Array(bView);
}
exports.intToBytes = intToBytes;

exports.DataInterface = function (info, buffer) {
  var self = this;

  function readMetaLength() {
    var lw = self.info.meta.getLengthWidth();
    return bytesToInt(self.buffer, lw);
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
    intToBytes(plen, hBuf, lw);

    // Pack the tag into the resized buffer.
    for (i = 0; i < tw; i++) {
      hBuf.addU8(ctor.hash[i]);
    }

    hBuf.append(oBuf);

    return hBuf.allData();
  };

  init();
};
