/*global ArrayBuffer, Uint8Array, Uint16Array, Uint32Array, Int8Array, Int16Array, Int32Array, Float32Array, Float64Array */
'use strict';

/* Unpack a value described by the `ctor` object from `u8buf` starting at
 * `offset`. */
function unpack(ctor, u8buf, offset) {
  var safeOffset = offset || 0;
  var view = ctor.view;
  var width = ctor.width;

  if (view.BYTES_PER_ELEMENT !== width) {
    throw new Error("View/width mismatch. Width is: " + width + ". " +
                    "View bytes per element is: " + view.BYTES_PER_ELEMENT + ".");
  }

  if (width + offset > u8buf.byteLength) {
    throw new Error("Insufficient bytes: " + u8buf.byteLength.toString());
  }

  var b = new ArrayBuffer(width);
  var u8view = new Uint8Array(b);

  var i; for (i = 0; i < width; i++) {
    u8view[i] = u8buf[safeOffset + i];
  }

  return new ctor(new view(b)[0]);
}

/* Unpack a 64 bit value described by the `ctor` object from `u8buf` starting
 * at `offset`. The 64 bit value will be stored as a pair of 32 bit values. The
 * `view` on the `ctor` object must be one of Uint32Array or Int32Array. */
function unpack64(ctor, u8buf, offset) {
  var safeOffset = offset || 0;
  var view = ctor.view;

  if (8 + safeOffset > u8buf.byteLength) {
    throw new Error("Insufficient bytes: " + u8buf.byteLength.toString());
  }

  if (view.name !== "Uint32Array" && view.name !== "Int32Array") {
    throw new Error("Invalid view: " + view.name);
  }

  var b0 = new ArrayBuffer(4);
  var b1 = new ArrayBuffer(4);
  var u8view_0 = new Uint8Array(b0);
  var u8view_1 = new Uint8Array(b1);

  var i; for (i = 0; i < 4; i++) {
    u8view_0[i] = u8buf[safeOffset + i];
  }

  var j; for (j = 4; j < 8; j++) {
    u8view_1[j - 4] = u8buf[safeOffset + j];
  }

  return new ctor(new view(b0)[0], new view(b1)[0]);
}

function pack(obj, u8buf, offset) {
  var safeOffset = offset || 0;
  var view = obj.constructor.view;
  var width = obj.constructor.width;

  if (width + safeOffset > u8buf.byteLength) {
    throw new Error("Insufficient bytes: " + u8buf.byteLength.toString());
  }

  var ab = new ArrayBuffer(view.BYTES_PER_ELEMENT);
  var v = new view(ab);
  var bv = new Uint8Array(ab);

  v[0] = obj.value;

  var i; for (i = 0; i < width; i++) {
    u8buf[safeOffset + i] = bv[i];
  }

  return width;
}

function pack64(obj, u8buf, offset) {
  var safeOffset = offset || 0;

  if (8 + safeOffset > u8buf.byteLength) {
    throw new Error("Insufficient bytes: " + u8buf.byteLength.toString());
  }

  var ab = new ArrayBuffer(8);
  var v = new Uint32Array(ab);
  var bv = new Uint8Array(ab);

  v[0] = obj.v0;
  v[1] = obj.v1;

  var i; for (i = 0; i < 8; i++) {
    u8buf[safeOffset + i] = bv[i];
  }

  return 8;
}

exports.pack = pack;
exports.pack64 = pack64;
exports.unpack = unpack;
exports.unpack64 = unpack64;
