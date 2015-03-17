/*global ArrayBuffer, Uint8Array, Uint16Array, Uint32Array */

'use strict';

/* Section: Byte Casting
 *
 * Tools to cast between integers and sequences of bytes.
 *
 * Exported functions:
 *
 *    - bytesToInt
 *    - intToBytes
 *
 * Private functions:
 *
 *    - viewOfSize
 *    - someBytes
 */

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

function someBytes(cBuf, sz) {
  var i, bytes = [];

  for (i = 0; i < sz; i++) {
    bytes.push(cBuf.nextE());
  }

  return bytes;
}
exports.someBytes = someBytes;

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

function intToBytes(cBuf, val, sz) {
  var view = viewOfSize(sz);

  var buf = new view(1);
  buf[0] = val;
  var bView = new Uint8Array(buf.buffer);
  cBuf.addU8Array(bView);

  return sz;
}
exports.intToBytes = intToBytes;

