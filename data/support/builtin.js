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

  if (width > u8buf.byteLength) {
    throw new Error("Insufficient bytes: " + u8buf.byteLength.toString());
  }

  var b = new ArrayBuffer(width);
  var u8view = new Uint8Array(b);

  for (var i = 0; i < width; i++) {
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

  if (view.name != "Uint32Array" && view.name != "Int32Array") {
    throw new Error("Invalid view: " + view.name);
  }

  var b0 = new ArrayBuffer(4);
  var b1 = new ArrayBuffer(4);
  var u8view_0 = new Uint8Array(b0);
  var u8view_1 = new Uint8Array(b1);

  for (var i = 0; i < 4; i++) {
    u8view_0[i] = u8buf[safeOffset + i];
  }

  for (var i = 4; i < 8; i++) {
    u8view_1[i - 4] = u8buf[safeOffset + i];
  }

  return new ctor(new view(b0)[0], new view(b1)[0]);
};

function Bool(v) { this.value = v; };
Bool.width = function () { return 1; }
Bool.view = function () { return Uint8Array; }
Bool.mk = function (u8buf, offset) {
  var b = unpack(U8, u8buf, offset);
  if (b.value != 0 && b.value != 1) {
    throw new Error("Illegal value for boolean: " + b.value);
  } else {
    return new Bool(b.value);
  }
};
Object.freeze(Bool);
exports.Bool = Bool;

function U8(v) { this.value = v; };
U8.width = 1;
U8.view = Uint8Array;
U8.mk = function (u8buf, offset) { return unpack(U8, u8buf, offset); };
Object.freeze(U8);
exports.U8 = U8;

function U16(v) { this.value = v; };
U16.width = 2;
U16.view = Uint16Array;
U16.mk = function (u8buf, offset) { return unpack(U16, u8buf, offset); };
Object.freeze(U16);
exports.U16 = U16;

function U32(v) { this.value = v; };
U32.width = 4;
U32.view = Uint32Array;
U32.mk = function (u8buf, offset) { return unpack(U32, u8buf, offset); };
Object.freeze(U32)
exports.U32 = U32;

function U64(v0, v1) { this.v0 = v0; this.v1 = v1; };
U64.width = 8;
U64.view = Uint32Array;
U64.mk = function (u8buf, offset) { return unpack64(U64, u8buf, offset); };
Object.freeze(U64)
exports.U64 = U64;

function S8(v) { this.value = v; };
S8.width = 1;
S8.view = Int8Array;
S8.mk = function (u8buf, offset) { return unpack(S8, u8buf, offset); };
Object.freeze(S8)
exports.S8 = S8;

function S16(v) { this.value = v; };
S16.width = 2;
S16.view = Int16Array;
S16.mk = function (u8buf, offset) { return unpack(S16, u8buf, offset); };
Object.freeze(S16)
exports.S16 = S16;

function S32(v) { this.value = v; };
S32.width = 4;
S32.view = Int32Array;
S32.mk = function (u8buf, offset) { return unpack(S32, u8buf, offset); };
Object.freeze(S32)
exports.S32 = S32;

function S64(v0, v1) { this.v0 = v0; this.v1 = v1; };
S64.width = 8;
S64.view = Uint32Array;
S64.mk = function (u8buf, offset) { return unpack64(S64, u8buf, offset); };
Object.freeze(S64)
exports.S64 = S64

function F32(v) { this.value = v; };
F32.width = 4;
F32.view = Float32Array;
F32.mk = function (u8buf, offset) { return unpack(F32, u8buf, offset); };
Object.freeze(F32)
exports.F32 = F32;

function F64(v) { this.value = v; };
F64.width = 8;
F64.view = Float64Array;
F64.mk = function (u8buf, offset) { return unpack(F64, u8buf, offset); };
Object.freeze(F64)
exports.F64 = F64;

function CU8(v) { this.value = v; };
CU8.width = 1;
CU8.view = Uint8Array;
CU8.mk = function (u8buf, offset) { return unpack(CU8, u8buf, offset); };
Object.freeze(CU8)
exports.CU8 = CU8;

function CU16(v) { this.value = v; };
CU16.width = 2;
CU16.view = Uint16Array;
CU16.mk = function (u8buf, offset) { return unpack(CU16, u8buf, offset); };
Object.freeze(CU16)
exports.CU16 = CU16;

function CU32(v) { this.value = v; };
CU32.width = 4;
CU32.view = Uint32Array;
CU32.mk = function (u8buf, offset) { return unpack(CU32, u8buf, offset); };
Object.freeze(CU32)
exports.CU32 = CU32;
