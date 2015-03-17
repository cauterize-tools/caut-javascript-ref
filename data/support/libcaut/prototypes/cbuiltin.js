/*global ArrayBuffer,
         Uint8Array, Uint16Array, Uint32Array,
         Int8Array, Int16Array, Int32Array,
         Float32Array, Float64Array */
'use strict';

var ctype = require('./ctype.js');

function CBuiltIn(buffer) {
  if (!(buffer instanceof ArrayBuffer)) {
    throw new Error("Expected ArrayBuffer");
  }

  var w = this.constructor.width;

  this.buffer = new ArrayBuffer(w);
  var thisView = new Uint8Array(this.buffer);
  var thatView = new Uint8Array(buffer);

  var i; for (i = 0; i < w; i++) {
    thisView[i] = thatView[i];
  }
  ctype.CType.call(this, buffer);
}
CBuiltIn.prototype = Object.create(ctype.CType.prototype);

CBuiltIn.prototype.cautproto = 'builtin';
CBuiltIn.prototype.pack = function (cautBuffer) {
  cautBuffer.addU8Array(new Uint8Array(this.buffer));
  return this.buffer.byteLength;
};
CBuiltIn.prototype.toJS = function () {
  return this.fromBytes(this.buffer);
};

CBuiltIn.unpack = function (biCtor, cautBuffer) {
  var width = biCtor.width;
  var ab = new Uint8Array(width);

  var i; for (i = 0; i < width; i++) {
    ab[i] = cautBuffer.nextE();
  }

  return new biCtor(ab.buffer);
};

Object.freeze(CBuiltIn);
exports.CBuiltIn = CBuiltIn;

function u8ToJs(ab)  { return new Uint8Array(ab)[0]; }
function u16ToJs(ab) { return new Uint16Array(ab)[0]; }
function u32ToJs(ab) { return new Uint32Array(ab)[0]; }
function u64ToJs(ab) { var u32 = new Uint32Array(ab); return [u32[0], u32[1]]; }
function s8ToJs(ab)  { return new Int8Array(ab)[0]; }
function s16ToJs(ab) { return new Int16Array(ab)[0]; }
function s32ToJs(ab) { return new Int32Array(ab)[0]; }
function s64ToJs(ab) { var u32 = new Uint32Array(ab); return [u32[0], u32[1]]; }
function f32ToJs(ab) { return new Float32Array(ab)[0]; }
function f64ToJs(ab) { return new Float64Array(ab)[0]; }
function cu8ToJs(ab)  { return new Uint8Array(ab)[0]; }
function cu16ToJs(ab) { return new Uint16Array(ab)[0]; }
function cu32ToJs(ab) { return new Uint32Array(ab)[0]; }
function boolToJs(ab) {
  var u8 = new Uint8Array(ab)[0];
  if (u8 === 0) { return false; }
  if (u8 === 1) { return true; }
  throw new Error("Unexpected value for bool: " + u8.toString());
}

/* Extend the CBuiltIn class */
function ebi(f, width, toJS) {
  f.prototype = Object.create(CBuiltIn.prototype);
  f.prototype.constructor = f;
  f.prototype.fromBytes = toJS;

  f.unpack = function (cautBuffer) {
    return CBuiltIn.unpack(f, cautBuffer);
  };

  f.width = width;

  return f;
}

function mkU8(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u8', 'builtin', hash, size), 1, u8ToJs);
}
exports.mkU8 = mkU8;

function mkU16(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u16', 'builtin', hash, size), 2, u16ToJs);
}
exports.mkU16 = mkU16;

function mkU32(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u32', 'builtin', hash, size), 4, u32ToJs);
}
exports.mkU32 = mkU32;

function mkU64(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u64', 'builtin', hash, size), 8, u64ToJs);
}
exports.mkU64 = mkU64;

function mkS8(f, hash, size) {
  return ebi(ctype.mkCType(f, 's8', 'builtin', hash, size), 1, s8ToJs);
}
exports.mkS8 = mkS8;

function mkS16(f, hash, size) {
  return ebi(ctype.mkCType(f, 's16', 'builtin', hash, size), 2, s16ToJs);
}
exports.mkS16 = mkS16;

function mkS32(f, hash, size) {
  return ebi(ctype.mkCType(f, 'S32', 'builtin', hash, size), 4, s32ToJs);
}
exports.mkS32 = mkS32;

function mkS64(f, hash, size) {
  return ebi(ctype.mkCType(f, 's64', 'builtin', hash, size), 8, s64ToJs);
}
exports.mkS64 = mkS64;

function mkCu8(f, hash, size) {
  return ebi(ctype.mkCType(f, 'cu8', 'builtin', hash, size), 1, cu8ToJs);
}
exports.mkCu8 = mkCu8;

function mkCu16(f, hash, size) {
  return ebi(ctype.mkCType(f, 'cu16', 'builtin', hash, size), 2, cu16ToJs);
}
exports.mkCu16 = mkCu16;

function mkCu32(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u32', 'builtin', hash, size), 4, cu32ToJs);
}
exports.mkCu32 = mkCu32;

function mkF32(f, hash, size) {
  return ebi(ctype.mkCType(f, 'f32', 'builtin', hash, size), 4, f32ToJs);
}
exports.mkF32 = mkF32;

function mkF64(f, hash, size) {
  return ebi(ctype.mkCType(f, 'f64', 'builtin', hash, size), 8, f64ToJs);
}
exports.mkF64 = mkF64;

function mkBool(f, hash, size) {
  return ebi(ctype.mkCType(f, 'bool', 'builtin', hash, size), 1, boolToJs);
}
exports.mkBool = mkBool;
