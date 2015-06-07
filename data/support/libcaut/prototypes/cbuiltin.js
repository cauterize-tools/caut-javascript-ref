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

Object.freeze(CBuiltIn);
exports.CBuiltIn = CBuiltIn;

function unpack(biCtor, cautBuffer) {
  var width = biCtor.width;
  var ab = new Uint8Array(width);

  var i; for (i = 0; i < width; i++) {
    ab[i] = cautBuffer.nextE();
  }

  return new biCtor(ab.buffer);
}

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
function boolToJs(ab) {
  var u8 = new Uint8Array(ab)[0];
  if (u8 === 0) { return false; }
  if (u8 === 1) { return true; }
  throw new Error("Unexpected value for bool: " + u8.toString());
}

function jsToCaut(val, width, viewCtor) {
  var buffer = new ArrayBuffer(width);
  var view = new viewCtor(buffer);
  view[0] = val;
  return buffer;
}

function jsToU8(val)   { return jsToCaut(val, 1, Uint8Array); }
function jsToU16(val)  { return jsToCaut(val, 2, Uint16Array); }
function jsToU32(val)  { return jsToCaut(val, 4, Uint32Array); }
function jsToU64()     { throw new Error("Cannot express 64 bit types in JavaScript. :("); }
function jsToS8(val)   { return jsToCaut(val, 1, Int8Array); }
function jsToS16(val)  { return jsToCaut(val, 2, Int16Array); }
function jsToS32(val)  { return jsToCaut(val, 4, Int32Array); }
function jsToS64()     { throw new Error("Cannot express 64 bit types in JavaScript. :("); }
function jsToF32(val)  { return jsToCaut(val, 4, Float32Array); }
function jsToF64(val)  { return jsToCaut(val, 8, Float64Array); }
function jsToBool(val) {
  if (val === true) { return jsToCaut(1, 1, Uint8Array); }
  if (val === false) { return jsToCaut(0, 1, Uint8Array); }
  throw new Error("Bad value for Bool: " + val);
}

/* Extend the CBuiltIn class */
function ebi(f, width, toJS, fromJS) {
  f.prototype = Object.create(CBuiltIn.prototype);
  f.prototype.constructor = f;
  f.prototype.fromBytes = toJS;

  f.unpack = function (cautBuffer) {
    return unpack(f, cautBuffer);
  };

  f.width = width;

  f.fromJS = function (val) {
    var b = fromJS(val);
    return new f(b);
  };

  return f;
}

function mkU8(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u8', 'builtin', hash, size), 1, u8ToJs, jsToU8);
}
exports.mkU8 = mkU8;

function mkU16(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u16', 'builtin', hash, size), 2, u16ToJs, jsToU16);
}
exports.mkU16 = mkU16;

function mkU32(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u32', 'builtin', hash, size), 4, u32ToJs, jsToU32);
}
exports.mkU32 = mkU32;

function mkU64(f, hash, size) {
  return ebi(ctype.mkCType(f, 'u64', 'builtin', hash, size), 8, u64ToJs, jsToU64);
}
exports.mkU64 = mkU64;

function mkS8(f, hash, size) {
  return ebi(ctype.mkCType(f, 's8', 'builtin', hash, size), 1, s8ToJs, jsToS8);
}
exports.mkS8 = mkS8;

function mkS16(f, hash, size) {
  return ebi(ctype.mkCType(f, 's16', 'builtin', hash, size), 2, s16ToJs, jsToS16);
}
exports.mkS16 = mkS16;

function mkS32(f, hash, size) {
  return ebi(ctype.mkCType(f, 'S32', 'builtin', hash, size), 4, s32ToJs, jsToS32);
}
exports.mkS32 = mkS32;

function mkS64(f, hash, size) {
  return ebi(ctype.mkCType(f, 's64', 'builtin', hash, size), 8, s64ToJs, jsToS64);
}
exports.mkS64 = mkS64;

function mkF32(f, hash, size) {
  return ebi(ctype.mkCType(f, 'f32', 'builtin', hash, size), 4, f32ToJs, jsToF32);
}
exports.mkF32 = mkF32;

function mkF64(f, hash, size) {
  return ebi(ctype.mkCType(f, 'f64', 'builtin', hash, size), 8, f64ToJs, jsToF64);
}
exports.mkF64 = mkF64;

function mkBool(f, hash, size) {
  return ebi(ctype.mkCType(f, 'bool', 'builtin', hash, size), 1, boolToJs, jsToBool);
}
exports.mkBool = mkBool;
