'use strict';

var CBuiltIn     = require('./prototypes/cbuiltin.js');
var CSynonym     = require('./prototypes/csynonym.js');
var CArray       = require('./prototypes/carray.js');
var CVector      = require('./prototypes/cvector.js');
var CRecord      = require('./prototypes/crecord.js');
var CCombination = require('./prototypes/ccombination.js');
var CUnion       = require('./prototypes/cunion.js');

exports.CBuiltIn = CBuiltIn.CBuiltIn;
exports.CSynonym = CSynonym.CSynonym;
exports.CArray = CArray.CArray;
exports.CVector = CVector.CVector;
exports.CRecord = CRecord.CRecord;
exports.CCombination = CCombination.CCombination;
exports.CUnion = CUnion.CUnion;

exports.mkU8 = CBuiltIn.mkU8;
exports.mkU16 = CBuiltIn.mkU16;
exports.mkU32 = CBuiltIn.mkU32;
exports.mkU64 = CBuiltIn.mkU64;
exports.mkS8 = CBuiltIn.mkS8;
exports.mkS16 = CBuiltIn.mkS16;
exports.mkS32 = CBuiltIn.mkS32;
exports.mkS64 = CBuiltIn.mkS64;
exports.mkCu8 = CBuiltIn.mkCu8;
exports.mkCu16 = CBuiltIn.mkCu16;
exports.mkCu32 = CBuiltIn.mkCu32;
exports.mkF32 = CBuiltIn.mkF32;
exports.mkF64 = CBuiltIn.mkF64;
exports.mkBool = CBuiltIn.mkBool;

exports.mkArray = CArray.mkArray;
exports.mkCombination = CCombination.mkCombination;
exports.mkRecord = CRecord.mkRecord;
exports.mkSynonym = CSynonym.mkSynonym;
exports.mkUnion = CUnion.mkUnion;
exports.mkVector = CVector.mkVector;
