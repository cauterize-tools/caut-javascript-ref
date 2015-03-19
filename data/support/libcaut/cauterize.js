'use strict';

/* Section: Transcoding Manager
 *
 * An object to manage transcoding Cauterize objects.
 *
 * Exported objects
 *
 *    - Cauterize
 */

var dict = require('./typedict.js');
var buffer = require('./buffer.js');
var cast = require('./cast.js');

function Cauterize(specDesc) {
  this.typeDict = new dict.TypeDict(specDesc.types);
  this.metaInfo = specDesc.metaInfo;
}

Cauterize.prototype.decode = function (cautBuffer) {
  var payloadLength = cast.bytesToInt(cautBuffer, this.metaInfo.lengthWidth);

  if (cautBuffer.remaining() < payloadLength) {
    throw new Error("Not enough bytes in buffer: " + payloadLength);
  }

  var payloadTag = cast.someBytes(cautBuffer, this.metaInfo.typeWidth);
  var typeCtor = this.typeDict.typeWithHashPrefixE(payloadTag).ctor;

  return typeCtor.unpack(cautBuffer);
};

Cauterize.prototype.encode = function (typeInstance) {
  var headerBuffer = new buffer.CautBuffer();
  var payloadBuffer = new buffer.CautBuffer();

  typeInstance.pack(payloadBuffer);

  cast.intToBytes(headerBuffer, payloadBuffer.length(), this.metaInfo.lengthWidth);
  var tw = this.metaInfo.typeWidth;
  var i; for (i = 0; i < tw; i++) {
    headerBuffer.addU8(typeInstance.constructor.hash[i]);
  }

  headerBuffer.append(payloadBuffer);

  return headerBuffer;
};

exports.Cauterize = Cauterize;
