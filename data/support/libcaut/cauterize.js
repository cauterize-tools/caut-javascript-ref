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
  var payloadLength = cast.bytesToInt(cautBuffer, this.metaInfo.getLengthWidth());

  if (cautBuffer.remaining() < payloadLength) {
    throw new Error("Not enough bytes in buffer: " + payloadLength);
  }

  var payloadTag = cast.bytesToInt(cautBuffer, this.metaInfo.getTypeWidth());
  var typeCtor = this.typeDict.typeWithHashPrefixE(payloadTag).ctor;

  return typeCtor.decode(cautBuffer);
};

Cauterize.prototype.encode = function (typeInstance) {
  var headerBuffer = new buffer.CautBuffer();
  var payloadBuffer = new buffer.CautBuffer();

  typeInstance.encode(payloadBuffer);

  cast.intToBytes(headerBuffer, payloadBuffer.length(), this.metaInfo.getLengthWidth());
  var tw = this.metaInfo.getTypeWidth();
  var i; for (i = 0; i < tw; i++) {
    headerBuffer.addU8(typeInstance.hash[i]);
  }

  headerBuffer.append(payloadBuffer);

  return headerBuffer;
};
