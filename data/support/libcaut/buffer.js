/*global Uint8Array */

'use strict';

/* Section: Buffering and Byte Iteration
 *
 * An object to collect Uint8Arrays and iterate over the bytes in the
 * concatenated arrays.
 *
 * Exported objects
 *
 *    - CautBuffer
 */
function CautBuffer() {
  this.buffers = [];
  this.reset();
}

/* Reset the iterator status. */
CautBuffer.prototype.reset = function () {
  this.bufferIx = 0;
  this.bufferPos = 0;
};

/* Append a Uint8Array to the buffer. */
CautBuffer.prototype.addU8Array = function (buffer) {
  if (buffer instanceof Uint8Array) {
    this.buffers.push(buffer);
  } else {
    throw new Error("Expected Uint8Array.");
  }
};

/* Append a single Uint8 value to the buffer. */
CautBuffer.prototype.addU8 = function(u8) {
  if (u8 < 0 || 255 < u8) {
    throw new Error("Invalid u8 value: " + u8.toString());
  }
  this.buffers.push(new Uint8Array([u8]));
};

/* Append another CautBuf to this one.  */
CautBuffer.prototype.append = function(cb) {
  this.addU8Array(cb.allData());
};

/* Concatenate all internal buffers into 1. */
CautBuffer.prototype.compact = function () {
  if (1 >= this.buffers.length) {
    return;
  }

  var pos = this.position();
  var newBuf = new Uint8Array(this.length());

  var i, j, p, b;
  for (i = 0, p = 0; i < this.buffers.length; i++) {
    b = this.buffers[i];
    for (j = 0; j < b.length; j++, p++) {
      newBuf[p] = b[j];
    }
  }

  this.buffers = [newBuf];
  this.bufferIx = 0;
  this.bufferPos = pos;
};

/* Return the next byte in the iterator. Throws an exception when the
 * iterator has exhausted the buffer. */
CautBuffer.prototype.nextE = function () {
  if (this.bufferIx < this.buffers.length) {
    if (this.bufferPos < this.buffers[this.bufferIx].length) {
      var n = this.buffers[this.bufferIx][this.bufferPos];
      this.bufferPos += 1;
      return n;
    }

    // Current buffer is out of data. Move to the next index and try again.
    this.bufferIx += 1;
    this.bufferPos = 0;

    return this.nextE();
  }

  throw new Error("Out of bytes!");
};

/* Return the overall position in the buffer. */
CautBuffer.prototype.position = function () {
  var i;
  var pos = 0;

  for (i = 0; i < this.bufferIx; i++) {
    pos += this.buffers[i].length;
  }
  pos += this.bufferPos;

  return pos;
};

/* Return the length of the overall buffer. */
CautBuffer.prototype.length = function () {
  return this.buffers.map(function (v) { return v.length; })
             .reduce(function (a, b) { return a + b; }, 0);
};

/* Returns how much data remains in the iterator. */
CautBuffer.prototype.remaining = function () {
  return this.length() - this.position();
};

/* Return a single Uint8Array with all contained data. */
CautBuffer.prototype.allData = function () {
  this.compact();
  return this.buffers[0];
};

exports.CautBuffer = CautBuffer;
