/**
 * A buffer class that can be used to collect and iterate over data.
 */
function CautBuffer(initData) {
  var self = this;

  /* Setup the object. */
  function init() {
    if (initData === undefined) {
      self.buffers = [];
    } else {
      self.buffers = [initData];
    }

    self.reset();
  }

  /* Reduce the number of buffers to at most 1. */
  function compact() {
    if (0 >= self.buffers.length) {
      return;
    }

    var pos = self.position();
    var newBuf = new Uint8Array(self.length());

    var i, j, p, b;
    for (i = 0, p = 0; i < self.buffers.length; i++) {
      b = self.buffers[i];
      for (j = 0; j < b.length; j++, p++) {
        newBuf[p] = b[j];
      }
    }

    self.buffers = [newBuf];
    self.bufferIx = 0;
    self.bufferPos = pos;
  }

  /**
   * Reset the iterator status. */
  this.reset = function () {
    this.bufferIx = 0;
    this.bufferPos = 0;
  };

  /**
   * Append a Uint8Array to the buffer. */
  this.addU8Array = function (buffer) {
    this.buffers.push(buffer);
  };

  /**
   * Append a single Uint8 value to the buffer. */
  this.addU8 = function(u8) {
    if (u8 < 0 || 255 < u8) {
      throw new Error("Invalid u8 value: " + u8.toString());
    }
    this.buffers.push(new Uint8Array([u8]));
  };

  /**
   * Append another CautBuf to this one.
   */
  this.append = function(cb) {
    this.addU8Array(cb.allData());
  };

  /**
   * Return the next byte in the iterator. Returns `undefined` when the
   * iterator has exhausted the buffer. */
  this.next = function () {
    if (this.bufferIx >= this.buffers.length) {
      // Out of buffers. This means we're out of data.
      return undefined;
    }

    if (this.bufferPos >= this.buffers[this.bufferIx].length) {
      // Current buffer is out of data. Move to the next index and try again.
      this.bufferIx += 1;
      this.bufferPos = 0;

      return this.next();
    }

    var n = this.buffers[this.bufferIx][this.bufferPos];
    this.bufferPos += 1;

    return n;
  };

  /**
   * Like `next`, but throws an exception if the buffer has no more available
   * bytes to iterate over. */
  this.nextE = function () {
    var n = this.next();

    if (undefined === n) {
      throw new Error("Out of bytes!");
    }

    return n;
  };

  /**
   * Return the overall position in the buffer. */
  this.position = function () {
    var i;
    var pos = 0;

    for (i = 0; i < this.bufferIx; i++) {
      pos += this.buffers[i].length;
    }
    pos += this.bufferPos;

    return pos;
  };

  /**
   * Return the length of the overall buffer. */
  this.length = function () {
    return this.buffers.map(function (v) { return v.length; })
               .reduce(function (a, b) { return a + b; });
  };

  /**
   * Returns how much data remains in the iterator. */
  this.remaining = function () {
    return this.length() - this.position();
  };

  this.allData = function () {
    compact();
    return this.buffers[0];
  };

  init();
}

exports.CautBuffer = CautBuffer;

/* Some built-in test code to be sure things are working as expected. */
if (!module.parent) {
  var b;
  b = new CautBuffer(new Uint8Array([0,1,2]));
  b.addU8Array(new Uint8Array([3,4,5]));
  b.addU8(6);
  b.addU8(7);
  b.addU8Array(new Uint8Array([8,9,10,11,12,13,14,15]));

  var x;
  do {
    x = b.next();
  } while (x !== undefined);

  b.reset();

  do {
    x = b.next();
  } while (x !== undefined);

  console.log(b.allData());
}

