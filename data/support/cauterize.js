function bytes1ToInt(u8array, offset) {
  return u8array[offset];
}

function bytes2ToInt(u8array, offset) {
  var sz = 2;
  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var u16view = new Uint16Array(buf);

  for (var i = 0; i < sz; i++) {
    u8view[i] = u8array[offset + i];
  }

  return u16view[0];
}

function bytes4ToInt(u8array, offset) {
  var sz = 4;
  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var u32view = new Uint32Array(buf);

  for (var i = 0; i < sz; i++) {
    u8view[i] = u8array[offset + i];
  }

  return u32view[0];
}

function bytes8ToInt(u8array, offset) {
  var sz = 8;
  var buf = new ArrayBuffer(sz);
  var u8view = new Uint8Array(buf);
  var u64view = new Uint64Array(buf);

  for (var i = 0; i < sz; i++) {
    u8view[i] = u8array[offset + i];
  }

  return u64view[0];
}

exports.DataInterface = function (info) {
  // Store the specification/meta information
  this.info = info;

  this.data = new Uint8Array(0);

  this.addData = function(ab) {
    var tmp = new Uint8Array(this.data.byteLength + ab.byteLength);
    tmp.set(this.data, 0);
    tmp.set(ab, this.data.byteLength);
    this.data = tmp;
  };

  this.addStrData = function (s) {
    var buf = new ArrayBuffer(s.length * 2);
    var view = new Uint16Array(buf);
    var slen = s.length;

    for (var i = 0; i < slen; i++) {
      view[i] = s.charCodeAt(i);
    }

    this.addData(new Uint8Array(buf));
  };

  this.allData = function() {
    return this.data;
  };

  this.metaLength = function () {
    var lw = this.info.meta.getLengthWidth();
    switch(lw) {
      case 1: return bytes1ToInt(this.data, 0); break;
      case 2: return bytes2ToInt(this.data, 0); break;
      case 4: return bytes4ToInt(this.data, 0); break;
      case 8: return bytes8ToInt(this.data, 0); break;
      default:
        throw new Error("Invalid meta length width: " + lw.toString());
    }
  };

  this.metaType = function () {
    var lw = this.info.meta.getLengthWidth();
    var tw = this.info.meta.getTypeWidth();
    var t = [];
    for (var i = 0; i < tw; i++) {
      t.push(this.data[lw + i]);
    };

    return t;
  };

  this.decodeAsType = function (typeName, offset) {
  };

  this.decodePayload = function () {
  };
};