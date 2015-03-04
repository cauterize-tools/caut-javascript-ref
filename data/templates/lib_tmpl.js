// lib{{jscLibName}}.js
var md = require('./meta_decoder.js')

{{#jscMeta}}
function MetaInfo() {
  this.getLengthWidth = function() {
    return {{jsmLengthWidth}};
  };

  this.getTypeWidth = function () {
    return {{jsmTypeWidth}};
  };
}
{{/jscMeta}}

function TypeInfo() {
}

exports.CautInfo = function () {
  this.meta = new MetaInfo();
  this.types = new TypeInfo();
};
