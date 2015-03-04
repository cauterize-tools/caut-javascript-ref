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
  this.types = {
{{#jscTypes}}
    '{{jstDetail.jstName}}': {
      name: '{{jstDetail.jstName}}',
      proto: '{{jstDetail.jstPrototype}}',
      hash: [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}],
      size: { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} },
    },
{{/jscTypes}}
  };
}

exports.CautInfo = function () {
  this.meta = new MetaInfo();
  this.types = new TypeInfo();
};
