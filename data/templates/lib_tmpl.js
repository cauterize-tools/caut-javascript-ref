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
    '{{jstName}}': {
      name: '{{jstName}}',
      proto: '{{jstPrototype}}',
      hash: [{{#jstHash}}{{.}},{{/jstHash}}],
      size: { min: {{jstSize.jstMinSize}}, max: {{jstSize.jstMaxSize}} },
    },
{{/jscTypes}}
  };
}

exports.CautInfo = function () {
  this.meta = new MetaInfo();
  this.types = new TypeInfo();
};
