// lib{{jscLibName}}.js
var C = require('./cauterize.js');
var BIS = require('./builtin_lib.js');

{{#jscMeta}}
function MetaInfo() {
  this.getLengthWidth = function() {
    return {{jsmLengthWidth}};
  };

  this.getTypeWidth = function () {
    return {{jsmTypeWidth}};
  };

  this.getPayloadOffset = function () {
    return {{jsmLengthWidth}} + {{jsmTypeWidth}};
  };
}
{{/jscMeta}}

{{#jscTypes}}
{{> jst_builtin_cstors_tmpl.js}}
{{> jst_synonym_cstors_tmpl.js}}
{{/jscTypes}}

function TypeInfo() {
  this.types = {
{{#jscTypes}}
    '{{jstDetail.jstName}}': {
      name: '{{jstDetail.jstName}}',
      proto: '{{jstDetail.jstPrototype}}',
      hash: [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}],
      size: { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} },
      constructor: {{jstDetail.jstConstructor}},
    },
{{/jscTypes}}
  };

  this.withTag = function(prefix) {
    function prefixMatches(tag) {
      var i;

      for (i = 0; i < prefix.length; i++) {
        if (prefix[i] !== tag[i]) {
          return false;
        }
      }

      return true;
    };

    var i;
    for (var key in this.types) {
      if (prefixMatches(this.types[key].hash)) {
        return this.types[key];
      }
    }

    return undefined;
  };
}

function packMeta(t) {
}

function unpackMeta(u8buf) {
}

exports.CautInfo = function () {
  this.meta = new MetaInfo();
  this.types = new TypeInfo();
};
