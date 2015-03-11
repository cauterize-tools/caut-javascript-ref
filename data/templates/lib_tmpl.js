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

/* Define all type information. */
{{#jscTypes}}
/* {{jstDetail.jstName}} */
{{> jst_array_cstors_tmpl.js}}
{{> jst_builtin_cstors_tmpl.js}}
{{> jst_combination_cstors_tmpl.js}}
{{> jst_record_cstors_tmpl.js}}
{{> jst_synonym_cstors_tmpl.js}}
{{> jst_union_cstors_tmpl.js}}
{{> jst_vector_cstors_tmpl.js}}
{{jstDetail.jstConstructor}}.name = "{{jstDetail.jstName}}";
{{jstDetail.jstConstructor}}.proto = '{{jstDetail.jstPrototype}}';
{{jstDetail.jstConstructor}}.hash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
{{jstDetail.jstConstructor}}.size = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
Object.freeze({{jstDetail.jstConstructor}});
exports.{{jstDetail.jstConstructor}} = {{jstDetail.jstConstructor}};

{{/jscTypes}}

function TypeInfo() {
  this.types = {
{{#jscTypes}}
    '{{jstDetail.jstName}}': {{jstDetail.jstConstructor}},
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
    for (var tyName in this.types) {
      if (prefixMatches(this.types[tyName].hash)) {
        return this.types[tyName];
      }
    }

    return undefined;
  };
}

exports.CautInfo = function () {
  this.meta = new MetaInfo();
  this.types = new TypeInfo();
};
