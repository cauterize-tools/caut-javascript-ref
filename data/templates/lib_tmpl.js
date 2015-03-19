'use strict';

var prot = require('./libcaut/prototypes.js');

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
Object.freeze({{jstDetail.jstConstructor}});
exports.{{jstDetail.jstConstructor}} = {{jstDetail.jstConstructor}};

{{/jscTypes}}

var libTypes = {
{{#jscTypes}}
  '{{jstDetail.jstName}}': {{jstDetail.jstConstructor}},
{{/jscTypes}}
};
Object.freeze(libTypes);

var metaInfo = {
{{#jscMeta}}
  lengthWidth: {{jsmLengthWidth}},
  typeWidth: {{jsmTypeWidth}},
{{/jscMeta}}
};
Object.freeze(metaInfo);

var specInfo = {
  types: libTypes,
  metaInfo: metaInfo,
};
Object.freeze(specInfo);

exports.SpecificationInfo = specInfo;
