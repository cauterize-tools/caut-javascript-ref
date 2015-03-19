{{#JSTArray}}
function {{jstDetail.jstConstructor}}(elems) { prot.CArray.call(this, elems); }
(function () {
  var typeHash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
  var typeSize = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
  prot.mkArray({{jstDetail.jstConstructor}}, '{{jstName}}', {{jstArrayRefCtor}}, {{jstArrayLen}}, typeHash, typeSize);
}());
{{/JSTArray}}
