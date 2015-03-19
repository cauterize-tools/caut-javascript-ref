{{#JSTBuiltIn}}
function {{jstDetail.jstConstructor}}(buffer) { prot.CBuiltIn.call(this, buffer); }
(function () {
  var typeHash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
  var typeSize = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
  prot.mk{{jstDetail.jstConstructor}}({{jstDetail.jstConstructor}}, typeHash, typeSize);
}());
{{/JSTBuiltIn}}
