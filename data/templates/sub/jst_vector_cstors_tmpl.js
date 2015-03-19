{{#JSTVector}}
function {{jstDetail.jstConstructor}}(elems) { prot.CVector.call(this, elems); }
(function () {
  var typeHash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
  var typeSize = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
  prot.mkArray({{jstDetail.jstConstructor}}, '{{jstName}}', {{jstVectorRefCtor}}, {{jstVectorMaxLen}}, {{jstVectorLenWidth}}, typeHash, typeSize);
}());
{{/JSTVector}}
