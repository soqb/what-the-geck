(comment) @comment.line.semicolon
(lit_string) @string.quoted.double
[(lit_int) (lit_float)] @constant.numeric
(scriptname (identifier) @entity.name.section)
(event_implementation
  name: (identifier) @function
) @meta.function
(variable_declaration (identifier) @variable)
(identifier) @identifier

[(assignment_punct) (binary_punct) (unary_punct)] @operator
"." @punctuation.delimiter
["(" ")"] @punctuation.bracket

[
  (kw_begin)
  (kw_end)
  (kw_scriptname)
  (kw_set)
  (kw_to)
  (kw_if)
  (kw_elseif)
  (kw_else)
  (kw_endif)
  (kw_let)

  (variable_type)
] @keyword

[
  (extraneous)
  (bad_terminator)
] @error