[
  (comment)
  (extraneous)
  (bad_terminator)
  (gibberish)
  ","
] (#ignore)

(binary_punct "||") (#new "CoreOperation::Or")
(binary_punct "&&") (#new "CoreOperation::And")
(binary_punct "==") (#new "CoreOperation::Eq")
(binary_punct "!=") (#new "CoreOperation::Neq")
(binary_punct ">") (#new "CoreOperation::Greater")
(binary_punct "<") (#new "CoreOperation::Lesser")
(binary_punct ">=") (#new "CoreOperation::GreaterOrEqual")
(binary_punct "<=") (#new "CoreOperation::LesserOrEqual")
(binary_punct "|'") (#new "CoreOperation::BitOr")
(binary_punct "&'") (#new "CoreOperation::BitAnd")
(binary_punct "<<") (#new "CoreOperation::LeftShift")
(binary_punct ">>") (#new "CoreOperation::RightShift")
(binary_punct "+") (#new "CoreOperation::Add")
(binary_punct "-") (#new "CoreOperation::Sub")
(binary_punct "*") (#new "CoreOperation::Mul")
(binary_punct "/") (#new "CoreOperation::Div")
(binary_punct "%") (#new "CoreOperation::Mod")
(binary_punct "^") (#new "CoreOperation::Pow")

(lit_int) @lit (#reparse @lit "i64") (#new "PrimtiveValue::LitInt" "0" @lit)
(lit_string) @lit (#reparse @lit "String") (#new "PrimitiveValue::LitString" "0" @lit)
(lit_float) @lit (#reparse @lit "f64") (#new "PrimitiveValue::LitFloat" "0") @lit

[
  [(lit_int) (lit_string) (lit_float)] (#map "Expression::PrimitiveValue" "0")
  [(binary_op) (unary_op)] (#map "Expression::Operation" "0")
  (invocation)
] (#new )

(identifier) (#literal)

(reference . (identifier) @head (identifier)* @tail) (#new @head @tail)

(invocation . (reference) @instance (_)* @args) (#manual "Expression" @instance @args)

(binary_op
  lhs: (_) @lhs
  (binary_punct) @punct
  rhs: (_) @rhs
) (#new "Operation" "operands[0" @lhs "operands[1" @rhs "operator" @punct)

(unary_op [
  (unary_punct "-") (#new "CoreOperation::Neg")
  (unary_punct "!") (#new "CoreOperation::Not")
] @op value: (_) @value) (#new "Operation" "operands[0" @value "operation" @op)

(let_assignment (reference) @variable value: (_) @value) (#manual @var @value)
(set_assignment (reference) @variable value: (_) @value) (#new "Set" @variable @value)


LIT_STR = (lit_string) @lit (#get-text @lit)
LIT_INT = (lit_int) @lit (#get-text @lit) (#parse "s64")
LIT_FLOAT = (lit_float) @lit (#get-text) @lit (#parse "f64")
LITERAL = { LIT_STR | LIT_INT | LIT_FLOAT }

IDENT = (identifier) @ident (#get-text @ident)

EXPRESSION =

sum UNARY_PUNCT { NEG | NOT } = [
  (unary_punct "-") @NEG
  (unary_punct "!") @NOT
]

sum BINARY_PUNCT { OR | AND | EQ | NEQ | GREATER | LESSER | GREATER_OR_EQ | LESSER_OR_EQ
  | BIT_OR | BIT_AND | SHL | SHR | ADD | SUB | MUL | DIV | MOD | POW } = [
  (binary_punct "||") (#new "CoreOperation::Or")
  (binary_punct "&&") (#new "CoreOperation::And")
  (binary_punct "==") (#new "CoreOperation::Eq")
  (binary_punct "!=") (#new "CoreOperation::Neq")
  (binary_punct ">") (#new "CoreOperation::Greater")
  (binary_punct "<") (#new "CoreOperation::Lesser")
  (binary_punct ">=") (#new "CoreOperation::GreaterOrEqual")
  (binary_punct "<=") (#new "CoreOperation::LesserOrEqual")
  (binary_punct "|'") (#new "CoreOperation::BitOr")
  (binary_punct "&'") (#new "CoreOperation::BitAnd")
  (binary_punct "<<") (#new "CoreOperation::LeftShift")
  (binary_punct ">>") (#new "CoreOperation::RightShift")
  (binary_punct "+") (#new "CoreOperation::Add")
  (binary_punct "-") (#new "CoreOperation::Sub")
  (binary_punct "*") (#new "CoreOperation::Mul")
  (binary_punct "/") (#new "CoreOperation::Div")
  (binary_punct "%") (#new "CoreOperation::Mod")
  (binary_punct "^") (#new "CoreOperation::Pow")
]

sum CORE_OPERATION = flatten { UNARY_PUNCT | BINARY_PUNCT }

sum OPERATION { CORE_OPERATION |  }

product OPERATION { OPERATOR | OPERANDS } = [
  (unary_op (UNARY_PUNCT) @OPERATOR ) 
]