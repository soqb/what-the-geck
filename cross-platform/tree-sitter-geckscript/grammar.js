const kw = ($, lit) => token(prec(1, new RegExp(lit, 'i')));
const kw_def = (lit) => ({
    [`kw_${lit}`]: $ => kw($, lit),
});

const binary_op = ($, token, precedence) => prec.left(precedence, seq(
    field('lhs', $._expression), alias(token, $.binary_punct), field('right', $._expression),
));

const block_def = ($, bad_terminators) => seq($.nlf, repeat(seq(choice($._statement, alias(choice(...bad_terminators), $.bad_terminator)), $.nlf)));

// workaround because aliasing within `block_def` causes tree-sitter to parse differently.
const block = ($, name) => alias($[`_${name}_block`], $.block);

module.exports = grammar({
    name: 'geckscript',

    extras: $ => [/[\r\t\f\v ]+/, $.gibberish],
    inline: $ => [$.nl, $.nlf],

    word: $ => $.kw_end,

    rules: {
        source_file: $ => seq(optional($.nl), $.scriptname, repeat($._item)),

        gibberish: $ => /===+|---+|`+/,
        
        identifier: $ => /[0-9]?[a-zA-Z_][a-zA-Z0-9_]*/,

        comment: $ => /;.*/,
        
        nl: $ => seq(optional($.comment), repeat1(seq('\n', optional($.comment)))),
        
        nlf: $ => seq(alias(optional(token(prec(-1, /[^\n;]+?/))), $.extraneous), $.nl),

        ...kw_def('begin'),
        ...kw_def('end'),
        ...kw_def('set'),
        ...kw_def('to'),
        ...kw_def('if'),
        ...kw_def('elseif'),
        ...kw_def('else'),
        ...kw_def('endif'),
        ...kw_def('let'),
        kw_scriptname: $ => choice(kw($, 'scn'), kw($, 'scriptname')),
        variable_type: $ => choice(kw($, 'int'), kw($, 'short'), kw($, 'long'), kw($, 'float'), kw($, 'ref')),
        
        scriptname: $ => seq($.kw_scriptname, $.identifier, $.nlf),

        _item: $ => seq(choice($.event_implementation, $.variable_declaration, alias($.kw_end, $.bad_terminator)), $.nlf),

        variable_declaration: $ => seq($.variable_type, repeat1($.identifier)),

        lit_int: $ => /[0-9]+/,
        lit_string: $ => /"[^"]"/,
        lit_float: $ =>  /[0-9]+\.[0-9]+/,
        _literal: $ => choice($.lit_int, $.lit_string, $.lit_float),

        event_argument: $ => choice($._literal, $.identifier),

        _event_header: $ => seq($.kw_begin, field('name', $.identifier), optional($.event_argument)),

        _end_block: $ => block_def($, [$.kw_elseif, $.kw_endif, $.kw_endif]),
        event_implementation: $ => seq($._event_header, block($, 'end'), $.kw_end),

        _statement: $ => choice($.variable_declaration, $.set_assignment, $.if_statement, $.let_assignment, $._expression),

        invocation: $ => prec.left(seq($.reference, repeat(seq(optional(','), $.command_argument)))),

        command_argument: $ => choice($._literal, $.reference, $.group, $.unary_op),

        reference: $ => seq($.identifier, repeat(seq('.', $.identifier))),

        binary_op: $ => choice(
            binary_op($, '||', 1),
            binary_op($, '&&', 2),
            binary_op($, '==', 4),
            binary_op($, '!=', 4),
            binary_op($, '>',  5),
            binary_op($, '<',  5),
            binary_op($, '>=', 5),
            binary_op($, '<=', 5),
            binary_op($, '|',  6),
            binary_op($, '&',  7),
            binary_op($, '<<', 8),
            binary_op($, '>>', 8),
            binary_op($, '+',  9),
            binary_op($, '-',  9),
            binary_op($, '*',  10),
            binary_op($, '/',  10),
            binary_op($, '%',  10),
            binary_op($, '^',  11),
        ),
        
        group: $ => seq('(', $._expression, ')'),

        unary_punct: $ => choice('-', '!', '$', '#', '*', '&'),
        unary_op: $ => seq($.unary_punct, field('value', $._expression)),
        
        _expression: $ => choice($.group, $.unary_op, $.binary_op, $._literal, $.invocation),

        set_assignment: $ => seq($.kw_set, $.reference, $.kw_to, field('value', $._expression)),

        assignment_punct: $ => choice(':=', '+=', '-=', '*=', '/=', '^=', '|=', '&=', '%='),
        let_assignment: $ => seq($.kw_let, $.reference, $.assignment_punct, field('value', $._expression)),


        _if_block: $ => block_def($, [$.kw_end]),
        _elseif: $ => seq($.kw_elseif, field('condition', $._expression), block($, 'if')),
        
        _else_block: $ => block_def($, [$.kw_end, $.kw_elseif, $.kw_else]),
        _else: $ => seq($.kw_else, block($, 'else')),

        if_statement: $ => seq($.kw_if, field('condition', $._expression), block($, 'if'), repeat($._elseif), optional($._else), $.kw_endif),
    }
})