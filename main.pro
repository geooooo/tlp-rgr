parse(LEXEMS, TREE) :-
    phrase(start(TREE), LEXEMS),
    !.

build(TREE) :-
    parse([
      % "inc", '::', 'Int', '->', 'Int', ';',
      % "is_even", '::', 'Int', '->', 'Bool', ';',
      % "printi", '::', '[', 'Int', ']', ';',
      % "unpack", '::', '(', 'Int', ',', '[', 'Bool', ']', ')', '->', 'Int', '->', 'Bool'
      %"const", '=', 0, ';',
      %"inc", "x", '=', "x", '+', 1, ';',
      %"foo", "x", "y", '=', 'let', "g", '=', 'let', "h", '=', 1, 'in', "h", 'in', "g"
      %"max", "x", "y", '=', 'if', "x", '>', "y", 'then', "x", 'else', "y"
      %"foo", "x", "y", '=', 'let', "max", "x", "y", '=', 'if', "x", '>', "y", 'then', "x", 'else', "y",
      % 	'in', "max", "x", "y", 'in', "g"
    ], TREE).

start(defines(X)) --> defines(X).

defines([X|Y]) --> gdefine(X), [';'], defines(Y).
defines(X) --> gdefine(X).

gdefine(define_type(X, Y)) -->  define_type(X, Y).
gdefine(define_func(X, Y, Z)) --> define_func(X, Y, Z).

define_type(X, types(Y)) --> id(X), ['::'], asequence_type(Y).

id(id(X)) --> [X], {string(X)}.

asequence_type(X) --> type(X).
asequence_type([X|Y]) --> type(X), ['->'], asequence_type(Y).

type(type_scalar(X)) --> type_scalar(X).
type(type_struct(X)) --> type_struct(X).

type_scalar(X) --> [X], {member(X, ['Int', 'Bool'])}.

type_struct(X) --> type_list(X).
type_struct(X) --> type_tuple(X).

type_list(list(X)) --> ['['], type_scalar(X), [']'].

type_tuple(tuple(X)) --> ['('], csequence_type(X), [')'].

csequence_type(X) --> type(X).
csequence_type([X|Y]) --> type(X), [','], csequence_type(Y).

fdefine(let(X,Y,Z,H)) --> block_let(X, Y, Z,H).
fdefine(if(X,Y,Z)) --> block_if(X, Y, Z).
fdefine(X) --> expr(X).

define_func(X, arguments(Y), body(Z)) --> id(X), sequence_varg(Y), ['='], fdefine(Z).

sequence_varg([]) --> [].
sequence_varg(X) --> id(X).
sequence_varg([X|Y]) --> id(X), sequence_varg(Y).

block_let(X,Y,Z,H) --> ['let'], define_func(X, Y, Z), ['in'], block_in(H).
block_in(in(Z)) --> expr(Z).

block_if(cond(X), then(Y), else(Z)) --> ['if'], expr_bool(X), ['then'], expr(Y), ['else'], expr(Z).

expr_bool(X) --> cmp(X).
expr_bool(and(X,Y)) --> cmp(X), ['&&'], expr_bool(Y).
expr_bool(or(X,Y)) --> cmp(X), ['||'], expr_bool(Y).

cmp(eq(X,Y)) --> expr(X), ['='], expr(Y).
cmp(neq(X,Y)) --> expr(X), ['/='], expr(Y).
cmp(lt(X,Y)) --> expr(X), ['<'], expr(Y).
cmp(gt(X,Y)) --> expr(X), ['>'], expr(Y).

expr(X) --> operand(X).
expr(mul(X,Y)) --> operand(X), ['*'], expr(Y).
expr(div(X,Y)) --> operand(X), ['/'], expr(Y).
expr(add(X,Y)) --> operand(X), ['+'], expr(Y).
expr(sub(X,Y)) --> operand(X), ['-'], expr(Y).

operand(int(X)) --> [X], {number(X)}.
operand(X) --> id(X).
operand(X) --> func_call(X).

func_call(call(X,arguments(Y))) --> id(X), sequence_arg(Y).

sequence_arg([]) --> [].
sequence_arg(X) --> operand(X).
sequence_arg([X|Y]) --> operand(X), sequence_arg(Y).
