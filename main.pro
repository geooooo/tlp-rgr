sub_start([], _List):-!.
sub_start([Head|TailSub], [Head|TailList]):-
   sub_start(TailSub, TailList).

sublist(Sub, List):-
   sub_start(Sub, List),!.
sublist(Sub, [_Head|Tail]):-
   sublist(Sub, Tail).

parse(LEXEMS, TREE) :-
    phrase(start(TREE), LEXEMS),
    !.

build(TREE) :-
    parse([
        % 'sum', '::',  'Int', '->', 'Int', '->', 'Int'
        % 'sum', '::', 'Bool', '->', '(', 'Int', ',', '[', 'Int', ']', ')'
        'sum', 'x', 'y', '=', 'let', 'g', '=', 'x', 'in', 'y'
    ], TREE).
    % build(T).

% START -> DEFINES
start(defines(X)) --> defines(X).
% DEFINES -> GDEFINE DEFINES
%          | e
defines([X|Y]) --> gdefine(X), defines(Y).
defines([]) --> [].
% GDEFINE -> DEFINE_TYPE
% 	       | DEFINE_FUNC
gdefine(define_type(X)) --> define_type(X).
gdefine(define_func([X|Y])) --> define_func(X, Y).
% DEFINE_TYPE -> ID '::' ASEQUENCE_TYPE
define_type([X|type(Y)]) --> id(X), ['::'], asequence_type(Y).
% ID -> 'id'
id(id(X)) --> [X], {atomic(X)}.
% ASEQUENCE_TYPE -> TYPE
%                 | TYPE '->' ASEQUENCE_TYPE
asequence_type(X) --> type(X).
asequence_type([X|Y]) --> type(X), ['->'], asequence_type(Y).
% TYPE -> TYPE_SCALAR
%       | TYPE_STRUCT
type(X) --> type_scalar(X).
type(X) --> type_struct(X).
% TYPE_SCALAR -> 'Int'
% 	           | 'Bool'
type_scalar(X) --> [X], {member(X, ['Int', 'Bool'])}.
% TYPE_STRUCT -> TYPE_LIST
% 	           | TYPE_TUPLE
type_struct(X) --> type_list(X).
type_struct(X) --> type_tuple(X).
% TYPE_LIST -> '[' TYPE_SCALAR ']'
type_list(list(X)) --> ['['], type_scalar(X), [']'].
% TYPE_TUPLE -> '(' CSEQUENCE_TYPE ')'
type_tuple(tuple(X)) --> ['('], csequence_type(X), [')'].
% CSEQUENCE_TYPE -> TYPE
%                 | TYPE ',' CSEQUENCE_TYPE
csequence_type(X) --> type(X).
csequence_type([X|Y]) --> type(X), [','], csequence_type(Y).
% FDEFINE -> BLOCK_LET
%          | BLOCK_IF
% 	       | EXPR
fdefine(let(X)) --> block_let(X).
% fdefine(X) --> block_if(X).
% fdefine(X) --> expr(X).
% DEFINE_FUNC -> ID SEQUENCE_VARG '=' FDEFINE
define_func(id(X), [argument(Y)|body(Z)]) --> id(X), sequence_varg(Y), ['='], fdefine(Z).
% SEQUENCE_VARG -> e
% 	             | ID
% 	             | ID SEQUENCE_VARG
sequence_varg(X) --> id(X).
sequence_varg([X|Y]) --> id(X), sequence_varg(Y).
sequence_varg([]) --> [].
% BLOCK_LET -> 'let' DEFINE_FUNC 'in' FDEFINE
block_let(X) -->
    ['let'], {sublist(Y1, X)}, {sublist(Y2, X)}, define_func(Y1, Y2),
    ['in'], {sublist(Z, X)}, fdefine(Z).
% BLOCK_IF -> 'if' EXPR_BOOL 'then' EXPR 'else' EXPR
%
% EXPR_BOOL -> CMP
% 	   | CMP OPERATION_LOGIC EXPR_BOOL
%
% CMP -> EXPR OPERATION_CMP EXPR
%
% OPERATION_CMP -> '<'
% 	       | '>'
% 	       | '='
% 	       | '/='
%
% OPERATION_LOGIC -> '&&'
% 		 | '||'
%
%
%
% EXPR -> OPERAND OPERATION_ARITHMETIC EXPR
%       | OPERAND
%
% OPERAND -> CONST
% 	 | ID
% 	 | FUNC_CALL
%
% OPERATION_ARITHMETIC -> '+'
% 		      | '-'
% 		      | '/'
% 		      | '*'
%
% CONST -> CONST_INT
%        | CONST_BOOL
%        | CONST_LIST
%        | CONST_TUPLE
%
% CONST_INT -> 'int value'
%
% CONST_BOOL -> 'bool value'
%
% CONST_LIST -> 'list value'
%
% CONST_TUPLE -> 'tuple value'
%
% FUNC_CALL -> ID SEQUENCE_ARG
%
% SEQUENCE_ARG -> ID
% 	      | ID ' ' SEQUENCE_ARG
% 	      | CONST
% 	      | CONST ' ' SEQUENCE_ARG
% 	      | FUNC_CALL
% 	      | FUNC CALL ' ' SEQUENCE_ARG
