
/*** 
Load the tokenizer (tokenize/2) and the file_writer (write_to_file/3).
***/
:- [tokenizer].
:- [filewriter].


/***
The top level predicate run/2 of the solution.
To be called like this:
?- run('program1.txt','myparsetree1.txt').
***/
run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	write_to_file(OutputFile,ParseTree,VariablesOut).
	
/***
parse(-ParseTree)-->
	A grammar defining your programming language,
	and returning a parse tree.
***/

/* WRITE YOUR CODE FOR THE PARSER HERE */
parse(Tree) --> block(Tree).

block(block(left_curly, Stmts, right_curly)) --> ['{'],stmts(Stmts),['}']. 


stmts(statements(Assign, Stmts))-->assign(Assign),stmts(Stmts).
stmts(statements)-->[].

assign(assignment(Id, assign_op, Expr, semicolon)) --> ident(Id), [=], expr(Expr), [;].

expr(expression(Term, add_op, Expr)) --> term(Term), [+], expr(Expr).
expr(expression(Term, sub_op, Expr)) --> term(Term), [-], expr(Expr).
expr(expression(Term)) --> term(Term).

term(term(Factor, mult_op, Term)) --> factor(Factor), [*], term(Term).
term(term(Factor, div_op, Term)) --> factor(Factor), [/], term(Term).
term(term(Factor)) --> factor(Factor).

factor(factor(Int)) --> int(Int).
factor(factor(Id)) --> ident(Id).
factor(factor(left_paren, Expr, right_paren)) --> ['('], expr(Expr), [')'].

int(int(X)) --> [X], {integer(X)}.
ident(ident(X)) --> [X], {atom(X)}.
	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
evaluate(block(left_curly, Stmts, right_curly), VariablesIn, Eval):- 
	evaluate(Stmts, VariablesIn, Eval).

evaluate(statements(Assign,Stmts), VariablesIn, VariablesOut2) :- 
	evaluate(Assign, VariablesIn, VariablesOut1),
	evaluate(Stmts, VariablesOut1, VariablesOut2).
evaluate(statements, VariablesIn, VariablesIn).

evaluate(assignment(ID, assign_op, Expr, semicolon), VariablesIn, Eval):-
	evaluate(ID, VariablesIn, VariablesOut,_), 
	evaluate(Expr, VariablesOut, VariablesOut2, Val), 
	setValue(ID, Val, VariablesOut2, Eval).

evaluate(expression(Term, Op, Expr), VariablesIn, VariablesOut2, Val):- 
	evaluate(Term, VariablesIn, VariablesOut1, Val1), 
	evaluate(Expr,VariablesOut1, VariablesOut2, Val, Val1, Op).
evaluate(expression(Term),VariablesIn, VariablesOut, Val):-
	evaluate(Term, VariablesIn, VariablesOut, Val).

evaluate(term(Factor, Op, Term),VariablesIn, VariablesOut2, Val) :- 
	evaluate(Factor, VariablesIn, VariablesOut1, Val1), 
	evaluate(Term, VariablesOut1, VariablesOut2, Val, Val1, Op).
evaluate(term(Factor), VariablesIn, VariablesOut, Val):-
	evaluate(Factor, VariablesIn, VariablesOut, Val).

evaluate(factor(int(Int)),VariablesIn, VariablesIn, Int).
evaluate(factor(ID),VariablesIn, VariablesOut, Val):-
	evaluate(ID,VariablesIn,VariablesOut,Val).
evaluate(factor(left_paren, Expr, right_paren),VariablesIn, VariablesOut, Val) :- 
	evaluate(Expr,VariablesIn, VariablesOut, Val).

%These two predicates ensure that every encountered variable gets added to the variablelist. Returns a new list with the variable added if not already there, 
%then it returns the same list. Also returns the value for the variable stored in the list
evaluate(ident(ID), VariablesIn, VariablesIn, Val):-
	is_member(ID=Val,VariablesIn).
evaluate(ident(ID),VariablesIn, [ID=0|VariablesIn],0):-
	is_not_member(ID=_,VariablesIn).

/*The evaluate-predicates below are used to force left associativity when an expression/term was preceded by an operation in another expression/term.
They do this by taking the calculated value of the root expression/term for this recursionloop so far and performing the operation from the last Expression/Term 
with this calculated value and the value of the Term/Factor of this node, before making another recursive call (if this should be done, otherwise just returning 
the result of the operation)*/
evaluate(expression(Term, Op, Expr), VariablesIn, VariablesOut2, ValOut, PrevVal, PrevOp):- 
	evaluate(Term, VariablesIn, VariablesOut1, TermVal), 
	operation(PrevVal, PrevOp, TermVal, ValSoFar), 
	evaluate(Expr, VariablesOut1, VariablesOut2, ValOut, ValSoFar, Op).
evaluate(expression(Term),VariablesIn,VariablesOut, ValOut, PrevVal, PrevOp):-
	evaluate(Term, VariablesIn, VariablesOut, TermVal), 
	operation(PrevVal, PrevOp, TermVal, ValOut).

evaluate(term(Factor, Op, Term), VariablesIn, VariablesOut2, ValOut, PrevVal, PrevOp):- 
	evaluate(Factor, VariablesIn, VariablesOut1, FacVal), 
	operation(PrevVal, PrevOp, FacVal, ValSoFar), 
	evaluate(Term, VariablesOut1, VariablesOut2, ValOut, ValSoFar,Op).
evaluate(term(Factor), VariablesIn, VariablesOut, ValOut, PrevVal, PrevOp):-
	evaluate(Factor, VariablesIn, VariablesOut,FacVal), 
	operation(PrevVal, PrevOp, FacVal, ValOut).


%Takes 2 values and an aritmethic operation and returns the result of the operation performed on the given values
operation(X,add_op,Y,Z):-Z is X + Y.  
operation(X,sub_op,Y, Z):-Z is X - Y.
operation(X,mult_op,Y,Z):-Z is X * Y.  
operation(X,div_op,Y, Z):-Z is X / Y.

%Takes an element and a list and returns true if element is part of the list
is_member(X,[X|_Ys]).
is_member(X,[_Y|Ys]):- is_member(X,Ys).

%Takes an element and a list and returns true if element is not part of the list
is_not_member(_X,[]).
is_not_member(X,[Y|Ys]):- X \= Y, is_not_member(X,Ys).


%Takes an Id, a Value and a List and returns a new list where the value for the given Id is set to the given value.
setValue(ident(ID), Val, [ID=_ | Xs], [ID=Val | Xs]).
setValue(ident(ID), Val, [X|Xs], [X|Ys]) :- setValue(ident(ID), Val, Xs, Ys).

