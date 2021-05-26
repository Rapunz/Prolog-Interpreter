# Prolog-Interpreter
A simple Prolog interpreter for a school assignment
Grammar:
block = ‘{’,stmts,‘}’;stmts = [assign,stmts];assign = id,‘=’,expr , ‘;’;expr = term , [(‘+’ | ‘−’),expr];term = factor,[(‘*’ | ‘/’),term];factor = int | id | ‘(’,expr,‘)’;
