# Interpreter

A test project to play around a bit with Scala. The idea to write an
interpreter comes from the blog post "Let's build a simple interpreter", from
Ruslan's Blog at https://ruslanspivak.com/lsbasi-part1/.

The interpreter consist of a _lexer_, a _parser_ and an _interpreter_. The _lexer_
transforms the input string into tokens that the parser recognizes. As an
example, the expression

    x := 2 + 5 * 6

might get translated to

    Id("x"), Assign, IntToken(2), Plus, IntToken(5), Times, IntToken(6).

Note that the lexer does not care about the meaning of the tokens, because that
is the job of the _parser_. The parser works with a predefined set of rules that
it recognizes, called a grammar. In the example above, it might recognize an
assignment statement with an expression on the right. The parser also knows
about operator precedence and is responsible for making sure that multiplication
goes before addition.
The parser produces an abstract syntax tree (AST) to represent the structure that it
found. The example above would lead to an AST similar to:

                                 Assignment
                                 /         \
                                /           \
                          Var("x")        Plus
                                         /    \
                                        /      \
                                 Number(2)   Times
                                             /    \
                                            /      \
                                    Number(5)    Number(6)


Finally, the _interpreter_ would interpret an Abstract syntax tree.

#The grammar

Currently, the grammar that the interpreter recognizes consists of the following
rules:

    program: compound_statement

    compound statement: BEGIN statement_list END

    statement_list: statement ( SEMICOLON statement )*

    statement: compound_statement | assignment_statement | empty_statement

    assignment_statement: ID ASSIGN expression

    expression: term ( (PLUS | MINUS) term )*

    term: factor ( (TIMES | DIV) factor ) *

    factor: LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
          | INT
          | VARIABLE

The left of each colon is the name of the rule. The right of the colon describes
what kind of token combinations are described by the rule. If something is not
on this list, the parser does not recognize it.

The vertical bar | is the or: (PLUS | MINUS) means you can either have a plus
token  or a minus token at this location.
The ( .... ) * means that the part between the parentheses can occur zero, one
or multiple times in the pattern.
