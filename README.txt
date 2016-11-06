# P2-P2
Programming Languages Project 2 Part 2

David Shull and Dylan Wulf

This project is written in Scheme programming language. In order to run the
code, a grammar must first be defined (there is a basic calculator grammar,
called calc-gram, built into the code already, but the code will work with any
grammar formatted in the same way). The input grammar must consist of a list of
lists, one per non-terminal in the grammar. The first element of each sublist
should be the non-terminal; the remaining elements should be the right- hand
sides of the productions for which that non-terminal is the left-hand side. The
sublist for the start symbol must come first. Every grammar symbol must be
represented as a quoted string.

By typing (parse-table <insert grammar name here>) at the command line, the
code will execute. The output is the parse-table of predict sets associated
with this grammar. The parse table, as returned by function parse-table, has
the same format as the input, except that every right-hand side is replaced by
a pair (a 2-element list) whose first element is the predict set for the
corresponding production, and whose second element is the right-hand side.

Our program is not particularly efficient in regards to its use of the stack or
its runtime. However, as this is our first attempt at using not only Scheme,
but a functional programming language in general, we think that the efficiency
is as good as can be expected.

As everything in Scheme is a list, lists were the only data structure that we
used. Our algorithms are all very commonplace, so we doubt that anything we have
done is dramatically different than what other people in our class will do.

Division of Labor:
David - Wrote various helper functions, as well as functions for first sets,
empty sets, and nonterminals.
Dylan - Wrote various helper functions, as well as functions for follow sets,
predict sets, and terminals.
