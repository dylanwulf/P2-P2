(define calc-gram
`(("P" ("SL" "$$"))
("SL" ("S" "SL") ())
("S" ("id" ":=" "E") ("read" "id") ("write" "E"))
("E" ("T" "TT"))
("T" ("F" "FT"))
("TT" ("ao" "T" "TT") ())
("FT" ("mo" "F" "FT") ())
("ao" ("+") ("-"))
("mo" ("*") ("/"))
("F" ("id") ("num") ("(" "E" ")"))
))

(define temp-firsts ;; delete later!
  '(("P" "id" "read" "write" "$$")
 ("SL" "id" "read" "write")
 ("S" "id" "read" "write")
 ("E" "id" "num" "(")
 ("T" "id" "num" "(")
 ("TT" "+" "-")
 ("FT" "*" "/")
 ("ao" "+" "-")
 ("mo" "*" "/")
 ("F" "id" "num" "("))
)

(define temp-eps ;; delete later!
  '(("P" #f)
 ("SL" #t)
 ("S" #f)
 ("E" #f)
 ("T" #f)
 ("TT" #t)
 ("FT" #t)
 ("ao" #f)
 ("mo" #f)
 ("F" #f))
)

(define (parse-table calc-gram)
  (map parse-helper calc-gram))

(define (parse-helper x)
  (append (list (car x)) (map all (cdr x))))

(define (all x) (cons '(1) (list x)))

(define (nonterms gram) (map car gram))

(define (contains list x)
    (cond ((null? list) #f)
        ((eq? (car list) x) #t)
        (else (contains (cdr list) x))))

(define get-terminals
  (lambda (gram)
    (get-terminals-helper gram (nonterms gram))
  )
)

(define get-terminals-helper
  (lambda (gram nonterms)
    (if (> (length gram) 0)
        (addifne (get-terminals-helper (cdr gram) nonterms) (get-terms-rhs-list (cdr (car gram)) nonterms))
        '()
    )
  )
)

(define get-terms-rhs-list
  (lambda (rhs-list nonterms)
    (if (> (length rhs-list) 0)
        (addifne (get-terms-rhs-list (cdr rhs-list) nonterms) (get-terms-rhs (car rhs-list) nonterms))
        '()
    )
  )
)

(define get-terms-rhs
  (lambda (rhs nonterms)
    (if (> (length rhs) 0)
        (if (contains nonterms (car rhs))
            (get-terms-rhs (cdr rhs) nonterms)
            (addifne (get-terms-rhs (cdr rhs) nonterms) (list (car rhs)))
        )
        '()
    )
  )
)

(define addifne
  (lambda (l x)
    (if (> (length x) 1)
        (addifne (addifne l (cdr x)) (list (car x)))
        (if (or (eq? (length x) 0) (contains l (car x)))
            l
            (append l x)
        )
    )
  )
)

; loops through every line of grammar
(define (firsts gram)
  (map firsts-helper gram))

; loops through every production on the right hand side of the line, pairing the list of firsts to the
; nonterminal that starts the line
(define (firsts-helper line)
  (flatten(cons (car line)
        (map firsts-helper2 (cdr line)))))

; If production starts with a terminal or the empty set, returns that item.
; If production starts with a nonterminal, it runs the function again on that nonterminal's productions
(define (firsts-helper2 production)
  (cond
    ((= (length production) 0) '())
    ((contains (get-terminals calc-gram) (car production))                                                  
      (car production))
    (else
      (map firsts-helper2 (cdr (get-line (car production) calc-gram))))))

; finds the line that starts with the specified nonterminal
(define (get-line nonterm gram)
  (if (equal? (car (car gram)) nonterm)
      (car gram)
      (get-line nonterm (cdr gram))))

; returns a list of pairs: first element in pair is nonterminal, second element is boolean value specifying
; whether that nonterminal has the empty set in its first set.
(define (eps gram)
  (map remove-repeats (empties gram)))

(define (remove-repeats line)
  (cond ((contains line #t) (cons (car line) #t))
        (else (cons (car line) #f))))

(define (empties gram)
  (map eps-helper gram))

(define (eps-helper line)
  (cons (car line)
        (map has-empty? (cdr line))))

(define (has-empty? production)
  (cond ((= (length production) 0) #t)
        ((> (length production) 0) #f)))

; removes excess parentheses
(define flatten
  (lambda (L)
    ; Return left-to-right fringe of tree as list.
    (cond
      ((null? L) L)
      ((list? (car L)) (append (flatten (car L)) (flatten (cdr L))))
      (else (cons (car L) (flatten (cdr L)))))))

;Combines the first sets of all symbols in the list 'symbols'. No duplicates are added.
(define string-first
  (lambda (symbols firsts eps terminals)
    (if (eq? (length symbols) 0)
        '()
        (if (contains terminals (car symbols))
            (list (car symbols))
            (if (cadr (get-line (car symbols) eps))
                (addifne (cdr (get-line (car symbols) firsts)) (string-first (cdr symbols) firsts eps terminals))
                (cdr (get-line (car symbols) firsts))
            )
        )
    )
  )
)

;Checks if all the symbols provided can lead to epsilon.
;true if they all can, false if one or more cannot.
(define string-eps
  (lambda (symbols eps terminals)
    (if (eq? (length symbols) 0)
        #t
        (if (contains terminals (car symbols))
            #f
            (if (cadr (get-line (car symbols) eps))
                (string-eps (cdr symbols) eps terminals)
                #f
            )
        )
    )
  )
)

;Adds 'addition' to a list in 'll' that starts with 'first'
(define add-to
  (lambda (ll first addition)
    (if (eq? (length ll) 0)
        '()
        (if (eq? (caar ll) first)
            (append (list (addifne (car ll) addition)) (add-to (cdr ll) first addition))
            (append (list (car ll)) (add-to (cdr ll) first addition))
        )
    )
  )
)

;Generates a blank follow set from the list of nonterminals.
(define blank-follow
  (lambda (nonterminals)
    (if (eq? (length nonterminals) 0)
        '()
        (append (list (list (car nonterminals))) (blank-follow (cdr nonterminals)))
    )
  )
)

;Calls check-follow-eq with a blank follow set. Nothing special.
(define follow
  (lambda (gram eps nonterms terms firsts)
    (check-follow-eq gram eps nonterms terms firsts (blank-follow nonterms))
  )
)

;This function simply makes check-follow-eq2 easier to read
(define check-follow-eq
  (lambda (gram eps nonterms terms firsts follows)
    (check-follow-eq2 gram eps nonterms terms firsts (follow-lines gram eps nonterms terms firsts follows))
  )
)

;Checks if follow set from current iteration is equivalent to follow set from last iteration.
;If they are equivalent, that means no change was made the entire last iteration,
;which means we are done and there is nothing more to add to the follow set.
;If they are NOT equivalent, that means we are not done and need to call
;follow-lines a few more times.
(define check-follow-eq2
  (lambda (gram eps nonterms terms firsts follows)
    (if (equal? follows (follow-lines gram eps nonterms terms firsts follows))
        follows
        (check-follow-eq gram eps nonterms terms firsts follows)
    )
  )
)

;Calls follow-line for each line in the grammar
(define follow-lines
  (lambda (gram eps nonterms terms firsts follows)
    (if (eq? (length gram) 0)
        follows
        (follow-lines (cdr gram) eps nonterms terms firsts (follow-line eps nonterms terms firsts follows (caar gram) (cdar gram)))
    )
  )
)

;Calls follow-production for each production in the current line
(define follow-line
  (lambda (eps nonterms terms firsts follows lhs prods)
    (if (eq? (length prods) 0)
        follows
        (follow-line eps nonterms terms firsts (follow-production eps nonterms terms firsts follows lhs (car prods)) lhs (cdr prods))
    )
  )
)

;Calculates which terminals to add to which follow sets for the current production
(define follow-production
  (lambda (eps nonterms terms firsts follows lhs rhs)
    ;If not the last symbol in the production, AND nonterminal
    (if (and (> (length rhs) 1) (contains nonterms (car rhs)))
        ;Then: If the rest of the symbols can all lead to epsilon
        (if (string-eps (cdr rhs) eps terms)
            ;Then: Add string_first AND follow of lhs to follow of (car rhs)
            (follow-production eps nonterms terms firsts (add-to (add-to follows (car rhs) (cdr (get-line lhs follows))) (car rhs) (string-first (cdr rhs) firsts eps terms)) lhs (cdr rhs))
            ;Else: Add only string_first to follow of (car rhs)
            (follow-production eps nonterms terms firsts (add-to follows (car rhs) (string-first (cdr rhs) firsts eps terms)) lhs (cdr rhs))
        )
      ;Else: If nonterminal AND last symbol in production
      (if (and (eq? (length rhs) 1) (contains nonterms (car rhs)))
          ;Then: Add follow of lhs to follow of (car rhs)
          (follow-production eps nonterms terms firsts (add-to follows (car rhs) (cdr (get-line lhs follows))) lhs (cdr rhs))
          ;Else: If we haven't exhausted rhs yet
          (if (> (length rhs) 0)
              ;Then: Call function again with the next symbol in rhs
              (follow-production eps nonterms terms firsts follows lhs (cdr rhs))
              ;Else: No more function calling, just return follows
              follows
          )
      )
    )
  )
)

(follow calc-gram temp-eps (nonterms calc-gram) (get-terminals calc-gram) temp-firsts)
