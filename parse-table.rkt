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
