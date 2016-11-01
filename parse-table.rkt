
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

(define (nonterms gram) (append (map car gram)))

(define (contains list x)
    (cond ((null? list) #f)
        ((eq? (car list) x) #t)
        (else (contains (cdr list) x))))