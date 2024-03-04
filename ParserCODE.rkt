#lang racket
; Parses a given source file into a list of (line-number, tokens) pairs.
(define (read-program source-file)
  (with-input-from-file source-file
    (lambda ()
      (for/list ((line (in-lines))
                 (line-num (in-naturals 1)))
        (cons line-num (filter (lambda (token) (not (string=? token "")))
                               (string-split line " ")))))))

; Splits tokens based on their type and associated line number.
(define (splitter tword)
  (match tword
    [(list line-num "id" id) `(id ,id ,line-num)]
    [(list line-num "num" num) `(num ,num ,line-num)]
    [(list line-num "true") `(bool true ,line-num)]
    [(list line-num "false") `(bool false ,line-num)]
    [(list line-num ">" symbol) `(rel ">" ,line-num)]
    [(list line-num "<" symbol) `(rel "<" ,line-num)]
    [(list line-num "=" symbol) `(rel "=" ,line-num)]
    [(list line-num "!=" symbol) `(rel "!=" ,line-num)]
    [(list line-num var "=" val) `(assign ,var ,val ,line-num)]
    [(list line-num "while" cond ...) `(while ,cond ... ,line-num)]
    [(list line-num "endwhile") `(endwhile ,line-num)]
    [(list line-num "gosub" target) `(gosub ,target ,line-num)]
    [(list line-num sym) (cond [(member sym '("+" "-" "*" "/" "(" ")" "=" "gosub")) `(sym ,sym ,line-num)]
                               [else (error "Unrecognized token" tword)])]))



; Parses expressions from a list of tokens.
(define (parse-expression tokens)
  ; Parses a boolean value.
  (define (parse-boolean tokens)
    (match tokens
      [(list (list 'bool value line-num) rest ...) (values `(boolean ,value) rest line-num)]
      [else (error "Invalid boolean in expression")]))
  
  ; Parses a relational expression.
  (define (parse-relation tokens)
    (let-values (((left-expr rest line-num) (parse-expr tokens)))
      (match rest
        [(list (list 'rel rel line-num) rest ...)
         (let-values (((right-expr rest line-next) (parse-expr rest)))
           (values `(rel ,rel ,left-expr ,right-expr) rest line-num))]
        [else (error "Expected relational operator")])))
  
  ; Parses an atomic expression, returning its type, value(s), and rest tokens.
  (define (parse-atom tokens)
    (match tokens
      [(list (list 'bool _ _) _) (parse-boolean tokens)]
      [(list (list 'num num line-num) rest ...) (values `(number ,num) rest line-num)]
      [(list (list 'id id line-num) rest ...) (values `(identifier ,id) rest line-num)]
      [else (error "Invalid atom in expression")]))
  
  ; Parses a factor in an expression.
  (define (parse-factor tokens)
    (match tokens
      [(list (list 'sym "(" _) rest ...)
       (let-values (((expr new-rest line-num) (parse-relation rest)))
         (match new-rest
           [(list (list 'sym ")" _) rest ...) (values expr rest line-num)]
           [else (error "Expected closing parenthesis")]))]
      [else (parse-atom tokens)]))
  
  ; Parses a term, applying multiplication or division as needed.
  (define (parse-term tokens)
    (let-values (((factor rest line-num) (parse-factor tokens)))
      (let loop ((factor factor) (tokens rest) (line-num line-num))
        (match tokens
          [(list (list 'sym "*" _) rest ...)
           (let-values (((next-factor rest line-next) (parse-factor rest)))
             (loop `(* ,factor ,next-factor) rest line-num))]
          [(list (list 'sym "/" _) rest ...)
           (let-values (((next-factor rest line-next) (parse-factor rest)))
             (loop `(/ ,factor ,next-factor) rest line-num))]
          [else (values factor tokens line-num)]))))
  
  ; Parses an expression, applying addition or subtraction as needed.
  (define (parse-expr tokens)
    (let-values (((term rest line-num) (parse-term tokens)))
      (let loop ((term term) (tokens rest) (line-num line-num))
        (match tokens
          [(list (list 'sym "+" _) rest ...)
           (let-values (((next-term rest line-next) (parse-term rest)))
             (loop `(+ ,term ,next-term) rest line-num))]
          [(list (list 'sym "-" _) rest ...)
           (let-values (((next-term rest line-next) (parse-term rest)))
             (loop `(- ,term ,next-term) rest line-num))]
          [else (values term tokens line-num)]))))
  
  (let-values (((expr rest line-num) (parse-expr tokens)))
    (if (null? rest)
        (values "Accept" expr)
        (values (format "Syntax Error on line ~a" line-num) #f))))

; Parses each file in the given list, applying the parsing process.
(define (parse-files file-list)
  (for-each (lambda (file-name)
              (printf "Parsing ~a:\n" file-name)
              (let* ([program (read-program file-name)]
                     [tokens (apply append (map splitter program))]
                     [result (parse-expression tokens)])
                (match result
                  [(list "Accept" expr) (printf "Accept\n")]
                  [(list error-msg _) (printf "~a\n" error-msg)])))
            file-list))

; Set of file names to parse.
(define file-names '("File01.txt" "File02.txt" "File03.txt" "File04.txt" "File05.txt"))
(parse-files file-names)