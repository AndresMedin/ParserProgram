#lang racket

;Reads files
(define (read-program source-file)
  (map (lambda (line) (cons line (string-split line))) (file->lines source-file)))

;Slitter twords function
(define (splitter twords)
  (match twords
    [(list a rest ...) (match (string->symbol a)
                              ['idx (if (tidx a) 'idx (error "Invalid idx"))]
                              ['id (if (fid a) 'id (error "Invalid id"))]
                              ['goto 'goto]
                              ['read 'read]
                              ['write 'write]
                              [_ (popr a)])]
    [else (error "Unrecognized token")]))

;Checks if a string represents a positive number.
(define (tidx sidx) (and (string->number sidx) (> (string->number sidx) 0)))

;Checking if the string consists only of alphabetic characters and is not a reserved keyword
(define (fid sid) (and (salpha sid) (not (P-KEY sid))))

;Maps operation symbols
(define (popr oprs)
  (match oprs
    ["(" 'lparen] [")" 'rparen] ["+" 'plus] ["-" 'minus]
    ["=" 'equals] [":" 'colon] ["$$" 'eof]
    [_ (error "Invalid operation or unmatched token")]))

;Checks if a string consists entirely of alphabetic characters
(define (salpha alpstr)
  (andmap char-alphabetic? (string->list alpstr)))

;Returns a symbolic representation if the string is a keyword
(define (P-KEY S-KEYS)
  (match S-KEYS
    ["goto" 'goto] ["read" 'read] ["write" 'write]
    ["gosub" 'gosub] ["return" 'return] ["if" 'if]
    ["then" 'then]
    [_ #f]))

;Parsing with detailed syntax error reporting
(define (ppro lline)
  (match lline
    [(list (list "Accept" "$$")) (printf "Accept~n")]
    [_ (printf "Syntax Error or Unrecognized structure at line: ~a~n" (caar lline))]))

;Parse-File function
(define (parse-files file-list)
  (for-each (lambda (file-name)
              (printf "Parsing ~a: \n" file-name)
              (let ([tokens (read-program file-name)])
                (match tokens
                  [_ (ppro tokens)]))
              (printf "\n"))
            file-list))

; File names list
(define file-names '("File01.txt" "File02.txt" "File03.txt" "File04.txt" "File05.txt"))

(parse-files file-names)
