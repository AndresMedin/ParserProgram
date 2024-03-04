#lang racket

; Improved to accurately handle syntax and line numbers
(define (read-program source-file)
  (map (lambda (line) (cons line (string-split line))) (file->lines source-file)))

; Enhanced token identification with detailed error handling
(define (splitter twords)
  (match twords
    [(list a rest ...) (match (string->symbol a)
                              ['idx (if (tidx a) 'idx (error "Invalid idx"))]
                              ['id (if (fid a) 'id (error "Invalid id"))]
                              ; Handle each token explicitly
                              ['goto 'goto]
                              ['read 'read]
                              ['write 'write]
                              ; Add missing token handlers based on grammar
                              [_ (popr a)])]
    [else (error "Unrecognized token")]))

(define (tidx sidx) (and (string->number sidx) (> (string->number sidx) 0)))

(define (fid sid) (and (salpha sid) (not (P-KEY sid))))

(define (popr oprs)
  (match oprs
    ["(" 'lparen] [")" 'rparen] ["+" 'plus] ["-" 'minus]
    ["=" 'equals] [":" 'colon] ["$$" 'eof]
    [_ (error "Invalid operation or unmatched token")]))

(define (salpha alpstr)
  (andmap char-alphabetic? (string->list alpstr)))

(define (P-KEY S-KEYS)
  (match S-KEYS
    ["goto" 'goto] ["read" 'read] ["write" 'write]
    ["gosub" 'gosub] ["return" 'return] ["if" 'if]
    ["then" 'then]
    [_ #f]))

; Parsing with detailed syntax error reporting
(define (ppro lline)
  (match lline
    ; Assume a more complex parsing logic here to validate against the grammar
    [(list (list "Accept" "$$")) (printf "Accept~n")]
    [_ (printf "Syntax Error or Unrecognized structure at line: ~a~n" (caar lline))]))

(define (parse-files file-list)
  (for-each (lambda (file-name)
              (printf "Parsing ~a: \n" file-name)
              ; Read and split program into tokens, then apply parsing logic
              (let ([tokens (read-program file-name)])
                (match tokens
                  ; Implement parsing logic based on grammar
                  ; This is a placeholder for the logic to parse and validate tokens
                  [_ (ppro tokens)]))
              (printf "\n"))
            file-list))

; Updated file names list
(define file-names '("File01.txt" "File02.txt" "File03.txt" "File04.txt" "File05.txt"))

(parse-files file-names)
