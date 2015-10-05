#| Assignment 1 - Functional Shakespeare Interpreter

Read through the starter code carefully. In particular, look for:

- interpret: the main function used to drive the program.
  This is provided for you, and should not be changed.
- evaluate: this is the main function you'll need to change.
  Please put all helper functions you write below this one.
  Doing so will greatly help TAs when they are marking. :)
|#
#lang racket

; You are allowed to use all the string functions in this module.
; You may *not* import any other modules for this assignment.
(require racket/string)

; This exports the main driver function. Used for testing purposes.
; This is the only function you should export. Don't change this line!
(provide interpret)

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add "join'd with")
(define mult "entranc'd by")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of")

; Function parameter name
(define param "Hamlet")

;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
  You can complete this assignment without modifying this function at all,
  but you may change the implementation if you like. Please note that you may
  not change the interface, as this is the function that will be autotested.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation (YOUR WORK GOES HERE)
;------------------------------------------------------------------------------

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
  This should be the main starting point of your work! Currently,
  it just outputs the semantically meaningful lines in the file.
|#
(define (evaluate body)
  ; TODO: Change this part!
  (define characters-text-list (split-left body "Finis"))
  (define functions-text-list (split-left (split-right body "Finis") "Finis"))
   (define dialogue-text-list
    (if (empty? functions-text-list)
        (split-right body "Finis")
        (split-right (split-right body "Finis") "Finis")))
  (define characters-value-list (map evaluate-character characters-text-list))
  (evaluate-dialogue dialogue-text-list '() characters-value-list)
  )

; The following two helper functions are used to section the body text into characters, functions and dialogue

; Return a list of everything before the first occurence of word, else return empty list if word does not occur in body
(define (split-left body word)
  (if (equal? (sublist (list word) body) #f)
      '()
      (rest (take body (sublist (list word) body)))))

; Return a list of everything after the first occurence of word, else return empty list if word does not occur in body
(define (split-right body word)
  (if (equal? (sublist (list word) body) #f)
      '()
      (rest (drop body (sublist (list word) body)))))

; Character parsing functions
(define (evaluate-character line)
  (let* ([name (first (string-split line ","))]
         [description (first (rest (string-split line ",")))]
         [bad-word-count (count-bad-words (string-split description) 0)])
    
    (if (> bad-word-count 0)
        (list name (evaluate-bad description bad-word-count))
        (list name (evaluate-normal description)))))

(define (count-bad-words description-list acc)
  (if (empty? description-list)
      acc
      (if (equal? (member (first description-list) bad-words) #f)
          (count-bad-words (rest description-list) acc)
          (count-bad-words (rest description-list) (+ acc 1)))))

; Evaluate expressions
(define (evaluate-bad description b)
  (* (* -1 (expt 2 b)) (length (string-split description))))

(define (evaluate-normal description)
  (length (string-split description)))

; Settings parsing functions
;(define (evaluate-setting line)
;  (let* ([name (first (string-split line ","))]
;         [expression (first (rest (string-split line ",")))])
;    ()))

(define (evaluate-dialogue dialogue-list acc cvl)
  (if (empty? dialogue-list)
      acc
      (let* ([name (first (string-split (first dialogue-list) ":"))])
        (evaluate-dialogue (rest (rest dialogue-list)) (append acc (list (evaluate-line name (first (rest dialogue-list)) cvl))) cvl))))

(define (evaluate-line name line cvl)
  (define entrancd-splitter (make-splitter "entranc'd by"))
  (define joind-splitter (make-splitter "join'd with"))
  (cond
    [(entrancd-splitter line)
     (let* ([splitter-res (entrancd-splitter line)]
            [ex1 (string-join (first splitter-res))]
            [ex2 (string-join (first (rest splitter-res)))])
        (entrancd (do-calculation name ex1 cvl) (do-calculation name ex2 cvl)))]
    [(joind-splitter line)
     (let* ([splitter-res (joind-splitter line)]
            [ex1 (string-join (first splitter-res))]
            [ex2 (string-join (first (rest splitter-res)))])
        (joind (do-calculation name ex1 cvl) (do-calculation name ex2 cvl)))]
    [else (do-calculation name line cvl)]))

(define (do-calculation name line cvl)
  name
   (let* ([bad-word-count (count-bad-words (string-split line) 0)])
     (cond
       [(equal? (length (string-split line)) 1)
        (cond
          [(member line self-refs) (retrieve-value cvl name)]
          [(member-nested cvl line) (retrieve-value cvl line)]
          [else (if (> bad-word-count 0)
                    (evaluate-bad line bad-word-count)
                    (evaluate-normal line))])]
       [else 
        (if (> bad-word-count 0)
        (evaluate-bad line bad-word-count)
        (evaluate-normal line))])))

(define (entrancd x y)
  (* x y))

(define (joind x y)
  (+ x y))

; Check if member of nested list
(define (member-nested lst value)
  (if (empty? lst)
      #f
      (if (equal? value (first (first lst)))
          #t
          (member-nested (rest lst) value))))
          

; Retrieve value from nested list
(define (retrieve-value lst value)
  (if (empty? lst)
      #f
      (let* ([name (first (first lst))]
             [val (first (rest (first lst)))])
        (if (equal? value name)
            val
            (retrieve-value (rest lst) value)))))
            
; Sublist function from EX1
(define (sublist sub-lst lst)
  (sublist-helper sub-lst lst 0))

(define (sublist-helper sub-lst lst acc)
  (cond
    [(empty? sub-lst) 0]
    [(empty? lst) #f]
    [(equal? (check-sublist sub-lst lst) #t) acc]
    [else (sublist-helper sub-lst (rest lst) (+ acc 1))]))

(define (check-sublist sub-lst lst)
  (cond
    [(empty? sub-lst) #t]
    [(empty? lst) #f]
    [(equal? (first sub-lst) (first lst)) (check-sublist (rest sub-lst) (rest lst))]
    [else #f]))

; Splitter function from EX3
(define (make-splitter splitter)
  (lambda (lst)
    (let* ([n (sublist (string-split splitter) (string-split lst))])
      (if (equal? n #f)
          #f
          (list (take (string-split lst) n) (drop (string-split lst) (+ n (length (string-split splitter)))))))))

;(interpret "descriptions.txt")
;(interpret "name_lookup.txt")
;(interpret "sample.txt")
;(interpret "part1.txt")
;(interpret "arithmetic.txt")
