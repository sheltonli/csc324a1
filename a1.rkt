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
  (define characters-text-list (split-left body "Finis"))
  (define functions-text-list (split-left (split-right body "Finis") "Finis"))
   (define dialogue-text-list
    (if (empty? functions-text-list)
        (split-right body "Finis")
        (split-right (split-right body "Finis") "Finis")))
  (define characters-value-list (map evaluate-character characters-text-list))
  (define functions-value-list (map evaluate-function functions-text-list))
  (evaluate-dialogue dialogue-text-list '() characters-value-list functions-value-list)
  )

; The following two helper functions are used to section the body text into characters, functions and dialogue

#|
(split-left body line)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.
  line: a string

  Returns a list of lines excluding the first line before the first occurence of line,
  else return an empty list if line does not occur in body.

> (split-left '("First line" "Second line" "Test" "Third line") "Test")
'("Second line")
|#

(define (split-left body line)
  (if (equal? (sublist (list line) body) #f)
      '()
      (rest (take body (sublist (list line) body)))))

#|
(split-right body line)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.
  line: a string

  Returns a list of lines after the first occurence of line,
  else return an empty list if line does not occur in body.

> (split-right '("First line" "Test" "Second line" "Third line") "Test")
'("Second line" "Third Line")
|#
(define (split-right body word)
  (if (equal? (sublist (list word) body) #f)
      '()
      (rest (drop body (sublist (list word) body)))))

#|
(evaluate-character line)
  line: a string from Dramatis Personae of type "<char>, <description>"

  Splits the character and description and returns a list of character name
  and the value according to FunShake evaluation for literals.

> (evaluate-character "Bob, a charming young nobleman from Verona")
'("Bob" 6)
> (evaluate-character "Bob, a scoundrelous and vile merchant")
'("Bob" -20)
|#
(define (evaluate-character line)
  (let* ([name (first (string-split line ","))]
         [description (first (rest (string-split line ",")))]
         [bad-word-count (count-bad-words (string-split description) 0)])
    
    (if (> bad-word-count 0)
        (list name (evaluate-bad description bad-word-count))
        (list name (evaluate-normal description)))))

#|
(evaluate-function line)
  line: a string from Settings of type "<name>, <expr>"

  Splits the name and expression and returns a list of function name
  and the expression.

> (evaluate-function "Verona, a magical unicorn gathering entranc'd by Hamlet.")
'("Verona" " a magical unicorn gathering entranc'd by Hamlet.")
|#
(define (evaluate-function line)
  (let* ([f-name (first (string-split line ","))]
         [description (first (rest (string-split line ",")))])
   (list f-name description)))

#|
(count-bad-words description-list acc)
  description-list: a list of words
  acc: a integer counter used to count number of bad words

  Counts the number of bad words in description-list according to FunShake
  and returns the count of such bad-words.

> (count-bad-words '("a" "vile" "and" "wicked" "man") 0)
2
> (count-bad-words '("a" "good" "and" "cool" "man") 0)
0
|#
(define (count-bad-words description-list acc)
  (if (empty? description-list)
      acc
      (if (equal? (member (first description-list) bad-words) #f)
          (count-bad-words (rest description-list) acc)
          (count-bad-words (rest description-list) (+ acc 1)))))

#|
(evaluate-bad description b)
  description: a string representing a description
  b: a integer representing the number of bad words in description

  Evaluates the description with the given number of bad words and
  returns an integer value.

> (evaluate-bad "a vile and wicked man" 2)
-20
|#
(define (evaluate-bad description b)
  (* (* -1 (expt 2 b)) (length (string-split description))))

#|
(evaluate-normal description)
  description: a string representing a description

  Evaluates the description normally by counting the number of words,
  returns an integer value.

> (evaluate-normal "this should be five words")
5
|#
(define (evaluate-normal description)
  (length (string-split description)))

#|
(evaluate-dialogue dialogue-list acc cvl fvl)
  dialogue-list: a list of dialogue lines
  acc: list, used to accumulate the values for each line
  cvl: a list of lists, list that stores lists of key, values (characters and their calculated value)
  fvl: a list of lists, list that stores lists of key, values (function and expression)

  Evaluates the dialogue by appending each line's value after
  being evaluated according to FunShake. Checks edge case where dialogue
  line is empty (ie. just a period). In this case the value of the line is 0.

> (evaluate-dialogue '("Shelton" "One two" "Aaron:" "One join'd with one") '() '(("Shelton" 5)("Aaron" 10)) '())
'(2 2)
|#
(define (evaluate-dialogue dialogue-list acc cvl fvl)
  (if (empty? dialogue-list)
      acc
      (if (empty? (rest dialogue-list))
          (append acc (list 0))
          (if (evaluate-blank (first (rest dialogue-list)))
              (evaluate-dialogue (rest dialogue-list) (append acc (list 0)) cvl fvl)
              (let* ([name (first (string-split (first dialogue-list) ":"))])
                (evaluate-dialogue (rest (rest dialogue-list)) (append acc (list (evaluate-line name (first (rest dialogue-list)) cvl fvl))) cvl fvl))))))

#|
(evaluate-blank line)
  line: a string, dialogue line

  This function is used to determine if the next line of dialogue is blank or not.
  If there was a blank line of dialogue, it gets ignored when it is parsed so
  the next line should be the name of the character who says the proceeding line.
  Returns true if line is of length one and has a colon (ie. "Bob:") and false otherwise.

> (evaluate-blank "Bob:")
#t
> (evaluate-blank "One join'd with one")
#f
|#
(define (evaluate-blank line)
  (if (equal? (length (string-split line)) 1)
      (let* ([line-length (string-length line)])
        (if (equal? (string-ref line (- line-length 1)) #\:)
            #t
            #f))
      #f))

#|
(evaluate-line name line cvl fvl)
  dialogue-list: a list of dialogue lines
  name: a string, character who said the line
  line: a string representing a line of dialogue
  cvl: a list of lists, list that stores lists of key, values (characters and their calculated value)
  fvl: a list of lists, list that stores lists of key, values (function and expression)

  Evaluates the line by calling do-calculation. Figures out if the line is
  a function call. There are two csaes when it is a function call:
  1. If it is a nested function call, it will call do-function.
  2. Else it will calculate the value of the function with the argument.
  Returns an integer value for the line after being evaluated according to FunShake.

> (evaluate-line "Shelton" "One join'd with one" '(("Shelton" 5)("Aaron" 10)) '())
2
> (evaluate-line "Shelton" "The song of Scotland and one" '(("Shelton" 5)) '(("Verona" "a magical unicorn gathering entranc'd by Hamlet.") ("Scotland" "The song of Verona and Hamlet join'd with one")))
8
|#
(define (evaluate-line name line cvl fvl)
  (define function-splitter (make-splitter "The song of"))
  (define and-splitter (make-splitter "and"))
  (cond
    [(function-splitter line)
     (let* ([function-splitter-res (function-splitter line)]
            [new-line (string-join (first (rest function-splitter-res)))]
            [and-splitter-res (and-splitter new-line)]
            [function-name (first (first and-splitter-res))]
            [function-argument (string-join (first (rest and-splitter-res)))]
            [func (retrieve-value fvl function-name)])
       (if (function-splitter func)
           (do-function func (do-calculation name function-argument cvl) name cvl fvl)
           (do-hamlet-calculation func (do-calculation name function-argument cvl) name cvl)))]
    [else (do-calculation name line cvl)]))

#|
(do-function func evaluated-arg name cvl fvl)
  func: a string representing a function expression
  evaluated-arg: integer representing the value of the argument to func
  name: a string, character who said the line
  cvl: a list of lists, list that stores lists of key, values (characters and their calculated value)
  fvl: a list of lists, list that stores lists of key, values (function and expression)

  Evaluates a function according to FunShake. A helper function to evaluate-line.
  Similar to evaluate-line, this function checks if there are nested function calls. 
  1. If it is a nested function call, it will call do-function again but with the argument being calculated
  with respect to Hamlet.
  2. Else it will calculate the value of the function with respect to Hamlet.
  Returns an integer value for the line after being evaluated according to FunShake.

> (do-function "The song of Verona and Hamlet join'd with one." 5 "Shelton" '(("Shelton" 5)) '(("Verona" "a magical unicorn gathering entranc'd by Hamlet.") ("Scotland" "The song of Verona and Hamlet join'd with one")))
24
|#
(define (do-function func evaluated-arg name cvl fvl)
  (define function-splitter (make-splitter "The song of"))
  (define and-splitter (make-splitter "and"))
  (cond
    [(function-splitter func)
     (let* ([function-splitter-res (function-splitter func)]
            [new-line (string-join (first (rest function-splitter-res)))]
            [and-splitter-res (and-splitter new-line)]
            [function-name (first (first and-splitter-res))]
            [function-argument (string-join (first (rest and-splitter-res)))]
            [func (retrieve-value fvl function-name)])
       (if (function-splitter func)
           (do-function func (do-hamlet-calculation function-argument evaluated-arg name cvl) name cvl fvl)
           (do-hamlet-calculation func (do-hamlet-calculation function-argument evaluated-arg name cvl) name cvl)))]))
#|
(do-hamlet-calculation line evaluated-arg name cvl)
  line: a string, more so an expression that contains a Hamlet
  evaluated-arg: an integer representing the value of an argument to the FunShake function
  name: a string, character name who says the line of dialogue
  cvl: a list of lists, list that stores lists of key, values (characters and their calculated value)

  Evaluates an expression that contains a "Hamlet" according to FunShake. Returns an integer
  that is the value of the line according to FunShake.

  Cases:
  1. Expression contains an entranc'd by. Split the expression on entranc'd by.
     A) If the first expression is Hamlet, replace it with evaluated-arg,
     multiply it with the calculated value of expression 2.
     B) If the second expression is Hamlet, replace it with evaluated-arg,
     multiply it with the calculated value of expression 1.
  2. Expression contains an join'd with. Split the expression on join'd with.
     A) If the first expression is Hamlet, replace it with evaluated-arg,
     add it to the calculated value of expression 2.
     B) If the second expression is Hamlet, replace it with evaluated-arg,
     add it to the calculated value of expression 1.
  3. Expression is Hamlet itself so just return evaluated-arg.

> (do-hamlet-calculation "Test join'd with Hamlet" 5 "Shelton" '('("Shelton" 5)))
6
|#
(define (do-hamlet-calculation line evaluated-arg name cvl)
  (let* ([entrancd-splitter (make-splitter "entranc'd by")]
         [joind-splitter (make-splitter "join'd with")])
    (cond
      [(entrancd-splitter line)
       (let* ([splitter-res (entrancd-splitter line)]
              [ex1 (string-join (first splitter-res))]
              [ex2 (string-join (first (rest splitter-res)))])
         (if (equal? ex1 "Hamlet")
             (entrancd evaluated-arg (do-calculation name ex2 cvl))
             (entrancd (do-calculation name ex1 cvl) evaluated-arg)))]
      [(joind-splitter line)
       (let* ([splitter-res (joind-splitter line)]
              [ex1 (string-join (first splitter-res))]
              [ex2 (string-join (first (rest splitter-res)))])
         (if (equal? ex1 "Hamlet")
             (joind evaluated-arg (do-calculation name ex2 cvl))
             (joind (do-calculation name ex1 cvl) evaluated-arg)))]
      [else evaluated-arg])))

#|
(do-calculation name line cvl)
  name: a string, character name who says the line of dialogue
  line: a string, line of dialogue
  cvl: a list of lists, list that stores lists of key, values (characters and their calculated value)

  Evaluates the line of dialogue according to FunShake. Returns an integer
  that is the value of the line according to FunShake.

  Cases:
  1. Line of dialogue contains an entranc'd by. Calculate the value of the
  first expression, the second expression, and then multiply them together.
  2. Line of dialogue contains an join'd with. Calculate the value of the
  first expression, the second expression, and then add them together.
  3. Singular word in the line.
     A) Word is a self identifier, return value of character who says line.
     B) Word is a name, return value of that name by referring to cvl.
     C) Evaluate depending on if word is a bad word or not.
  4. None of the above cases, so evaluate like a regular description.

> (do-calculation "Shelton" "Test join'd with test" '('("Shelton" 5)))
2
> (do-calculation "Shelton" "Myself entranc'd by a vile woman" '(("Shelton" 5)))
-30
|#
(define (do-calculation name line cvl)
   (let* ([bad-word-count (count-bad-words (string-split line) 0)]
          [entrancd-splitter (make-splitter "entranc'd by")]
          [joind-splitter (make-splitter "join'd with")])
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
       [(equal? (length (string-split line)) 1)
        (cond
          [(member line self-refs) (retrieve-value cvl name)]
          [(retrieve-value cvl line) (retrieve-value cvl line)]
          [else (if (> bad-word-count 0)
                    (evaluate-bad line bad-word-count)
                    (evaluate-normal line))])]
       [else 
        (if (> bad-word-count 0)
        (evaluate-bad line bad-word-count)
        (evaluate-normal line))])))
#|
(entrancd x y)
  x: an integer
  y: an integer

  Returns the value of x and y multipled together.
|#
(define (entrancd x y)
  (* x y))

#|
(joind x y)
  x: an integer
  y: an integer

  Returns the value of x and y added together.
|#
(define (joind x y)
  (+ x y))
       
#|
(retrieve-value lst value)
  lst: a list of lists
  value: a string

  Checks if value is a key in lst, which is a list of key, value lists.
  If value is a key in lst, this function returns the value for that key.
  Otherwise, the function returns #f.

> (retrieve-value '('("Shelton" 5) '("Bob" 10)) "Shelton")
5
> (retrieve-value '('("Shelton" 5) '("Bob" 10)) "Aaron")
#f
|#
(define (retrieve-value lst value)
  (if (empty? lst)
      #f
      (let* ([name (first (first lst))]
             [val (first (rest (first lst)))])
        (if (equal? value name)
            val
            (retrieve-value (rest lst) value)))))
            
#|
(sublist sub lst)
  sub: a list
  lst: a list

  Checks whether 'sub' is a sublist of 'lst' (i.e., all the items in
  'sub' appear consecutively in 'lst').

  If 'sub' is a sublist of 'lst', this function returns the *index*
  of the first element of the first occurrence of 'sub' within 'lst'.
  Otherwise, this function returns #f.

  Note that the empty list is a sublist of every list, and it first
  occurs at index 0.

> (sublist '(30 40) '(10 20 30 40 50))
2
> (sublist '(20 30) '(10 20 30 20 30 40 50))
1
> (sublist '(1 2 3) '(5 4 3 2 1))
#f
|#
(define (sublist sub-lst lst)
  (sublist-helper sub-lst lst 0))

;Helper function to the above function
(define (sublist-helper sub-lst lst acc)
  (cond
    [(empty? sub-lst) 0]
    [(empty? lst) #f]
    [(equal? (check-sublist sub-lst lst) #t) acc]
    [else (sublist-helper sub-lst (rest lst) (+ acc 1))]))

#|
(check-sublist sub-lst lst)
  sub-lst: a list
  lst: a list

  Checks if sub-lst is a sublist of lst by comparing from the
  first item of each list. Returns #t if sub-lst is a sublist of lst
  and #f otherwise.

>(check-sublist '(1 2) '(4 5 1 2 6 7))
#f
>(check-sublist '(1 2) '(1 2 1 6 7))
#t
|#
(define (check-sublist sub-lst lst)
  (cond
    [(empty? sub-lst) #t]
    [(empty? lst) #f]
    [(equal? (first sub-lst) (first lst)) (check-sublist (rest sub-lst) (rest lst))]
    [else #f]))

#|
(make-splitter splitter)
  splitter: a string

  Returns a function f that takes a string s and implements the following
  behaviour:
    1. If 'splitter' is *not* a substring of the 's' list,
       return #f.
    2. Else, return a list of two elements (list before after), where
       'before' is the list of words in 's' *before* the first occurrence
       of 'splitter', and 'after' is the list of words in 's' *after* the
       first occurrence of 'splitter'. Note that neither 'before' nor
       'after' include the words from the first occurrence of 'splitter'.

> (define f (make-splitter "hello world"))
> (f "this is a hello world kind of party")
'(("this" "is" "a") ("kind" "of" "party"))
> (f "this is a hello not world kind of party")
#f

|#
(define (make-splitter splitter)
  (lambda (lst)
    (let* ([n (sublist (string-split splitter) (string-split lst))])
      (if (equal? n #f)
          #f
          (list (take (string-split lst) n) (drop (string-split lst) (+ n (length (string-split splitter)))))))))
