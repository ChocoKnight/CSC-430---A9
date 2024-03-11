#lang typed/racket

(require typed/rackunit)

;; Assignment Progress: Complete

(define-type ExprC (U NumC IdC StrC IfC LamC AppC))
(struct NumC
  ([n : Real]) #:transparent)
(struct IdC
  ([id : Symbol]) #:transparent)
(struct StrC
  ([str : String]) #:transparent)
(struct IfC
  ([condition : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC
  ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC
  ([func : ExprC] [arguments : (Listof ExprC)]) #:transparent)

(define-type Value (U NumV StrV CloV BoolV PrimV))
(struct NumV
  ([n : Real]) #:transparent)
(struct StrV
  ([str : String]) #:transparent)
(struct CloV
  ([args : (Listof Symbol)] [body : ExprC] [env : Environment]) #:transparent)
(struct BoolV
  ([bool : Boolean]) #:transparent)
(struct PrimV
  ([op : Symbol]) #:transparent)

(define-type Environment (U (Listof Bind)))
(struct Bind
  ([name : Symbol] [val : Value]) #:transparent)

;; Top Level Enviroment, a list of bindings
(define top-env
  (list (Bind '+ (PrimV '+))
        (Bind '- (PrimV '-))
        (Bind '* (PrimV '*))
        (Bind '/ (PrimV '/))
        (Bind '<= (PrimV '<=))
        (Bind 'equal? (PrimV 'equal?))
        (Bind 'error (PrimV 'error))
        (Bind 'true (BoolV #t))
        (Bind 'false (BoolV #f))))

;; This function takes on input an s-expression and returns the string
;; value of the s-expression passed in
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;; This function takes on an ExprC and an Enviroment, a list of bindings, and
;; returns a Value
(define (interp [ast : ExprC] [env : Environment]) : Value
  (match ast
    [(NumC number) (NumV number)]
    [(IdC variable) (lookup variable env)]
    [(StrC string) (StrV string)]
    [(IfC condition then else-body) (match (interp condition env)
                            [(BoolV bool) (cond
                                            [bool (interp then env)]
                                            [else (interp else-body env)])]
                                      [other (error 'interp "OAZO Interp - Condition was not a boolean")])]
    [(LamC ids body) (CloV ids body env)]
    [(AppC func args) (match (interp func env)
                        [(CloV ids body env2)
                         (cond
                           [(= (length ids) (length args))
                            (interp body (extend-environment (interp-args ids args env) env2))]
                           [else
                            (error 'interp "OAZO Interp - Wrong number of params ~e to args ~e" ids args)])]
                        [(PrimV op) (prim-op-function op args env)]
                        [other (error 'interp "OAZO Interp - Invalid AppC")])]))

;; This function takes on input an s-expression and returns the ExprC
;; equivalent to that s-expression
(define (parse [symbol : Sexp]) : ExprC
  (match symbol
    [(? real? num) (NumC num)]
    [(? symbol? id) (cond
                      [(valid-id? id) (IdC id)]
                      [else (error 'parse "OAZO Parse - Invalid Id: ~e" id)])]
    [(? string? str) (StrC str)]
    [(list 'if cond 'then then 'else else) (IfC (parse cond) (parse then) (parse else))]
    [(list 'let (list (? valid-id? (? symbol? a)) '<- b)... body)
     (cond
       [(duplicate-parameters (cast a (Listof Symbol)))
        (error 'parse "OAZO Parse - Duplicate Ids: ~e" a)]
       [else (AppC (LamC (cast a (Listof Symbol))
                         (parse body))
                   (map parse (cast b (Listof Sexp))))])]
    [(list 'anon (list (? valid-id?(? symbol? ids)) ...) ': body)
     (cond
       [(duplicate-parameters (cast ids (Listof Symbol)))
        (error 'parse "OAZO Parse - Duplicate Ids: ~e" ids)]
       [else (LamC (cast ids (Listof Symbol)) (parse body))])]
    [(list body args ...) (AppC (parse body) (map parse args))]
    [other (error 'parse "OAZO Parse - Invalid Input: ~e" other)]))

;; This function takes on input a Value and returns the string version of it
(define (serialize [value : Value]) : String
  (match value
    [(NumV num) (~v num)]
    [(StrV str) (~v str)]
    [(BoolV bool) (cond
                    [bool "true"]
                    [else "false"])]
    [(CloV id body env) "#<procedure>"]
    [(PrimV op) "#<primop>"]))

;; This function takes on input a symbol and an environment, and returns
;; the value of the symbol if its in the environment. error if not
(define (lookup [symbol : Symbol] [env : Environment]) : Value
  (match env
    ['() (error 'lookup "OAZO Lookup - Variable not found in environment: ~a" symbol)]
    [(cons f r) (cond
                  [(equal? symbol (Bind-name f)) (Bind-val f)]
                  [else (lookup symbol r)])]))

;; This function takes on a a list of symbols, a list of ExprC and an Environment,
;; and adds those ExprC to the Environment as a list of bindings
(define (interp-args [args : (Listof Symbol)] [values : (Listof ExprC)] [env : Environment]) : Environment
  (match args
    ['() '()]
    [(cons fa ra) (match values
                    ['()
                     (error 'parse-prog "OAZO Add-Environment -
Wrong amount of parameters and arguments ~e ~e" args values)]
                    [(cons fv rv) (cons (Bind fa (interp fv env)) (interp-args ra rv env))])]))

;; This function extends the closure's environment
(define (extend-environment [args : Environment] [env : Environment]) : Environment
  (match args
    ['() env]
    [(cons f r) (cons f (extend-environment r env))]))

;; This function takes on input a symbol from a PrimV and a list of ExprC and
;; returns the value of the built in function
(define (prim-op-function [op : Symbol] [args : (Listof ExprC)] [env : Environment]) : Value
  (match args
    [(list l r) (let ([left (interp l env)] [right (interp r env)])
                  (match op
                    ['+ (NumV (+ (extract-numv left) (extract-numv right)))]
                    ['- (NumV (- (extract-numv left) (extract-numv right)))]
                    ['* (NumV (* (extract-numv left) (extract-numv right)))]
                    ['/ (cond
                          [(equal? (extract-numv right) 0)
                           (error 'prim-op-function "OAZO Prim-Op-Function - Divide by Zero")]
                          [else (NumV (/ (extract-numv left) (extract-numv right)))])]
                    ['<= (BoolV (<= (extract-numv left) (extract-numv right)))]
                    ['equal? (BoolV (equal? left right))]
                    [other
                     (error 'prim-op-function "OAZO Prim-Op-Function -
Not a PrimOp symbol that takes two arguments ~e" other)]))]
    [(list err) (match op
                  ['error (error 'prim-op-function "OAZO user-error ~v" (serialize (interp err env)))]
                  [other (error 'prim-op-function "OAZO Prim-Op-Function -
Not a PrimOp symbol that takes one argument ~e" other)])]
    [other (match op
             ['true (BoolV #t)]
             ['false (BoolV #f)]
             [other (error 'prim-op-function "OAZO Prim-Op-Function -
Wrong args for any PrimOp ~e" other)])]))

;; This function takes on input a NumV and extracts the real number from it to return
(define (extract-numv [num : Value]) : Real
  (match num
    [(NumV number) number]
    [other (error 'extract-numv "OAZO Extract-NumV - Not a NumV ~e" other)]))

;; This function takes on a list of symbols and returns true if there is a duplicate
;; in the list
(define (duplicate-parameters [sym-lst : (Listof Symbol)]) : Boolean
  (match sym-lst
    ['() #f]
    [(cons f r) (cond
                  [(member f r) #t]
                  [else (duplicate-parameters r)])]))

;; This function takes on input an s-expression and makes sure that it is
;; a valid id in the concrete syntax
(define (valid-id? [symbol : Sexp]) : Boolean
  (not (or (equal? symbol 'if)
           (equal? symbol 'then)
           (equal? symbol 'else)
           (equal? symbol 'let)
           (equal? symbol 'anon)
           (equal? symbol ':)
           (equal? symbol '<-))))

;; Unit Tests
(check-equal? (top-interp '10) "10")
(check-equal? (top-interp '{{anon {x y} : {+ x y}} 5 7}) "12")
(check-equal? (top-interp '{let [x <- 5] [y <- 7] {+ x y}}) "12")
(check-equal? (top-interp '{if true then "one" else "two"}) "\"one\"")
(check-equal? (top-interp '{{anon {compose add1} : {{anon {add2} : {add2 99}}
                                                    {compose add1 add1}}}
                            {anon {f g} : {anon {x} : {f {g x}}}}
                            {anon {x} : {+ 1 x}}}) "101")
(check-equal? (top-interp '{let [fact <- {anon {self n} : {if {<= n 0}
                                                              then 1
                                                              else {* n {self self {- n 1}}}}}]
                             {fact fact 12}}) "479001600")
(check-equal? (top-interp '{let [pow <- {anon {self base exponent} :
                                              {if {<= exponent 1}
                                                  then base
                                                  else {* base {self self base (- exponent 1)}}}}]
                             {pow pow 5 5}}) "3125")

;; interp
(check-equal? (interp (NumC 10) (list (Bind 'x (NumV 10)))) (NumV 10))
(check-equal? (interp (IdC 'x) (list (Bind 'x (NumV 10)))) (NumV 10))
(check-equal? (interp (IfC (AppC (IdC 'equal?)
                                 (list (NumC 5) (NumC 5)))
                           (NumC 0)
                           (NumC 1))
                      top-env) (NumV 0))
(check-equal? (interp (IfC (AppC (IdC 'equal?)
                                 (list (NumC 5) (NumC 10)))
                           (NumC 0)
                           (NumC 1))
                      top-env) (NumV 1))
(check-equal? (interp (LamC '() (NumC 5)) top-env) (CloV '() (NumC 5) top-env))
(check-equal? (interp (LamC '(x y) (AppC (IdC '+)
                                         (list (IdC 'x) (IdC 'y))))
                      (cons (Bind 'x (NumV 2)) (cons (Bind 'y (NumV 3))top-env)))
              (CloV '(x y)
                    (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))
                    (cons (Bind 'x (NumV 2)) (cons (Bind 'y (NumV 3))top-env))))
(check-equal? (interp (AppC (IdC '+)
                            (list (NumC 5) (NumC 5)))
                      top-env)
              (NumV 10))
(check-equal? (interp (AppC (LamC '(x y) (AppC (IdC '+)
                                               (list (IdC 'x) (IdC 'y))))
                            (list (NumC 5) (NumC 5)))
                      top-env)
              (NumV 10))
(check-equal? (interp (AppC (LamC '(add1) (AppC (IdC 'add1) (list (NumC 7))))
                            (list (LamC '(x)
                                        (AppC (IdC '+) (list (IdC 'x) (NumC 1))))))
                      top-env)
              (NumV 8))

(check-equal? (interp (AppC (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
                            (list (NumC 5) (NumC 7)))
                      top-env)
              (NumV 12))
(check-exn (regexp (regexp-quote "OAZO Interp - Condition was not a boolean"))
           (lambda () (interp (IfC (AppC (IdC '+)
                                         (list (NumC 5) (NumC 5)))
                                   (NumC 0)
                                   (NumC 1))
                              top-env)))
(check-exn (regexp (regexp-quote "OAZO Interp - Wrong number of params '() to args (list (NumC 17))"))
           (lambda () (interp (AppC (LamC '() (NumC 9))
                                    (list (NumC 17)))
                              top-env)))
(check-exn (regexp (regexp-quote "OAZO Interp - Invalid AppC"))
           (lambda () (interp (AppC (NumC 5)
                                    (list (NumC 5) (NumC 5)))
                              top-env)))

;; parse
(check-equal? (parse '10) (NumC 10))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse '"hello") (StrC "hello"))
(check-equal? (parse '(if 5 then 10 else 15))
              (IfC (NumC 5) (NumC 10) (NumC 15)))
(check-equal? (parse '{anon {x} : {+ x 1}})
              (LamC '(x) (AppC (IdC '+) (list (IdC 'x) (NumC 1)))))
(check-equal? (parse '{{anon {x y} : {+ x y}} 5 7})
              (AppC (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
                    (list (NumC 5) (NumC 7))))
(check-equal? (parse '{let [x <- 5] [y <- 7] {+ x y}})
              (AppC (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
                    (list (NumC 5) (NumC 7))))
(check-equal? (parse '{{anon {add1} : {add1 7}} {anon {x} : {+ x 1}}})
              (parse '{let [add1 <- {anon {x} : {+ x 1}}] {add1 7}}))
(check-equal? (parse '{{anon {f} :
                             {{anon {y} : {+ y 7}}
                              {f 2}}}
                       {anon {x} : {+ x 1}}})
              (parse '{let [f <- {anon {x} : {+ x 1}}]
                        {let [y <- {f 2}]
                          {+ y 7}}}))
(check-exn (regexp (regexp-quote "OAZO Parse - Invalid Id: 'if"))
           (lambda () (parse 'if)))
(check-exn (regexp (regexp-quote "OAZO Parse - Invalid Id: 'then"))
             (lambda () (parse '(+ then 4))))
(check-exn (regexp (regexp-quote "OAZO Parse - Duplicate Ids: '(x x)"))
           (lambda () (parse '{anon {x x} : {+ x x}})))
(check-exn (regexp (regexp-quote "OAZO Parse - Duplicate Ids: '(z z)"))
             (lambda () (parse '(let (z <- (anon () : 3)) (z <- 9) (z)))))
(check-exn (regexp (regexp-quote "OAZO Parse - Invalid Input: '()"))
             (lambda () (parse '())))

;; serialize
(check-equal? (serialize (NumV 10)) "10")
(check-equal? (serialize (StrV "Hello")) "\"Hello\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV '(x) (NumC 10) top-env)) "#<procedure>")
(check-equal? (serialize (CloV '(x y)
                               (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))
                               (cons (Bind 'x (NumV 2)) (cons (Bind 'y (NumV 3))top-env)))) "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (PrimV 'equal?)) "#<primop>")

;; lookup
(check-equal? (lookup 'x (list (Bind 'x (NumV 10)))) (NumV 10))
(check-equal? (lookup 'x (list (Bind 'y (NumV 5)) (Bind 'x (NumV 10)))) (NumV 10))
(check-exn (regexp (regexp-quote "OAZO Lookup - Variable not found in environment: x"))
           (lambda () (lookup 'x (list (Bind 'y (NumV 10)) (Bind 'z (NumV 10))))))

;; interp-args
(check-equal? (interp-args '() '() '()) '())
(check-equal? (interp-args '(x) (list (NumC 5)) '())
              (list (Bind 'x (NumV 5))))
(check-equal? (interp-args '(x) (list (NumC 5)) (list (Bind 'y (NumV 5))))
              (list (Bind 'x (NumV 5))))
(check-equal? (interp-args '(x y) (list (NumC 5) (NumC 10)) (list (Bind 'z (NumV 5))))
              (list (Bind 'x (NumV 5)) (Bind 'y (NumV 10))))
(check-exn (regexp (regexp-quote "OAZO Add-Environment -
Wrong amount of parameters and arguments '(y) '()"))
           (lambda () (interp-args '(x y) (list (NumC 5)) (list (Bind 'y (NumV 5))))))

;; extend environment
(check-equal? (extend-environment '() top-env) top-env)
(check-equal? (extend-environment (list (Bind 'x (NumV 10))) top-env) (cons (Bind 'x (NumV 10))top-env))

;; prim-op-function
(check-equal? (prim-op-function '+ (list (NumC 5) (NumC 5)) top-env) (NumV 10))
(check-equal? (prim-op-function '- (list (NumC 5) (NumC 5)) top-env) (NumV 0))
(check-equal? (prim-op-function '* (list (NumC 5) (NumC 5)) top-env) (NumV 25))
(check-equal? (prim-op-function '/ (list (NumC 5) (NumC 5)) top-env) (NumV 1))
(check-equal? (prim-op-function '<= (list (NumC 5) (NumC 10)) top-env) (BoolV #t))
(check-equal? (prim-op-function '<= (list (NumC 10) (NumC 5)) top-env) (BoolV #f))
(check-equal? (prim-op-function 'equal? (list (NumC 5) (NumC 5)) top-env) (BoolV #t))
(check-equal? (prim-op-function 'equal? (list (NumC 5) (NumC 10)) top-env) (BoolV #f))
(check-equal? (prim-op-function 'equal? (list (StrC "hello") (StrC "hello")) top-env) (BoolV #t))
(check-equal? (prim-op-function 'equal? (list (StrC "hello") (StrC "bye")) top-env) (BoolV #f))
(check-equal? (prim-op-function 'true '() top-env) (BoolV #t))
(check-equal? (prim-op-function 'false '() top-env) (BoolV #f))
(check-exn (regexp (regexp-quote "OAZO user-error"))
           (lambda () (prim-op-function 'error (list (StrC "1234")) top-env)))
(check-exn (regexp (regexp-quote "OAZO Prim-Op-Function -
Not a PrimOp symbol that takes one argument 'err"))
           (lambda () (prim-op-function 'err (list (StrC "1234")) top-env)))
(check-exn (regexp (regexp-quote "OAZO Prim-Op-Function - Divide by Zero"))
           (lambda () (prim-op-function '/ (list (NumC 5) (NumC 0)) top-env)))
(check-exn (regexp (regexp-quote "OAZO Prim-Op-Function -
Not a PrimOp symbol that takes two arguments '%"))
           (lambda () (prim-op-function '% (list (NumC 5) (NumC 5)) top-env)))
(check-exn (regexp (regexp-quote "OAZO Prim-Op-Function -
Not a PrimOp symbol that takes two arguments 'true"))
           (lambda () (prim-op-function 'true (list (NumC 5) (NumC 5)) top-env)))
(check-exn (regexp (regexp-quote "OAZO Prim-Op-Function -
Not a PrimOp symbol that takes two arguments 'false"))
           (lambda () (prim-op-function 'false (list (NumC 5) (NumC 5)) top-env)))
(check-exn (regexp (regexp-quote "OAZO Prim-Op-Function -
Wrong args for any PrimOp '&"))
           (lambda () (prim-op-function '& (list ) top-env)))

;; extract-numv
(check-equal? (extract-numv (NumV 0)) 0)
(check-equal? (extract-numv (NumV 5)) 5)
(check-equal? (extract-numv (NumV -5)) -5)
(check-equal? (extract-numv (NumV 5.5)) 5.5)
(check-exn (regexp (regexp-quote "OAZO Extract-NumV - Not a NumV (BoolV #t)"))
           (lambda () (extract-numv (BoolV #t))))

;; duplicate-parameters
(check-equal? (duplicate-parameters '()) #f)
(check-equal? (duplicate-parameters '(x)) #f)
(check-equal? (duplicate-parameters '(x y z)) #f)
(check-equal? (duplicate-parameters '(x x)) #t)
(check-equal? (duplicate-parameters '(x x y)) #t)
(check-equal? (duplicate-parameters '(x y x)) #t)

;; valid-id
(check-equal? (valid-id? '+) #t)
(check-equal? (valid-id? '*) #t)
(check-equal? (valid-id? 'x) #t)
(check-equal? (valid-id? 'if) #f)
(check-equal? (valid-id? 'then) #f)
(check-equal? (valid-id? 'else) #f)
(check-equal? (valid-id? 'let) #f)
(check-equal? (valid-id? 'anon) #f)
(check-equal? (valid-id? ':) #f)
(check-equal? (valid-id? '<-) #f)