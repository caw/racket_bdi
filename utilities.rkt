#lang racket

;;
;; A transliteration of the code in Paul Graham's ANSI Common Lisp
;; 


(define rules (make-hash))

; not quite, but nearly
(define (atom? expr)
  (not (pair? expr)))

(define (sublis alist tree)
  (if (pair? tree)
      (cons (sublis alist (car tree))
            (sublis alist (cdr tree)))
      (if (assv tree alist)
          (cdr (assv tree alist))
          tree)))

(define (match x y [binds '()])
    (cond
    ((eqv? x y) (values binds #t))
    ((assoc x binds) (match (binding x binds) y binds))
    ((assoc y binds) (match x (binding y binds) binds))
    ((var? x) (values (cons (cons x y) binds) #t))
    ((var? y) (values (cons (cons y x) binds) #t))
    (else
     (if (and (pair? x) (pair? y))
         (let-values (((b2 success) (match (car x) (car y) binds)))
           (if success
               (match (cdr x) (cdr y) b2)
               (values binds #f)))
         (values binds #f)))))
                                 
(define (var? x)
  (and (symbol? x)
       (char=? (string-ref (symbol->string x) 0) #\?)))

(define (binding x binds)
  (let ((b (assoc x binds)))
    (if b
        (or (binding (cdr b) binds)
            (cdr b))
        #f)))


(match '(p a b c a) '(p ?x ?y c ?x))
(match '(p ?x b ?y a) '(p ?y b c a))
(match '(a b c) '(a a a))
(match '(p ?x) '(p ?x))
(match '(p ?v b ?x d (?z ?z))'(p a ?w c ?y (e e)) '((?v . a) (?w . b)))


;; 1. There will be a cleaner way to do this (capture of global "rules")
;; 2. There will be cleaner way to do optional arguments, instead of 2 entries into the syntax-case form

(define-syntax (<- stx)
  (syntax-case stx ()
     [(_ con)
          (with-syntax ([rules (datum->syntax stx 'rules)])
       #`(let ((k (car 'con))
               (v (cons (cdr 'con) '())))
           (hash-set! rules k (cons v (hash-ref rules k '())))
           (length (hash-ref rules k))))]
    [(_ con ant)
     (with-syntax ([rules (datum->syntax stx 'rules)])
       #`(let ((k (car 'con))
               (v (cons (cdr 'con) 'ant)))
             (hash-set! rules k (cons v (hash-ref rules k '())))
             (length (hash-ref rules k))))]))


(define (prove expr [binds '()])
  (case (car expr)
    ((and) (prove-and (reverse (cdr expr)) binds))
    ((or) (prove-or (cdr expr) binds))
    ((not) (prove-not (cadr expr) binds))
    (else (prove-simple (car expr) (cdr expr) binds))))

(define (prove-simple pred args binds)
  ; pred 'child
  ; args '(?x ?y)
  ; binds ()
  (append-map (lambda (r)
                (let-values (((b2 yes) (match args (car r) binds)))
                  (when yes
                    (if (null? (cdr r))
                          (list b2)
                          (prove (cdr r) b2)))))
              (map change-vars (hash-ref rules pred))))
                  

(define (vars-in expr)
  (if (atom? expr)
      (if (var? expr)
          (list expr)
          '())
      (let ((car-vars (vars-in (car expr)))
            (cdr-vars (vars-in (cdr expr))))
        (remove-duplicates (append car-vars cdr-vars)))))

(define (change-vars expr)
  (sublis (map (lambda (v) (cons v (gensym "?_")))
               (vars-in expr))
          expr))

(define (prove-and clauses binds)
  (if (null? clauses)
      binds
      (append-map (lambda (b)
                    (prove (car clauses) b))
                  (prove-and (cdr clauses) binds))))

(define (prove-or clauses binds)
  (append-map (lambda (c) (prove c binds))
              clauses))

(define (prove-not clause binds)
  (unless (prove clause binds)
    (list binds)))


(writeln "DB assertions:")


(<- (parent donald nancy))
(<- (child ?x ?y) (parent ?y ?x))

(writeln "Testing prove:")

(prove-simple 'parent '(donald nancy) '())
(prove-simple 'child '(?x ?y) '())

