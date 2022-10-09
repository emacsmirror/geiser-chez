(library (geiser)
  (export geiser:eval
          geiser:completions
          geiser:module-completions
          geiser:autodoc
          geiser:no-values
          geiser:load-file
          geiser:newline
          geiser:macroexpand)
  (import (chezscheme))

  (define-syntax as-string
    (syntax-rules () ((_ b ...) (with-output-to-string (lambda () b ...)))))

  (define (write-to-string x) (as-string (write x)))
  (define (pretty-string x) (as-string (pretty-print x)))

  (define (call-with-result thunk)
    (let ((output-string (open-output-string)))
      (write
       (call/cc
        (lambda (k)
          (with-exception-handler
              (lambda (e)
                (debug-condition e)     ; save the condition for the debugger
                (k `((result "")
                     (output . ,(get-output-string output-string))
                     (error (key . condition)
                            (msg . ,(as-string (display-condition e)))))))
            (lambda ()
              (call-with-values
                  (lambda ()
                    (parameterize ((current-output-port output-string)) (thunk)))
                (lambda result
                  `((result ,(pretty-string
                              (if (null? (cdr result)) (car result) result)))
                    (output . ,(get-output-string output-string))))))))))
      (newline)
      (close-output-port output-string)))

  (define (last-index-of str-list char idx last-idx)
    (if (null? str-list)
        last-idx
        (last-index-of (cdr str-list) char (+ 1 idx)
                       (if (char=? char (car str-list)) idx last-idx))))

  (define (obj-file-name name)
    (let ((idx (last-index-of (string->list name) #\. 0 -1)))
      (if (= idx -1)
          (string-append name ".so")
          (string-append (substring name 0 idx) ".so"))))

  (define (geiser:load-file filename)
    (let ((output-filename (obj-file-name filename)))
      (call-with-result
       (lambda ()
         (with-output-to-string
           (lambda () (maybe-compile-file filename output-filename)))
         (load output-filename)))))

  (define string-prefix?
    (lambda (x y)
      (let ([n (string-length x)])
        (and (fx<= n (string-length y))
             (let prefix? ([i 0])
               (or (fx= i n)
                   (and (char=? (string-ref x i) (string-ref y i))
                        (prefix? (fx+ i 1)))))))))

  (define (geiser:completions prefix . rest)
    (sort string-ci<?
          (filter (lambda (el)
                    (string-prefix? prefix el))
                  (map write-to-string
                       (environment-symbols (interaction-environment))))))

  (define (geiser:eval module form)
    (call-with-result
     (lambda () (if module (eval form (environment module)) (eval form)))))

  (define (geiser:module-completions prefix . rest)
    (define (substring? s1 s2)
      (let ([n1 (string-length s1)] [n2 (string-length s2)])
        (let loop2 ([i2 0])
          (let loop1 ([i1 0] [j i2])
            (if (fx= i1 n1)
                i2
                (and (not (fx= j n2))
                     (if (char=? (string-ref s1 i1) (string-ref s2 j))
                         (loop1 (fx+ i1 1) (fx+ j 1))
                         (loop2 (fx+ i2 1)))))))))
    (filter (lambda (el)
              (substring? prefix el))
            (map write-to-string (library-list))))

  (define (arity->parameter-list p)
    (define (nparams n)
      (map (lambda (n) (string->symbol (format "x~a" n))) (iota n)))
    (define (add-opt pl)
      (cons (append (if (null? pl) '() (car pl)) '(...)) pl))
    (let* ((m (procedure-arity-mask p))
           (pm (if (< m 0) (+ 1 (lognot m)) m))
           (n (if (> pm 0) (/ (log pm) (log 2)) 0)))
      (let loop ((k 1) (pl '()))
        (cond ((> k n) (reverse (if (< m 0) (add-opt pl) pl)))
              ((logbit? k pm) (loop (+ k 1) (cons (nparams k) pl)))
              (else (loop (+ k 1) pl))))))

  (define (source->parameter-list p)
    ;; same as (inspect object), then hitting c
    (let* ((s (((inspect/object p) 'code) 'source))
           (form (and s (s 'value))))
      (and (list? form)
           (>= (length form) 2)
           (case (car form)
             [(lambda) (list (cadr form))]
             [(case-lambda) (map car (cdr form))]
             [(record-predicate record-accessor)
              (list (list (record-type-name (cadr (cadr form)))))]
             [(record-mutator)
              (let ([rtd (cadr (cadr form))]
                    [field-idx (caddr form)])
                (list (list (record-type-name rtd)
                            (vector-ref (record-type-field-names rtd)
                                        field-idx))))]
             [(record-constructor)
              (let* ([rcd (cadr (cadr form))]
                     [rtd (((inspect/object rcd) 'ref 'rtd) 'value)])
                (list (vector->list (record-type-field-names rtd))))]
             [else #f]))))

  (define (value->string x)
    (define max-len 80)
    (define sub-str "...")
    (define sub-len (- max-len (string-length sub-str)))
    (let* ((s (write-to-string x))
           (l (string-length s)))
      (if (<= l max-len) s (string-append (substring s 0 sub-len) sub-str))))

  (define not-found (gensym))

  (define (try-eval sym)
    (call/cc
     (lambda (k)
       (with-exception-handler (lambda (e) (k not-found))
         (lambda () (eval sym))))))

  (define (operator-arglist operator)
    (define (procedure-parameter-list p)
      (and (procedure? p)
           (or (source->parameter-list p)
               (arity->parameter-list p))))
    (define (autodoc-arglist* args req)
      (cond ((null? args) (list (list* "required" (reverse req))))
            ((pair? args) (autodoc-arglist* (cdr args) (cons (car args) req)))
            (else `(("required" . ,(reverse req))
                    ("optional" ,args)))))
    (define (autodoc-arglist arglist) (autodoc-arglist* arglist '()))
    (let ([binding (try-eval operator)])
      (if (not (eq? binding not-found))
          (let ([arglists (procedure-parameter-list binding)])
            (if arglists
                `(,operator ("args" ,@(map autodoc-arglist arglists)))
                `(,operator ("value" . ,(value->string binding)))))
          '())))

  (define (geiser:autodoc ids)
    (cond ((null? ids) '())
          ((not (list? ids)) (geiser:autodoc (list ids)))
          ((not (symbol? (car ids))) (geiser:autodoc (cdr ids)))
          (else (map operator-arglist ids))))

  (define (geiser:no-values) #f)

  (define (geiser:newline) #f)

  (define (geiser:macroexpand form . rest)
    (with-output-to-string
      (lambda () (pretty-print (syntax->datum (expand form)))))))
