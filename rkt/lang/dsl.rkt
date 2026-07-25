#lang racket

;;; Phase 5 — Macro DSL for Actions (`define-action`) and Events (`define-event`).

(require (for-syntax syntax/parse racket/base racket/syntax)
         "../world/state.rkt")

(provide (struct-out custom-action-entry)
         custom-actions-registry
         register-custom-action!
         clear-custom-actions!
         get-custom-actions
         define-action
         define-event)

(struct custom-action-entry (name description params availability-proc handler-proc) #:transparent)

;; Global registry mapping action name string -> custom-action-entry
(define custom-actions-registry (make-parameter (hash)))

(define (register-custom-action! entry)
  (custom-actions-registry
   (hash-set (custom-actions-registry)
             (custom-action-entry-name entry)
             entry)))

(define (clear-custom-actions!)
  (custom-actions-registry (hash)))

(define (get-custom-actions)
  (hash-values (custom-actions-registry)))

;; ---------------------------------------------------------------------------
;; define-action macro
;; ---------------------------------------------------------------------------

(define-syntax (define-action stx)
  (syntax-parse stx
    [(_ name-id:id (~seq kw:keyword val:expr) ...)
     (define kw-table
       (for/hash ([k (in-list (syntax->datum #'(kw ...)))]
                  [v (in-list (syntax->list #'(val ...)))])
         (values k v)))

     (define (get-val kw-name default-expr)
       (hash-ref kw-table kw-name default-expr))

     (define action-name-expr (get-val '#:name #'(symbol->string 'name-id)))
     (define desc-expr (get-val '#:description #'"" ))
     (define params-expr (get-val '#:params #'(hash)))
     (define avail-expr (get-val '#:available #'(λ (w tag) #t)))
     (define handler-expr (get-val '#:handler #'(λ (w tag params prg) (values w '() '() #f))))

     #`(begin
         (define name-id
           (custom-action-entry
            #,action-name-expr
            #,desc-expr
            #,params-expr
            #,avail-expr
            #,handler-expr))
         (register-custom-action! name-id))]))

;; ---------------------------------------------------------------------------
;; define-event macro
;; ---------------------------------------------------------------------------

(define-syntax (define-event stx)
  (syntax-parse stx
    [(_ type-id:id #:fields (field-id:id ...))
     (define make-fn-name (format-id #'type-id "make-~a-event" #'type-id))
     (define pred-fn-name (format-id #'type-id "~a-event?" #'type-id))
     (define field-list (syntax->list #'(field-id ...)))
     (define kv-pairs
       (apply append
              (for/list ([f (in-list field-list)])
                (list #`(quote #,f) f))))

     #`(begin
         (define (#,make-fn-name field-id ...)
           (evt 'type-id (hasheq #,@kv-pairs)))
         (define (#,pred-fn-name e)
           (and (event? e) (equal? (event-type e) 'type-id))))]))
