#lang racket

;;; SPEC §9 — JSONL event log: the source of truth. One JSON object per line,
;;; every record {v: 1, ts: int, type: string, ...}. Line-buffered so a crash
;;; loses at most the current line.

(require json
         racket/file
         racket/path
         "../world/state.rkt")

(provide (struct-out event-log)
         open-event-log
         log-event!
         close-event-log)

(struct event-log (path out) #:transparent)

(define (open-event-log path)
  (define p (if (string? path) (string->path path) path))
  (define dir (path-only p))
  (when dir (make-directory* dir))
  (define out (open-output-file p #:exists 'append))
  (file-stream-buffer-mode out 'line)
  (event-log p out))

(define (sanitize-json-val v)
  (cond
    [(symbol? v) (symbol->string v)]
    [(hash? v)
     (for/hasheq ([(k val) (in-hash v)])
       (values (if (symbol? k) k (string->symbol (format "~a" k)))
               (sanitize-json-val val)))]
    [(list? v) (map sanitize-json-val v)]
    [(pos? v) (hasheq 'x (pos-x v) 'y (pos-y v))]
    [else v]))

(define (log-event! log step-ts evt-or-data [extra-type #f])
  (define out (event-log-out log))
  (define record
    (cond
      [(event? evt-or-data)
       (define type-str (symbol->string (event-type evt-or-data)))
       (define data (sanitize-json-val (event-data evt-or-data)))
       (hash-set (if (hash? data) data (hasheq 'data data))
                 'type type-str)]
      [(hash? evt-or-data)
       (define data (sanitize-json-val evt-or-data))
       (if extra-type
           (hash-set data 'type (if (symbol? extra-type) (symbol->string extra-type) extra-type))
           data)]
      [else (hasheq 'data (sanitize-json-val evt-or-data))]))

  (define full-record
    (hash-set (hash-set record 'v 1) 'ts step-ts))

  (write-json full-record out)
  (newline out)
  (flush-output out))

(define (close-event-log log)
  (when (event-log-out log)
    (flush-output (event-log-out log))
    (close-output-port (event-log-out log))))
