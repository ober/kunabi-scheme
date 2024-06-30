#lang racket

(require json
         rnrs/io/ports-6
         file/gunzip)

(define (read-json-gzip file-path)
  (call-with-input-file file-path
    (lambda (in-port)
      ;; Decompress the gzip data into a bytevector
      (define output-bytes
        (let ([output (open-output-bytes)])
          (gunzip-through-ports in-port output)
          (get-output-bytes output)))
      ;; Read JSON from the decompressed bytevector
      (define input-bytes (open-input-bytes output-bytes))
      (read-json input-bytes))
    #:mode 'binary))

(define (json-gz-file? file-name)
  (regexp-match? #rx"^.*\\.json\\.gz$" file-name))

(define (walk-directory dir)
  (displayln  "walk-directory")
  (define results '())
  (for-each
   (lambda (entry)
     (define full-path (build-path dir entry))
     (cond
       ((and (directory-exists? full-path)
             (not (member entry '("." ".."))))
        (set! results (append results (walk-directory full-path))))
       ((json-gz-file? entry)
        (set! results (cons (format "~a/~a" dir entry) results)))))
   (directory-list dir))
  results)

(define (ct dir)
  (define files (walk-directory dir))
  (for-each
   (lambda (file)
     (let* ((data (read-json-gzip file))
            (records (hash-ref data 'Records)))
       (for-each
        (lambda (record)
          (displayln (format "~a ~a" (hash-ref record 'requestID)
                                        (hash-ref record 'eventName))))
        records)))
     files))

  (define (type-of x)
    (cond
      ((procedure? x) 'procedure)
      ((number? x) 'number)
      ((string? x) 'string)
      ((symbol? x) 'symbol)
      ((pair? x) 'pair)
      ((vector? x) 'vector)
      ((input-port? x) 'port)
      ((output-port? x) 'port)
      ((boolean? x) 'boolean)
      ((char? x) 'char)
      ((hash? x) 'hash)
      ;;((bytevector? x) 'bytevector)
      ((null? x) 'null)
      (else 'unknown)))

  (ct "/home/user/bench2")
