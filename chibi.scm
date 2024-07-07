(import (scheme base)
        (scheme file)
        (scheme process-context)
        (chibi gzip)
        (chibi json)
        (chibi string)
        (srfi 1)
        (srfi 11)
        (srfi 19)
        (srfi 64)
        (srfi 26)
        (srfi 1))

(define (read-json-gzip file-path)
  (call-with-input-file file-path
    (lambda (in)
      (call-with-gzip-input-port
       in
       (lambda (gzip-port)
         (let ((decompressed (get-bytevector-all gzip-port)))
           (json-string->scm (utf8->string decompressed))))))))

;; Regular expression to match files with .json.gz extension
(define json-gz-regexp ".*\\.json\\.gz$")

;; Function to check if a file has a .json.gz extension
(define (json-gz-file? file-name)
  (regexp-exec (regexp json-gz-regexp) file-name))

;; Function to walk a directory tree and return list of files matching json.gz
(define (walk-directory dir)
  (let loop ((entries (directory-files dir))
             (results '()))
    (for-each
     (lambda (entry)
       (let ((full-path (string-append dir "/" entry)))
         (cond
          ((and (file-exists? full-path)
                (not (member entry '("." ".."))))
           (if (file-directory? full-path)
               (set! results (append results (walk-directory full-path)))
               (if (json-gz-file? entry)
                   (set! results (cons full-path results))))))))
     entries)
    results))

;; Example usage
(define (ct dir)
  (let ((files (walk-directory dir)))
    (for-each
     (lambda (file)
       (let ((json (read-json-gzip file)))
         (for-each
          (lambda (record)
            (let* ((h (alist->hash-table record))
                   (request-id (hash-ref h "requestID"))
                   (event-name (hash-ref h "eventName"))
                   (user-identity (hash-ref h "userIdentity")))
              (displayln (format #f "~a: ~a ~a" request-id event-name user-identity))))
          (vector->list (assoc-ref json "Records")))))
     files)))

(define (type-of x)
  (cond
   ((procedure? x) 'procedure)
   ((number? x) 'number)
   ((string? x) 'string)
   ((symbol? x) 'symbol)
   ((pair? x) 'pair)
   ((vector? x) 'vector)
   ((port? x) 'port)
   ((boolean? x) 'boolean)
   ((char? x) 'char)
   ((bytevector? x) 'bytevector)
   ((null? x) 'null) ; Empty list
   (else 'unknown)))

(define (displayln x)
  (display x)
  (newline))

(ct "/home/user/bench")