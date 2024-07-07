(load "syscalls")
(load "format")

(define (read-json-gzip file-path)
  (let ((command-string (format #f "gzip -dc ~a | jq . -c" file-path)))
    (string->json (system->string command-string))))  ;; Assuming a function system->string and string->json

(define (json-gz-file? file-name)
  (regexp-match "^.*\\.json\\.gz$" file-name))

(define (walk-directory dir)
  (displayln "walk-directory")
  (let ((entries (directory-files dir))
        (results '()))
    (for-each
     (lambda (entry)
       (let ((full-path (string-append dir "/" entry)))
         (cond
           ((and (file-exists? full-path)
                 (not (member entry '("." ".."))))
            (if (directory? full-path)
                (set! results (append results (walk-directory full-path)))
                (if (json-gz-file? entry)
                    (set! results (cons (string-append dir "/" entry) results)))))))
     entries)
    results))

(define (ct dir)
  (let ((files (walk-directory dir)))
    (for-each
     (lambda (file)
       (let ((json (read-json-gzip file)))
         (display file)
         (newline)
         (display (type-of json))
         (newline)))
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
    ((null? x) 'null) ;; Empty list
    (else 'unknown)))

(ct "/home/user/bench2")
