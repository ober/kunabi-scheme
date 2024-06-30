(import (chicken base)
        (chicken io)
        (chicken file)
        (chicken file posix)
        (chicken process-context)
        (chicken string)
        (format)
        (srfi 18)
        (json)
        (regex)
        (z3))

;; Function to decompress a gzipped file and return the contents as a string
(define (read-json-gzip file-path)
         (json-read (z3:open-compressed-input-file file-path)))

;; Function to check if a file has a .json.gz extension
(define (json-gz-file? file-name)
  (string-match "^.*\\.json\\.gz$" file-name))

;; Function to walk a directory tree and return list of files matching json.gz
(define (walk-directory dir)
  (print "walk-directory")
  (let ((entries (directory dir))
        (results (list)))
    (for-each
     (lambda (entry)
       (let ((full-path (conc dir "/" entry)))
         (cond
          ((and (directory? full-path)
                (not (or (string=? entry ".") (string=? entry ".."))))
           (walk-directory full-path))
          ((json-gz-file? entry)
           (set! results (cons (format "~a/~a" dir entry) results))))))
     entries)
    results))

(define (ct dir)
  (let ((files (walk-directory dir)))
    (for-each
     (lambda (file)
       (let ((whatis (time (read-json-gzip file))))
         (print file)
         (print (type-of whatis))))
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
    ((eq? x '()) 'null) ; Empty list
    ((##sys#slot? x) (##sys#slot-type x)) ; Chicken internal slot
    (else 'unknown)))

(ct "/home/user/bench2")
