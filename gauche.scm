#!/home/linuxbrew/.linuxbrew/bin/gosh
(use rfc.zlib)
(use gauche.uvector)
(use binary.io)
(use srfi-19)
(use rfc.json)
(use file.util)
;;(use scheme.hash-table)

(define (read-json-gzip file-path)
  (call-with-input-file file-path
    (lambda (in)
      (let* ((gzip-port (open-inflating-port in :window-bits 47))
             (json-string (port->string gzip-port)))
        (time (parse-json* json-string))))))

;; Function to check if a file has a .json.gz extension
(define (json-gz-file? file-name)
  (rxmatch ".*\\.json\\.gz$" file-name))

;; Function to walk a directory tree and return list of files matching json.gz
(define (walk-directory dir)
  (let loop ((entries (directory-list dir))
             (results '()))
    (for-each
     (lambda (entry)
       (let ((full-path (string-append dir "/" entry)))
         (cond
          ((and (file-exists? full-path)
                (not (member entry '("." ".."))))
           (if (file-is-directory? full-path)
               (set! results (append results (walk-directory full-path)))
               (if (json-gz-file? entry)
                   (set! results (cons full-path results))))))))
     entries)
    results))

(define (ct dir)
  (let ((files (walk-directory dir)))
    (for-each
     (lambda (file)
       (let ((a (read-json-gzip file)))
         (for-each
          (lambda (record)
            (let ((request (assoc-ref record "requestID"))
                  (event-name (assoc-ref record "eventName")))
              (displayln (format "~a ~a" request event-name))))
          (vector->list (assoc-ref (car a) "Records")))))
     files)))

(define (type-of x)
  (cond
   ((hash-table? x) 'hash-table)
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
