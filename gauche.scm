#!/home/linuxbrew/.linuxbrew/bin/gosh
(use rfc.zlib)        ; For gzip compression/decompression
(use gauche.uvector)  ; For bytevector operations
(use binary.io)       ; General I/O operations
(use srfi-19)         ; Time and date library, only if needed
(use rfc.json)        ; JSON parsing
(use file.util)       ; File utilities for directory walking

(define (read-json-gzip file-path)
  (call-with-input-file file-path
    (lambda (in)
      (let* ((gzip-port (open-inflating-port in :window-bits 47))
             (json-string (port->string gzip-port)))
        (parse-json json-string)))))

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

;; Example usage
(define (ct dir)
  (let ((files (walk-directory dir)))
    (for-each
     (lambda (file)
       (let ((json (read-json-gzip file)))
         (displayln (type-of json))))
         ;; (for-each
         ;;  (lambda (record)
         ;;    (let ((request-id (gethash "requestID" record))
         ;;          (event-name (gethash "eventName" record))
         ;;          (user-identity (gethash "userIdentity" record)))
         ;;      (print (format "~a: ~a ~a\n" request-id event-name user-identity))))
         ;;  (cdr (assoc "Records" json)))))
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

(ct "/home/user/bench2")
