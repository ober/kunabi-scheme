#!/home/linuxbrew/.linuxbrew/bin/gosh

(use rpc.zlib)        ; For gzip compression/decompression
(use gauche.uvector)  ; For bytevector operations
(use binary.io)       ; General I/O operations
(use srfi-19)         ; Time and date library, only if needed
(use rfc.json)        ; JSON parsing
(use file.util)       ; File utilities for directory walking

(define (read-json-gzip file-path)
  (call-with-input-file file-path
    (lambda (in)
      (let* ((gzip-port (zlib-inflate-port in))
             (json-string (port->string gzip-port)))
        (json-string->object json-string)))))

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
         (for-each
          (lambda (record)
            (let ((request-id (gethash "requestID" record))
                  (event-name (gethash "eventName" record))
                  (user-identity (gethash "userIdentity" record)))
              (print (format "~a: ~a ~a\n" request-id event-name user-identity))))
          (cdr (assoc "Records" json)))))
     files)))

(ct "/home/user/bench2")
