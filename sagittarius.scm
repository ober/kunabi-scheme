;; Import necessary libraries
(import (sagittarius)
        (sagittarius regex)      ; For regular expression matching
        (text json)       ; For JSON parsing
        (rnrs)
        (rnrs bytevectors)       ; For binary data handling
        (binary data)     ; For gzip decompression
        (rfc zlib))      ; Gzip functionality

;; Function to decompress a gzipped JSON file and parse it
(define (read-json-gzip file-path)
  (call-with-input-file file-path
    (lambda (bin-port)
      (let* ((gzip-port (open-inflating-input-port bin-port))
             (json-text (read-all gzip-port)))
        (json-string->object json-text)))))

;; Function to check if a file has a .json.gz extension
(define (json-gz-file? file-name)
  (let ((pattern (regex ".*\\.json\\.gz$")))
    (looking-at pattern file-name)))

;; Function to walk a directory tree and return list of files matching json.gz
(define (walk-directory dir)
  (display "walk-directory\n")
  (let loop ((entries (read-directory dir))
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
         (display file)
         (newline)
         (display json)  ;; Displaying JSON object, adjust if needed
         (newline)))
     files)))

(ct "/home/user/bench2")
