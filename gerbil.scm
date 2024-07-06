(import
  :gerbil/gambit
  :std/actor
  :std/debug/heap
  :std/debug/memleak
  :std/format
  :std/generic/dispatch
  :std/getopt
  :std/iter
  :std/logger
  :std/misc/list
  :std/misc/lru
  :std/net/address
  :std/pregexp
  :std/srfi/1
  :std/srfi/95
  :std/sugar
  :std/text/json
  :std/text/zlib
  )

(export #t)

(def (main)
  (let ((files (find-ct-files "/home/user/bench")))
    (for-each
      (lambda (file)
        (let (json (read-ct-file file))
          (if (hash-table? json)
            (let-hash json
              (displayln .requestID
                         .eventName)))))
      files)))

(def (find-ct-files dir)
  (find-files
   dir
   (lambda (filename)
     (and (equal? (path-extension filename) ".gz")
	      (not (equal? (path-strip-directory filename) ".gz"))))))


(def (read-ct-file file)
  (let ((btime (time->seconds (current-time)))
	    (count 0))
    (call-with-input-file file
	  (lambda (file-input)
	    (let ((mytables (load-ct-file file-input)))
          (for-each
	        (lambda (row)
              (set! count (+ count 1))
              ;;(displayln row))
              )
	        mytables))))

    (let ((delta (- (time->seconds (current-time)) btime)))
        (displayln
         "rps: " (float->int (/ count delta ))
         " size: " count
         " delta: " delta
	       " file: " file
	       ))))

(def (load-ct-file file)
  (parameterize ((read-json-key-as-symbol? #t))
    (hash-ref
     (read-json
      (open-input-string
       (utf8->string
        (uncompress file))))
     'Records)))

(defalias λ lambda)

(def (find-files path
		         (pred? true)
		         recurse?: (recurse? true)
		         follow-symlinks?: (follow-symlinks? #f))
  (with-list-builder
   (collect!)
   (walk-filesystem-tree! path
                          (λ (file) (when (pred? file) (collect! file)))
                          recurse?: recurse?
                          follow-symlinks?: follow-symlinks?)))

(def (walk-filesystem-tree!
      path
      visit
      recurse?: (recurse? true)
      follow-symlinks?: (follow-symlinks? #f))
  (visit path)
  (when (and (path-is-directory? path follow-symlinks?)
	         (recurse? path))
    (for-each!
     (directory-files path)
     (lambda (name) (walk-filesystem-tree!
		        (path-expand name path) visit
		        recurse?: recurse? follow-symlinks?: follow-symlinks?)))))

(def (path-is-directory? path (follow-symlinks? #f))
  (equal? 'directory (file-info-type (file-info path follow-symlinks?))))

(def (float->int num)
  (inexact->exact
   (round num)))
