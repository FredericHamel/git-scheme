
(define (git-remote-get-url path)
  (let* ((full-path (path-expand path))
         (pid (open-process (list
                              path: "git"
                              arguments: (list "remote" "get-url" "origin")
                              directory: full-path
                              stdout-redirection: #t
                              stderr-redirection: #f))))
    (and (= (process-status pid) 0)
         (read-line pid))))


(define (path-parent dir)
  (path-directory
    (path-strip-trailing-directory-separator
      dir)))

(define (git-open-ref directory)
  (define (try-open fn)
    (with-exception-handler
      (lambda (exn)
        #f)
      (lambda ()
        (println fn)
        (open-input-file fn))))
  (let loop ((dir directory))
    (or
      (try-open
        (##path-expand
         ".git/packed-refs"
         dir))
      (if (string=? dir "/")
        (error "Reaching filesystem boundary")
        (loop
          (path-parent dir))))))

(define (has-prefix? str prefix)
  (and (string=? str)
       (string=? prefix)
       (let ((len-str (string-length str))
             (len-prefix (string-length prefix)))
         (and (>= len-str len-prefix)
              (string=? prefix
                (substring str 0 len-prefix))
              (substring str len-prefix len-str)))))

(define (parse-git-ref-tags port)
  (read-line port) ;; Ignore first line
  (let loop ((result '())
    (let ((line (read-line port)))
      (if (eof-object? line)
        (begin
          (close-input-port port)
          result)
        (cond
          ((char=? (string-ref line 0)
                   #\^)
           (loop result))
          (else
            (let ((sha1 (substring line 0 40))
                  (rest (has-prefix? (substring line 41 (string-length line)) "refs/tags/")))
              (loop
                (if rest
                  (cons (vector sha1 rest) result)
                  result))))))))))

