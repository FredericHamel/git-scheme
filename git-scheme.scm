
(define-library (git)
  (export clone checkout status ls-remote-tag tag-list max-tag)

  (import (gambit) (rename (prefix (version) version-)
                           (version-version>? version>?)))

  (begin
    (define (has-prefix? str prefix)
      (and (string=? str)
           (string=? prefix)
           (let ((len-str (string-length str))
                 (len-prefix (string-length prefix)))
             (and (>= len-str len-prefix)
                  (string=? prefix
                            (substring str 0 len-prefix))
                  (substring str len-prefix len-str)))))

    (define (parent-directory dir)
      (path-directory
        (path-strip-trailing-directory-separator
          dir)))

    ;; Find git repository
    (define (git-find dir)
      (let loop ((curdir dir))
        (if (file-exists? (path-expand ".git" curdir))
          curdir
          (if (string=? curdir "/")
            (error "Reaching filesystem boundary")
            (loop (parent-directory curdir))))))

    (define (is-tag? ref)
      (has-prefix? ref "refs/tags/"))

    (define (run args-list #!key (directory #f) (no-prompt #f))
      (let ((pid (open-process (list path: "git"
                                     arguments: args-list
                                     directory: (or directory ".")
                                     stdout-redirection: #t
                                     environment: (and no-prompt (list "GIT_TERMINAL_PROMPT=0"))))))
        pid))

    ;; Begin of exports function.
    ;; Possibly kill process if don't terminate
    (define (close pid)
      (close-input-port pid)
      (close-output-port pid))

    ;; Show repo status
    (define (status dir)
      (let ((pid (run (list "status") directory: dir)))
        (let loop ((line (read-line pid)))
          (cond
            ((eof-object? line)
             (close pid))
            (else
              (begin
                (println line)
                (loop (read-line pid))))))))

    (define (clone url dir #!key (timeout 5))
      (let ((pid (run (list "clone" url dir))))
        (let ((status (process-status pid 5 #f)))
          (if status
            (or (= status 0)
              (println "[git] Process terminate with status: " status))
            (println "[git] Process didn't terminate: " status))
          (close pid))))

    (define (checkout repo-dir ver)
      (let ((dir (git-find repo-dir)))
        (let ((pid (run (list "checkout" ver) directory: dir)))
          (let ((status (process-status pid)))
            (close pid)
            (= status 0)))))

    (define (tag-list repo-dir)
      (let ((dir (git-find repo-dir)))
        (let ((pid (run (list "tag") directory: dir)))
          (let ((status (process-status pid 5 #f)))
            (if status
              (cond
                ((= status 0)
                 (let loop ((rev-res '()))
                   (let ((line (read-line pid)))
                     (if (eof-object? line)
                       (begin
                         (close pid)
                         (reverse rev-res))
                       (loop (cons
                               (version-parse line) rev-res))))))
                (else
                  (error "Process did not terminate correctly")))
              (error "Process timeout"))))))

    (define (max-tag repo-dir base-ver)
      (let ((dir (git-find repo-dir)))
        (let ((pid (run (list "tag") directory: dir)))
          (let ((status (process-status pid 5 #f)))
            (if status
              (cond
                ((= status 0)
                 (let loop ((max-ver base-ver))
                   (let ((line (read-line pid)))
                     (if (eof-object? line)
                       (begin (close pid) max-ver)
                       (let ((cur-ver (version-parse line)))
                         (loop (if (version>? cur-ver max-ver)
                                 cur-ver max-ver)))))))
                (else
                  (error "Process did not terminate correctly")))
              (error "Process timeout"))))))

    (define (ls-remote-tag url)
      (let ((pid (run (list "ls-remote" "--tags" url) no-prompt: #t)))
        (letrec ((read-refs
                (lambda (r)
                  (let ((sha1 (read-line r #\tab))
                        (ref (read-line r)))
                    (cond
                      ((and (string? sha1) (string? ref))
                       (let ((sha1-length (string-length sha1)))
                         (if (and (= sha1-length 40)
                                  (string->number sha1 16))
                           (let ((tag (is-tag? ref)))
                             (if tag
                               (let ((tag-length (string-length tag)))
                                 (if (string=? (substring tag (- tag-length 3) tag-length) "^{}")
                                   (read-refs r)
                                   tag))))

                           (error "Not a valid sha1"))))
                      (else
                        ref))))))
          (let ((lst-tag (read-all pid (lambda (r) (read-refs r)))))
            (close pid)
            (and (= (process-status pid) 0) lst-tag)))))


    #;(define (tag repo-dir #!key (sha1? #f))
      (let* ((dir (git-find repo-dir))
             (packed-refs-port
               (open-input-file
                 (path-expand ".git/packed-refs" dir))))

        (read-line packed-refs-port)
        (let loop ((tag-list '()))
          (let ((line (read-line packed-refs-port)))
            (if (eof-object? line)
              (begin
                (close-input-port packed-refs-port)
                tag-list)
              (cond
                ((char=? (string-ref line 0) #\^)
                 (loop tag-list))
                (else
                  (let ((sha1 (substring line 0 40))
                        (ref (is-tag? (substring line 41 (string-length line)))))
                    (loop (if ref
                            (cons (if sha1? (vector sha1 ref) (version-parse ref)) tag-list)
                            tag-list))))))))))))

