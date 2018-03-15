
(define-library (git)
  (export clone checkout status ls-remote-tag ls-tag max-tag)

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

    (define (run args-list thunk #!key (directory #f) (no-prompt #f))
      (call-with-input-process
        (list path: "git"
              arguments: args-list
              directory: directory
              environment: (and no-prompt (list "GIT_TERMINAL_PROMPT=0")))
        (lambda (p)
          (thunk p)))
      #;(let ((pid (open-process (list path: "git"
                                     arguments: args-list
                                     directory: (or directory ".")
                                     stdout-redirection: #t
                                     environment: (and no-prompt (list "GIT_TERMINAL_PROMPT=0"))))))
        pid))

    ;; Begin of exports function.
    (define (show-output pid)
      (let loop ((line (read-line p)))
        (if (not (eof-object? line))
          (begin
            (println line)
            (loop (read-line p))))))

    ;; Show repo status
    (define (status dir)
      (run (list "status")
           (lambda (p)
             (show-output p))
           directory: dir)
      #;(let ((pid (run (list "status") directory: dir)))
        (let loop ((line (read-line pid)))
          (cond
            ((eof-object? line)
             (close pid))
            (else
              (begin
                (println line)
                (loop (read-line pid))))))))

    (define (clone url dir #!key (quiet? #t) (timeout 5) (directory #f))
      (run (if quiet?
             (list "clone" "--quiet" url dir)
             (list "clone" url dir))
           (lambda (p)
             (let ((status (process-status p 5 255)))
               (or (= status)
                   (println "[git] Process terminated with status: " status))))
           directory: directory)

      #;(let ((pid (run (list "clone" url dir))))
        (let ((status (process-status pid 5 #f)))
          (if status
            (or (= status 0)
              (println "[git] Process terminate with status: " status))
            (println "[git] Process didn't terminate: " status))
          (close pid))))

    (define (checkout repo-dir ver #!key (quiet? #t))
      (let ((dir (git-find repo-dir)))
        (run (if quiet?
               (list "checkout" "--quiet" ver)
               (list "checkout" ver))

             (lambda (pid)
               (let ((status (process-status pid)))
                 (= status 0)))
             directory: dir)
        #;(let ((pid (run (list "checkout" ver) directory: dir)))
          (let ((status (process-status pid)))
            (close pid)
            (= status 0)))))

    (define (ls-tag repo-dir)
      (let ((dir (git-find repo-dir)))
        (run (list "tag")
             (lambda (pid)
               (let ((status (process-status pid 5 255)))
                   (cond
                     ((= status 0)
                      (let loop ((rev-res '()))
                        (let ((line (read-line pid)))
                          (if (eof-object? line)
                            (reverse rev-res)
                            (loop (cons
                                    (version-parse line) rev-res))))))
                     (else
                       (error "[git] Process terminated with status: " status)))))
             directory: dir)))

    (define (max-tag repo-dir base-ver)
      (let ((dir (git-find repo-dir)))
        (run (list "tag" directory: dir)
             (lambda (pid)
               (let ((status (process-status 5 255)))
                 (cond
                   ((= status 0)
                    (let loop ((max-ver base-ver))
                      (let ((line (read-line pid)))
                        (if (eof-object? line)
                          max-ver
                          (let ((cur-ver (version-parse line)))
                            (loop (if (version>= cur-ver max-ver)
                                    cur-ver max-ver)))))))))))
        #;(let ((pid (run (list "tag") directory: dir)))
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

      (run (list "ls-remote" "--tags" url)
           (lambda (pid)
             (let ((tag-list (read-all pid (lambda (r) (read-refs r)))))
               (and (= (process-status pid 5 255) 0) tag-list)))
           no-prompt: #t)
      #;(let ((pid (run (list "ls-remote" "--tags" url) no-prompt: #t)))
        (letrec ((read-refs
                (lambda (r)
                  #f)))
          (let ((lst-tag (read-all pid (lambda (r) (read-refs r)))))
            (close pid)
            (and (= (process-status pid) 0) lst-tag))))))


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

