
(define-library (git-scheme)
  (export archive clone checkout pull
          status ls-remote-tag
          ls-tag max-tag
          extract-archive)

  (import (gambit))

  (begin

    (define (parent-directory dir)
      (path-directory
        (path-strip-trailing-directory-separator
          dir)))

    ;; Find git repository using a similar find algorithm.
    (define (git-find dir)
      (let loop ((curdir (path-expand dir)))
        (if (file-exists? (path-expand ".git" curdir))
          curdir
          (if (string=? curdir "/")
            (error "Reaching filesystem boundary")
            (loop (parent-directory curdir))))))

    (define (is-tag? ref)
      (define (start-with? str prefix)
        (and (string=? str)
             (string=? prefix)
             (let ((len-str (string-length str))
                   (len-prefix (string-length prefix)))
               (and (>= len-str len-prefix)
                    (string=? prefix
                              (substring str 0 len-prefix))
                    (substring str len-prefix len-str)))))
      (start-with? ref "refs/tags/"))

    (define (run args-list thunk #!optional (directory #f) (prompt? #f))
      (call-with-input-process
        (list path: "git"
              arguments: args-list
              directory: directory
              environment: (and (not prompt?)
                                (list "GIT_TERMINAL_PROMPT=0")))
        (lambda (p)
          (thunk p))))

    (define (show-output pid)
      (let loop ((line (read-line pid)))
        (if (not (eof-object? line))
          (begin
            (println line)
            (loop (read-line pid))))))

    ;; Begin of exports function.
    ;; Show repo status
    (define (status dir)
      (run (list "status")
           (lambda (p)
             (show-output p))
           directory: dir))

    ;; dir: path to filesystem repo.
    (define (archive tag output #!optional (directory #f))
      (let ((taglist (ls-tag directory))
            (archive-name (string-append output ".tar"))
            (prefix (string-append "--prefix=" tag "/")))
        (and (member tag taglist)
             (run (list "archive" "-o" archive-name prefix tag)
                  (lambda (p)
                    (cond
                      ((= (process-status p) 0) archive-name)
                      (else
                        (let () (delete-file archive-name) #f))))
                  directory))))


    (define (extract-archive archive-name dir)
      (call-with-input-process
        (list path: "tar"
              arguments: (list "xf" archive-name)
              directory: dir)
        (lambda (p) (let ((result (= (process-status p) 0)))
                      (delete-file archive-name)
                      result))))

    (define (clone url dir #!key (quiet? #t) (prompt? #f) (timeout 5) (directory #f))
      (run (if quiet?
             (list "clone" "--quiet" url dir)
             (list "clone" url dir))
           (lambda (p)
             (let ((status (if prompt?
                             (process-status p)
                             (process-status p timeout 255))))
               (= status 0)))
           directory prompt?))

    (define (checkout repo-dir ver #!key (quiet? #t))
      (let ((dir (git-find repo-dir)))
        (run (if quiet?
               (list "checkout" "--quiet" ver)
               (list "checkout" ver))

             (lambda (pid)
               (let ((status (process-status pid)))
                 (= status 0)))
             dir)))

    (define (pull repo-dir #!key (quiet? #t) (prompt? #f))
      (run (if quiet?
             (list "pull" "--quiet" "origin" "master")
             (list "pull" "origin" "master"))

           (lambda (pid)
             (let ((status (process-status pid)))
               (= status 0)))
           directory: repo-dir))

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
                            (loop (cons line rev-res))))))
                     (else
                       (error "[git] Process terminated with status: " status)))))
             dir)))

    ;; Not tested.
    (define (max-tag repo-dir base-version thunk)
      (let ((dir (git-find repo-dir)))
        (run (list "tag")
             (lambda (pid)
               (let ((status (process-status pid 5 255)))
                 (and (= status 0)
                      (let loop ((max-version base-version))
                        (let ((line (read-line pid)))
                          (if (eof-object? line)
                            max-version
                            (loop (thunk line max-version))))))))
             dir)))

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
               (and (= (process-status pid 5 255) 0) tag-list))))))))

