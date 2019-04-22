
(define-library (git-scheme)
  (export archive clone checkout pull
          status ls-remote-tag
          ls-tag max-tag
          extract-archive)

  (import (gambit) (semver))

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

    (define (run args-list thunk #!key (directory #f) (no-prompt? #t))
      (call-with-input-process
        (list path: "git"
              arguments: args-list
              directory: directory
              environment: (and no-prompt? (list "GIT_TERMINAL_PROMPT=0")))
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
                  directory: directory))))


    (define (extract-archive archive-name dir)
      (call-with-input-process
        (list path: "tar"
              arguments: (list "xf" archive-name)
              directory: dir)
        (lambda (p) (= (process-status p) 0))))

    (define (clone url dir #!key (quiet? #t) (no-prompt? #t) (timeout 5) (directory #f))
      (run (if quiet?
             (list "clone" "--quiet" url dir)
             (list "clone" url dir))
           (lambda (p)
             (let ((status (process-status p 5 255)))
               (= status 0)))
           directory: directory
           no-prompt?: no-prompt?))

    (define (checkout repo-dir ver #!key (quiet? #t))
      (let ((dir (git-find repo-dir)))
        (run (if quiet?
               (list "checkout" "--quiet" ver)
               (list "checkout" ver))

             (lambda (pid)
               (let ((status (process-status pid)))
                 (= status 0)))
             directory: dir)))

    (define (pull repo-dir #!key (quiet? #t))
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
                            (loop (cons
                                    (parse-version line) rev-res))))))
                     (else
                       (error "[git] Process terminated with status: " status)))))
             directory: dir)))

    (define (max-tag repo-dir base-ver)
      (let ((dir (git-find repo-dir)))
        (run (list "tag")
             (lambda (pid)
               (let ((status (process-status pid 5 255)))
                 (cond
                   ((= status 0)
                    (let loop ((max-ver base-ver))
                      (let ((line (read-line pid)))
                        (if (eof-object? line)
                          max-ver
                          (let ((cur-ver (parse-version line)))
                            (loop (if (version>? cur-ver max-ver)
                                    cur-ver max-ver))))))))))
               directory: dir)))

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
           no-prompt: #t)))))

