#! /usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 getopt-long))
(use-modules (ice-9 format))

(define (main args)
  (let* ((option-spec '((version      (single-char #\v) (value #f))
                        (help         (single-char #\h) (value #f))
                        (polarization (single-char #\d) (value #t))
                        (logic-value  (single-char #\k) (value #t)) 
                        (function     (single-char #\f) (value #t))))
                                      ;(required? #t))))
                                      ;(predicate string->number))))
        (options (getopt-long args option-spec))
        (help-wanted (option-ref options 'help #f))
        (version-wanted (option-ref options 'version #f))
        (d (string->number (option-ref options 'polarization "0")))
        (k (option-ref options 'logic-value #f))
        (f (option-ref options 'function #f)))
    (if (or version-wanted help-wanted (not k) (not f))
        (begin
          (if version-wanted
            (display "getopt-long-example version 0.3\n"))
          (if (or help-wanted (not k) (not f))
            (display "\
getopt-long-example [options]
 -v, --version       Display version
 -h, --help          Display this help
 -k, --logic-value   Value of the logic (required)
 -f, --function      Vector of the function (required)
 -d, --polarization  Polarization of the polynomial (default: 0)
")))
       (begin
         (set! k (string->number k))
         (format #t "k = ~a\nd = ~d\nfunction = ~a\n" k d f)))))
