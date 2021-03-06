;; Normal (not aligned) identation
(put 'and-let* 'racket-indent-function 1)
(put 'let-args 'racket-indent-function 1)
(put 'test-assert 'racket-indent-function 1)
(put 'test-eq 'racket-indent-function 1)
(put 'test-eqv 'racket-indent-function 1)
(put 'test-equal 'racket-indent-function 1)
(put 'test-approximate 'racket-indent-function 1)
(put 'test-error 'racket-indent-function 1)

;; Keywords highlighting
(defconst scm-keywords
    '("letrec-syntax" "parametrize" "define-constant"
         "load" "include" "require"
         "define*" "lambda*" "let*" "letrec*" "and-let*"
         "define-values" "set!-values"
         "values" "call-with-values" "receive" "let-values" "let*-values" ;; SRFI-8,11
         "match" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec"
         "match-define" "rxmatch-cond" "rxmatch-case"
         "raise" "with-exception-handler" "guard" ;; SRFI-34
         "define-record-type" "define-condition-type" "define-reader-ctor"
         "define-module" "use-modules" "use" "select-module" "with-module" "import"
         "export" "export-all" "extend" "dynamic-wind" "unwind-protect"
         "call-with-current-continuation" "call/cc"
         "cut" "cute" "$" "$*" ".$" "~" "delay" "force" "lazy" "eager")) ;; SRFI-26

(defconst scm-goops-keywords
    '("define-class" "slot-set!" "slot-ref" "class-slot-set!" "class-slot-ref"
        "define-method"))

;; Builtins highlighting
(defconst scm-builtins
    '("set-car!" "set-cdr!" "inc!" "dec!" "update!" "index" "let-args"
         "sorted?" "sort" "sort!" "stable-sort" "stable-sort!" "merge" "merge!"
         "acons" "assq-ref" "assv-ref" "assoc-ref" "make-parameter"
         "trace-macro" "macroexpand" "macroexpand-1" "macroexpand-all"
         "errorf" "syntax-error" "syntax-errorf" "any-pred" "every-pred"
         "compare" "default-hash" "portable-hash" "comparator-hash" "combine-hash-value"
         "comparator-compare" "default-comparator" "<?" "<?" "<=?" ">=?" ">?" "map-with-index"
         "exact" "inexact" "exact?" "inexact?" "finite?" "infinite?" "min&max" "approx=?"
         "quotient&remainder" "square" "make-rectangular" "make-polar" "pprint"
         "undefined" "get-keyword" "complement" "reduce" "reduce-right"
         "string->regexp" "rxmatch" "rxmatch-substring" "rxmatch-substrings"
         "regexp-replace" "regexp-replace-all"
         "hash-table" "hash-table-unfold" "hash-table-get" "hash-table-put!"
         "hash-table-contains?" "hash-table-delete" "hash-table-push!"
         "hash-table-pop!" "hash-table-map" "hash-table-for-each"
         "call-with-string-io" "with-string-io"))

(defconst scm-goops-builtins
    '("make" "make-instance" "merge-generics" "next-method" "class-of" "is-a?"))

(defconst scm-port-builtins
    '("put-string" "get-string-all"))

(defconst scm-hash-table-builtins ;; SRFI-69
    '("make-hash-table" "hash-table-merge!"
         "alist->hash-table" "hash-table->alist"
         "hash-table?"
         "hash-table-exists?"
         "hash-table-ref" "hash-table-ref/default"
         "hash-table-set!"
         "hash-table-update!" "hash-table-update!/default"
         "hash-table-delete!"
         "hash-table-size"
         "hash-table-keys" "hash-table-values"
         "hash-table-walk" "hash-table-fold"))

(defconst scm-record-type-builtins ;; SRFI-9
    '("define-record-type"))

(defconst scm-random-source-builtins ;; SRFI-27
    '("default-random-source" "random-source-randomize!"
         "random-integer" "random-real"))

(defconst scm-list-library-builtins ;; SRFI-1
    '("list-tabulate" "iota" "list="
         "concatenate" "zip" "unzip1" "unzip2" "unzip3" "unzip4"
         "fold" "fold-right" "unfold" "unfold-right"
         "find" "find-tail" "take-while" "drop-while" "span" "break"
         "every" "any" "list-index"
         "delete" "delete-duplicates"))

(defconst scm-string-library-builtins ;; SRFI-13
    '("string-tabulate"
         "string-null?" "string-every" "string-any"
         "string-take" "string-drop" "string-take-right" "string-drop-right"
         "string-pad" "string-pad-right" "string-trim-right" "string-trim-both"
         "string=" "string<>" "string<" "string>" "string<=" "string>="
         "string-ci=" "string-ci<>" "string-ci<" "string-ci>" "string-ci<=" "string-ci>="
         "string-hash" "string-hash-ci"
         "string-index" "string-index-right" "string-skip" "string-skip-right"
         "string-count" "string-contains" "string-contains-ci"
         "string-reverse" "string-concatenate" "xsubstring"
         "string-map" "string-for-each" "string-for-each-index"
         "string-fold" "string-fold-right" "string-unfold" "string-unfold-right"
         "string-tokenize" "string-filter" "string-delete" "string-scan"))

(defconst scm-charset-library-builtins ;; SRFI-14
    '("char-set" "char-set?" "char-set=" "char-set<=" "char-set-hash"
         "list->char-set" "char-set->list" "string->char-set" "char-set->string"
         "char-set-fold" "char-set-unfold" "char-set-for-each" "char-set-map"
         "char-set-filter"
         "char-set-size" "char-set-count"
         "char-set-contains?" "char-set-every" "char-set-any"
         "char-set-adjoin" "char-set-delete"
         "char-set-union" "char-set-intersection" "char-set-difference" "char-set-xor"
         "char-set-complement"))

(defconst scm-vector-library-builtins ;; SRFI-133
    '("vector-unfold" "vector-unfold-right" "vector-append" "vector-concatenate"
         "vector-empty?" "vector=" "vector-fold" "vector-fold-right"
         "vector-map" "vector-for-each" "vector-count" "vector-tabulate"
         "vector-index" "vector-index-right" "vector-skip" "vector-skip-right"
         "vector-any" "vector-every" "vector-partition" "vector-swap!" "make-vector"
         "vector-fill!" "vector-reverse!"))

(defconst scm-irregex-library-builtins ;; SRFI-115
    '("irregex" "irregex?"
         "irregex-search" "irregex-match"
         "irregex-substring"
         "irregex-replace" "irregex-replace/all"
         "irregex-split" "irregex-extract" "irregex-fold"))

(defconst scm-comprehensions-builtins ;; SRFI-42
    '("do-ec" "list-ec" "append-ec" "string-ec" "string-append-ec" "vector-ec"
         "fold-ec" "fold3-ec" "any?-ec" "every?-ec" "first-ec" "last-ec"
         ":list" ":string" ":vector" ":range" ":real-range" ":char-range" ":port"
         ":parallel" ":while" ":until"))

(defconst scm-time-date-builtins ;; SRFI-19
    '("current-time" "current-date"
         "make-time" "time-nanosecond" "time-second" "time-utc" "time-duration"
         "time<=?" "time<?" "time=?" "time>=?" "time>?"
         "time-difference" "add-duration" "subtract-duration"
         "make-date" "date-nanosecond" "date-second" "date-minute" "date-hour"
         "date-day" "date-month" "date-year" "date-zone-offset" "date-year-day"
         "date-week-day" "date-week-number" "date->time-utc" "time-utc->date"
         "date->string" "string->date"))

(defconst scm-condition-builtins ;; SRFI-34,35
    '("define-condition-type" "condition"))

(defconst scm-unit-test-builtins ;; SRFI-64
    '("test-begin" "test-end"
         "test-group" "test-group-with-cleanup"
         "test-assert"
         "test-eq" "test-eqv" "test-equal"
         "test-approximate"
         "test-error" "test-expect-fail"
         "test-skip"))

(defconst scm-data-structure-builtins
    '("make-stack" "stack-null?" "push" "pop" "peek"
         "make-queue" "queue-null?" "enqueue" "dequeue" "front"))

;; Types highlighting
(defconst scm-types
    '("MyType1" "MyType2"))

(font-lock-add-keywords 'racket-mode
    `((,(regexp-opt
            (append scm-keywords scm-goops-keywords) t) . font-lock-keyword-face)
         (,(regexp-opt
               (append
                   scm-builtins
                   scm-goops-builtins
                   scm-port-builtins
                   scm-hash-table-builtins
                   scm-record-type-builtins
                   scm-random-source-builtins
                   scm-list-library-builtins
                   scm-string-library-builtins
                   scm-charset-library-builtins
                   scm-vector-library-builtins
                   scm-irregex-library-builtins
                   scm-comprehensions-builtins
                   scm-time-date-builtins
                   scm-condition-builtins
                   scm-unit-test-builtins
                   scm-data-structure-builtins)
               t) . font-lock-builtin-face)
         (,(regexp-opt scm-types t) . font-lock-type-face)))
