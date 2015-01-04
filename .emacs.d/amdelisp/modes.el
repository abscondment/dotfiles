;;;;;;;;;;;;;;;;;;;;;;;;; mode defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autoloads (file &rest funcs)
  "A helper function written by jp that lets you autoload many
functions from one source file."
  (let ((result))
    (while (not (null funcs))
      (let ((func-string (format "%s" (car funcs))))
        (setq result (cons `(autoload (quote ,(car funcs))
                              ,file
                              ,func-string
                              (quote ,(car funcs)))
                           result)))
      (setq funcs (cdr funcs)))
    (eval `(progn ,@result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags

(setq tags-revert-without-query t)
(defun my-etags-setup ()
  (setq case-fold-search nil))
(eval-after-load "etags"
  '(add-hook 'tags-table-format-hooks 'my-etags-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-color

(autoloads "ansi-color"
           'ansi-color-for-comint-mode-on
           'ansi-color-apply-on-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep
(setq grep-command "grep -n -s -F ")
                                        ;
;; ansi colors for grep - makes --color=auto work
(eval-after-load "compile"
  '(defun grep-process-setup ()
     "Set up `compilation-exit-message-function' for `grep'."
     (set (make-local-variable 'compilation-exit-message-function)
          (lambda (status code msg)
            (require 'ansi-color)
            (ansi-color-apply-on-region (point-min) (point-max))
            (if (eq status 'exit)
                (cond ((zerop code)
                       '("finished (matches found)\n" . "matched"))
                      ((= code 1)
                       '("finished with no matches found\n" . "no match"))
                      (t
                       (cons msg code)))
              (cons msg code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

(eval-when-compile (require 'compile))

(defun my-compile-setup ()
  (setq compilation-scroll-output t
        compilation-window-height 20))
(add-hook 'compilation-mode-hook 'my-compile-setup)

;; this little helper highlights gcc compilation lines
;; (defvar compilation-added-font-lock-keywords nil)
;; (setq compilation-added-font-lock-keywords
;;       (list
;;        '("^\\(g?cc\\|[gc][+][+]\\) .* \\([^ ]+\.\\(c\\|cc\\|cpp\\)\\)$"
;;          (1 font-lock-keyword-face) (2 font-lock-constant-face))))
;; (font-lock-add-keywords 'compilation-mode compilation-added-font-lock-keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filladapt mode

(require 'filladapt)
(setq filladapt-mode-line-string nil)
(setq-default filladapt-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c/cc/java/csharp mode

;; shared

;; (eval-when-compile (require 'cc-mode)
;;                    (require 'dabbrev))

;; (setq minor-mode-alist (assq-delete-all 'abbrev-mode minor-mode-alist))

;; ;; guess-offset for all c++ files
;; (eval-after-load "cc-mode"
;;   '(require 'guess-offset))

;; (defun cc-debug ()
;;   (interactive)
;;   (setq c-echo-syntactic-information-p (not c-echo-syntactic-information-p)))

;; (defun my-cc-common-setup ()
;;   (setq c-auto-newline nil
;;         c-basic-offset 2
;;         c-electric-pound-behavior '(alignleft)
;;         dabbrev-case-replace nil
;;         fill-column 80
;;         indent-tabs-mode nil
;;         )
;;   (c-setup-filladapt)
;;   (define-key c-mode-base-map "\C-m" `c-context-line-break)
;;   (define-key c-mode-base-map "\C-c\C-u" 'uncomment-region))
;; (add-hook 'c-mode-common-hook 'my-cc-common-setup)

;; java

;;;(require 'dabbrev)
;; (define-abbrev-table 'java-mode-abbrev-table
;;   '(("sop" "System.out.println" nil 1)
;;     ("sof" "System.out.flush" nil 1)
;;     ("sep" "System.err.println" nil 1)
;;     ("ctm" "System.currentTimeMillis" nil 1)
;;     ("sac" "System.arraycopy" nil 1)
;;     ("ija" "import java.awt.*")
;;     ("iji" "import java.io.*")
;;     ("ijn" "import java.net.*")
;;     ("ijt" "import java.text.*")
;;     ("iju" "import java.util.*")
;;     ("cch" "/**
;;  *
;;  *
;;  * @author
;;  * @version     %\111%, %\107%
;;  */" java-class-comment-hook 0)
;;     ("mch" "/**
;;  *
;;  */" nil 0)
;;     ))

;; (define-abbrev-table 'c++-mode-abbrev-table
;;   '(("sop" "printf" nil 1)
;;     ("cch" "/**
;;  *
;;  *
;;  * @author
;;  * @version     %\111%, %\107%
;;  */" java-class-comment-hook 0)
;;     ("mch" "/**
;;  *
;;  */" nil 0)
;;     ))

;; (setq c-mode-abbrev-table c++-mode-abbrev-table)

;; (defun java-class-comment-hook ()
;;   (save-excursion
;;     (let ((username (getenv "FULLUSERNAME"))
;;           )
;;       (when (not (null username))
;;         (forward-line -2)
;;         (end-of-line)
;;         (insert username))))
;;   )

;; (defun copyright-comment-hook ()
;;   (let ((username (or (getenv "FULLUSERNAME") (getenv "USER") (getenv "USERNAME")))
;;         (filename (file-name-nondirectory (buffer-file-name)))
;;         )
;;     (if (not (null username))
;;         (save-excursion
;;           (search-backward "Author: ")
;;           (end-of-line)
;;           (insert username)))
;;     (if (not (null filename))
;;         (save-excursion
;;           (search-backward "File: ")
;;           (end-of-line)
;;           (insert filename)))))

;; ;; jdok
;; (eval-when-compile (require 'jdok))
;; (defun my-jdok-setup ()
;;   (jdok-define-template 'jdok-author-tag-template '("* @author\t" (getenv "FULLUSERNAME")))

;;   (jdok-define-template 'jdok-since-tag-template nil)
;;   (jdok-define-template 'jdok-version-tag-template '("* @version\t%\111%, %\107%"))
;;   (defun jdok-insert-see-tag (ref)))
;; (add-hook 'jdok-load-hook 'my-jdok-setup)
;; (autoload 'jdok-generate-javadoc-template "jdok" "jdok" t)

;; (defun my-java-setup ()
;;   (abbrev-mode 1)
;;   (c-add-style
;;    "amd-java"
;;    '((c-offsets-alist . ((arglist-intro . +)
;;                          (access-label . 0)
;;                          (case-label . *)
;;                          (statement-case-intro . *)
;;                          (substatement-open . 0)
;;                          (inline-open . 0)
;;                          (block-open - 0)
;;                          )))
;;    t)
;;   (setq tab-width 2
;;         c-basic-offset 2)
;; )
;; (add-hook 'java-mode-hook 'my-java-setup)

;; (defun jad-buffer ()
;;   "Runs jad.exe on the current buffer, which is presumably a .class file."
;;   (interactive)
;;   (let ((tmp (make-temp-file "jad.class"))
;;         (buf (get-empty-buffer "*jad*")))
;;     (write-region (point-min) (point-max) tmp nil nil)
;;     (call-process "jad.exe" nil buf t "-p" tmp)
;;     (delete-file tmp)
;;     (switch-to-buffer buf)
;;     (goto-char (point-min))
;;     (java-mode)))

;; ;; cc

;; (defun my-cc-setup ()
;;   (c-add-style
;;    "amd"
;;    '((c-comment-only-line-offset . (0 . 0))
;;      (c-offsets-alist . ((statement-block-intro . +)
;;                          (knr-argdecl-intro . 5)
;;                          (substatement-open . 0)
;;                          (label . 0)
;;                          (statement-case-open . 0)
;;                          (statement-cont . +)
;;                          (arglist-close . c-lineup-arglist)
;;                          (arglist-intro . +)
;;                          (access-label . -)
;;                          (case-label . +)
;;                          (statement-case-intro . +)
;;                          (inline-open . 0)
;;                          )))
;;    t)
;;   )
;; (add-hook 'c++-mode-hook 'my-cc-setup)

(add-to-list 'auto-mode-alist '("\\.\\(c\\|h\\|cpp\\|uc\\)$" . c++-mode))
(add-to-list 'auto-mode-alist '("/\\(MAP\\|VECTOR\\|LIST\\|XTREE\\|XMEMORY\\|FUNCTIONAL\\)$" . c++-mode))

;; objc

(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))

;; (defun my-objc-setup ()
;;   (abbrev-mode 1)
;;   (c-add-style
;;    "amd-objc"
;;    '((c-offsets-alist . ((arglist-intro . +)
;;                          (access-label . 0)
;;                          (case-label . *)
;;                          (statement-case-intro . *)
;;                          (substatement-open . 0)
;;                          (inline-open . 0)
;;                          (block-open - 0)
;;                          )))
;;    t))
;; (add-hook 'objc-mode-hook 'my-objc-setup)


;; ;; bah - remove c-mode entirely! We have to do it this way because it's
;; ;; in autoloads
;; (defun adjust-autoloads ()
;;   (delete '("\\.[ch]\\'" . c-mode) auto-mode-alist))
;; (add-hook 'after-init-hook 'adjust-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c#

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; amd - are these still used?
;; (eval-after-load "compile"
;;   '(progn
;;      (add-to-list 'compilation-error-regexp-alist
;;                   '("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4))
;;      (add-to-list 'compilation-error-regexp-alist
;;                   '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))))

;; (defun my-csharp-setup ()
;;   (c-add-style
;;    "amd-csharp"
;;    '((c-comment-only-line-offset . (0 . 0))
;;      (c-offsets-alist . (
;;                          (c . c-lineup-C-comments)
;;                          (inclass . 0)
;;                          (namespace-open . +)
;;                          (namespace-close . +)
;;                          (innamespace . 0)
;;                          (class-open . 0)
;;                          (class-close . 0)
;;                          (inclass . +)
;;                          (defun-open . 0)
;;                          (defun-block-intro . +)
;;                          (inline-open . 0)
;;                          (statement-block-intro . 0)
;;                          (brace-list-intro . +)
;;                          (substatement-open . 0)
;;                          (statement-block-intro . +)
;;                          (case-label . +)
;;                          (block-open - 0)
;;                          )))
;;    t)
;;   )
;; (add-hook 'csharp-mode-hook 'my-csharp-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text

;; (defun my-text-setup ()
;;   (turn-on-auto-fill)
;;   (if (eq indent-line-function 'indent-to-left-margin)
;;       (setq indent-line-function 'indent-relative-maybe)))
;; (add-hook 'text-mode-hook 'my-text-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl

;; (eval-when-compile (require 'perl-mode))
;; (defun my-perl-setup ()
;;   (setq tab-width 2))
;; (add-hook 'perl-mode-hook 'my-perl-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(eval-when-compile (require 'python-mode))
(defun my-python-setup ()
  (setq tab-width 2)
  (define-key py-mode-map [backspace] 'py-electric-backspace))
(add-hook 'python-mode-hook 'my-python-setup)
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcl

(eval-when-compile (require 'tcl))
(defun my-tcl-setup ()
  (setq indent-tabs-mode nil))
(add-hook 'tcl-mode-hook 'my-tcl-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makefile

(eval-when-compile (require 'make-mode))
(defun my-makefile-setup ()
  (define-key makefile-mode-map "\M-m" 'compile-make)
  (define-key makefile-mode-map "\M-p" 'compile-re-make))
(add-hook 'makefile-mode-hook 'my-makefile-setup)
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\(defs\\|rules\\)$" . makefile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html and friends

;; html
(autoload 'html-helper-mode "html-helper-mode" "html-helper-mode" t)
(eval-when-compile (require 'html-helper-mode))
(setq html-helper-mode-uses-JDE nil)
(setq html-helper-new-buffer-template
      '("<html>\n<head>\n<title>   </title>\n</head>\n<body>\n</body>\n</html>\n"))
(defun my-html-setup ()
  (copy-face 'font-lock-function-name-face 'html-tag-face)
  (auto-fill-mode 0))
(add-hook 'html-helper-mode-hook 'my-html-setup)
(add-to-list 'auto-mode-alist '("\\.htm[l]?$" . html-helper-mode))

;; mmm
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)

(mmm-add-mode-ext-class 'html-helper-mode nil 'embedded-css)
(mmm-add-mode-ext-class 'html-helper-mode nil 'html-js)

;; hexcolor
(defun hex-color-face (s)
  (list
   :foreground
   (if (< (color-get-intensity s) 127) "white" "black")
   :background
   s))

(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (hex-color-face (match-string-no-properties 0)))))))

(defun hexcolor-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords))

;; css
(defun my-css-setup ()
  (setq css-indent-offset 2)
  (hexcolor-add-to-font-lock))
(eval-after-load "css-mode"
  '(add-hook 'css-mode-hook 'my-css-setup))

;; jsp
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-helper-mode))
(mmm-add-mode-ext-class 'html-helper-mode "\\.jsp$" 'jsp)
(add-to-list 'auto-mode-alist '("\\.jspi$" . html-helper-mode))
(mmm-add-mode-ext-class 'html-helper-mode "\\.jspi$" 'jsp)

;; js
(setq js2-use-font-lock-faces t)
(defun my-js2-setup ()
  (setq js2-basic-offset 2
        js2-auto-indent-p nil
        js2-enter-indents-newline t)
  (hexcolor-add-to-font-lock))
(eval-after-load "js2"
  '(add-hook 'js2-mode-hook 'my-js2-setup))
(add-to-list 'auto-mode-alist '("\\.as$" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; php

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql

(eval-when-compile (require 'sql))

;; default user/pw/db
(setq sql-user "xxxx"
      sql-password "yyy"
      sql-database "zzz"
      )
(eval-after-load "sql"
  '(progn
     (when (not (fboundp 'old-sql-get-login))
       (fset 'old-sql-get-login (symbol-function 'sql-get-login)))
     (defun sql-get-login (&rest what)
       "Overridden to add \"as sysdba\" when sql-user is sys"
       (apply 'old-sql-get-login what)
       (setq sql-oracle-options
             (if (string= sql-user "sys")
                 (list "as" "sysdba")
               nil)))))

(defun my-sql-setup ()
  (sql-set-sqli-buffer-generally))
(defun my-sql-send-file (path)
  (if (file-readable-p path)
      (progn
        (message "Sending %s..." path)
        (goto-char (point-max))
        (insert-file-contents path)
        (setq path (buffer-substring (point) (point-max)))
        (delete-region (point) (point-max))
        (comint-send-string sql-buffer path))
    (message "%s not found." path)))

(defun my-sql-interactive-setup ()
  (sql-set-sqli-buffer-generally)
  (setq comint-scroll-to-bottom-on-output t)
  (my-sql-send-file (format "%s/default.sql" (getenv "HOME"))))

(eval-after-load "sql"
  '(progn
     (add-hook 'sql-mode-hook 'my-sql-setup)
     (add-hook 'sql-interactive-mode-hook 'my-sql-interactive-setup)))

(add-to-list 'auto-mode-alist '("\\.ddl$" . sql-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu

(setq imenu-sort-function 'imenu--sort-by-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic modes

(setq max-specpdl-size 1000)
(eval-when-compile (require 'generic-x))
(if is-win32
    (setq generic-define-mswindows-modes t)
  (setq generic-define-unix-modes t)
  (setq generic-extras-enable-list (list 'ini-generic-mode)))

(add-to-list 'auto-mode-alist '("my.cnf$" . samba-generic-mode))
(let ((max-specpdl-size 1200))
  (require 'generic-x))

(defvar generic-mode-map nil "Keys used in some generic modes.")
(setq generic-mode-map (make-sparse-keymap))
(define-key generic-mode-map "\C-c\C-c" 'comment-region)

(eval-when-compile (require 'generic))
(require 'generic)

(define-generic-mode 'properties-generic-mode
  (list ?\#)
  nil
  '(("\\({.*}\\)" 1 'font-lock-type-face)
    ("^\\([\\.A-Za-z0-9_-]+\\)\\(\\s-+\\|\\(\\s-*[:?]?=\\s-*\\)\\)\\([^\r\n]*\\)$"
     (1 font-lock-reference-face) (4 font-lock-variable-name-face))
    ("^\\(!include\\) \\(.*\\):\\(.*\\)$"
     (1 'font-lock-keyword-face) (3 font-lock-function-name-face)))
  (list (concat "/" (eval-when-compile (regexp-opt '("properties.txt") t)))
        "\\.properties$" "\\.contract$")
  nil
  "Generic mode for properties.txt files.")

(define-generic-mode 'xdefaults-generic-mode
  (list ?\!)
  nil
  '(("^\\([\\.*A-Za-z0-9_]+\\)\\(\\s-+\\|\\(\\s-*:\\s-*\\)\\)\\([^\r\n]*\\)$"
     (1 font-lock-reference-face) (4 font-lock-variable-name-face)))
  (list "/xrdb\\.txt" "/\\.Xdefaults")
  nil
  "Generic mode for Xdefaults files.")

(define-generic-mode 'jad-generic-mode
  (list ?\#)
  nil
  '(("^\\([\\.A-Za-z0-9_\\-]+\\)\\(\\s-+\\|\\(\\s-*:\\s-*\\)\\)\\([^\r\n]*\\)$"
     (1 font-lock-reference-face) (4 font-lock-variable-name-face)))
  (list "\\.\\(jad\\|mf\\)$")
  nil
  "Generic mode for jad files.")

(define-generic-mode 'valgrind-generic-mode
  nil
  nil
  '(("^==[0-9]+== \\(Syscall param .*\\)$" (1 'font-lock-builtin-face))
    ("^==[0-9]+== \\(Invalid read .*\\)$" (1 'font-lock-keyword-face))
    ("^==[0-9]+== \\(Invalid write .*\\)$" (1 'font-lock-warning-face))
    ("^==[0-9]+== \\(Conditional jump or move .*\\)$" (1 'font-lock-constant-face))
    ("^==[0-9]+== \\([0-9]+ bytes in [0-9]+ blocks are still .*\\)$" (1 'font-lock-constant-face))
    ("^==[0-9]+== \\([0-9]+ bytes in [0-9]+ blocks are .* lost .*\\)$" (1 'font-lock-string-face))
    ("^==[0-9]+==  \\(Address 0x[0-9A-f]+ is [0-9]+ bytes .*\\)$" (1 'font-lock-string-face))
    ("^\\(==[0-9]+==\\) .*$" (1 'font-lock-builtin-face))
    )
  (list "/valgrind.txt")
  nil
  "Generic mode for valgrind.txt files.")


(when is-win32
  (define-generic-mode 'nsis-generic-mode
    (list ?\;)
    '("Section" "SectionEnd" "Function" "FunctionEnd" "Call" "Goto")
    '(("!\\([A-Za-z]+\\)" (1 'font-lock-builtin-face))
      ("$[({]?\\([A-Za-z0-9_]+\\)[)}]?" (1 'font-lock-variable-name-face))
      )
    (list "\\.\\(nsi\\|nsh\\)$")
    nil
    "Generic mode for nsis files."))

(define-generic-mode 'log4j-generic-mode
  nil
  nil
  '(("^\\([0-9-]+ [0-9:,]+\\) \\([A-Z ]+\\)\\(\\[[^\]]+\\]\\)"
     (1 'font-lock-builtin-face)
     (2 'font-lock-comment-face)
     (3 'font-lock-string-face))
    ("^\\([A-Za-z0-9_.$]+Exception\\):" 1 'font-lock-constant-face)
    ("^[ \t]+at \\([A-Za-z0-9_.<>$]+\\)(\\([^)]+\\))"
     (1 'font-lock-constant-face)
     (2 'font-lock-comment-face))
    )
  nil
  (list (lambda ()
          (make-local-variable 'font-lock-keywords-only)
          (setq font-lock-keywords-only t)))
  "Generic mode for log4j files.")

(define-generic-mode 'msg-generic-mode
  nil
  nil
  '(("^\\([A-Za-z-]+\\): \\([^\r\n]*\\)$"
     (1 font-lock-reference-face) (2 font-lock-variable-name-face))
    ("\\(\\${.*?}\\)" (1 'font-lock-keyword-face t)))
  (list "\\.msg$")
  nil
  "Generic mode for msg files.")

(define-generic-mode 'mbox-generic-mode
  nil
  nil
  '(("\\(From - \\w\\{3\\} \\w\\{3\\} [0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]\\{4\\}\\)"
     (1 'font-lock-comment-face))
    ;;	("^\\([A-Za-z0-9-]+\\): \\([^\r\n]*\\)[\r\n]*$"
    ;;	 (1 font-lock-reference-face) (2 font-lock-variable-name-face)))
    )
  nil
  nil
  "Generic mode for mbox files.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent-relative

(defun indent-relative (&optional unindented-ok)
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
If the previous nonblank line has no indent points beyond the
column point starts at, `tab-to-tab-stop' is done instead."
  (interactive "P")
  (if (and abbrev-mode
           (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let ((start-column (current-column))
        indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
          (let ((end (save-excursion (forward-line 1) (point))))
            (move-to-column start-column)
            ;; Is start-column inside a tab on this line?
            (if (> (current-column) start-column)
                (backward-char 1))
            (or (looking-at "[ \t]")
                unindented-ok
                (skip-chars-forward "^ \t" end))
            (skip-chars-forward " \t" end)
            (or (= (point) end) (setq indent (current-column))))))
    (if indent
        (let ((opoint (point-marker)))
          (delete-region (point) (progn (skip-chars-backward " \t") (point)))
          (indent-to indent 0)
          (if (> opoint (point))
              (goto-char opoint))
          (move-marker opoint nil))
      ;;; amd - only do tab-to-tab if we can't leave it alone
      (if (not unindented-ok)
          (tab-to-tab-stop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ps-print

(eval-when-compile (require 'ps-print))
(defun my-ps-hook ()
  (setq ps-line-number t
        ps-line-number-font "Times"
        ps-n-up-printing 2
        ps-print-color-p 'black-white
        ps-spool-duplex t
        ps-zebra-stripes t

        ps-left-margin   (/ (* 72 1.2) 2.54)
        ps-right-margin  (/ (* 72 0.2) 2.54)
        ps-bottom-margin (/ (* 72 0.5) 2.54)
        ps-top-margin    (/ (* 72 0.5) 2.54)))
                           ;;;     ^
                           ;;; centimeters
(add-hook 'ps-print-hook 'my-ps-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pager
(require 'pager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ascii
(eval-after-load "ascii"
  '(set-face-background 'ascii-ascii-face (face-background 'region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gud

(eval-when-compile (require 'gud))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apache

(add-to-list 'auto-mode-alist '("\\my.cnf$" . apache-mode))

;; (add-to-list
;;  'auto-mode-alist
;;  (cons (format "/%s$"
;;         (eval-when-compile (regexp-opt
;;                 '(".htaccess" "httpd.conf" "httpd.conf.in"
;;                   "srm.conf" "access.conf" "squid.conf"))))
;;        'apache-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff

(eval-after-load "ediff"
  '(setq ediff-split-window-function 'split-window-horizontally))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zip support

(setq archive-zip-use-pkzip nil)
(add-to-list 'auto-mode-alist '("\\.war$" . archive-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff

(eval-when-compile (require 'diff-mode))
(defun my-diff-setup ()
  (copy-face 'font-lock-string-face 'diff-removed-face)
  (copy-face 'font-lock-builtin-face 'diff-added-face)
  (copy-face 'font-lock-comment-face 'diff-hunk-header-face))
(add-hook 'diff-mode-hook 'my-diff-setup)
(add-to-list 'auto-mode-alist '("[.-]patch$" . diff-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-script-mode

(add-to-list
 'auto-mode-alist
 (cons
  (format "/%s$"
          (eval-when-compile
            (regexp-opt '(".bash_logout" ".bash_profile" ".bashrc" ".cshrc"
                          ".inputrc" "bashrc" "csh.cshrc" "csh.login" "profile"))))
  'shell-script-mode))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'auto-mode-alist '("\\.\\(tcsh\\|bash\\)$" . shell-script-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; p4

;; (autoloads "p4"
;;            'p4-client
;;            'p4-diff
;;            'p4-edit
;;            'p4-opened
;;            'p4-revert
;;            'p4-toggle-read-only
;;            'p4-set-client-name)
;; (autoload 'p4-prefix-map "p4" "p4-prefix-map" nil `keymap)

;; (eval-when-compile (require 'p4))
;; (eval-after-load "p4"
;;   '(progn
;;      (copy-face 'font-lock-keyword-face       'p4-diff-change-face)
;;      (copy-face 'font-lock-string-face        'p4-diff-del-face)
;;      (copy-face 'font-lock-variable-name-face 'p4-diff-file-face)
;;      (copy-face 'font-lock-comment-face       'p4-diff-head-face)
;;      (copy-face 'font-lock-builtin-face       'p4-diff-ins-face)
;;      (copy-face 'font-lock-variable-name-face 'p4-depot-unmapped-face)
;;      (copy-face 'font-lock-string-face        'p4-depot-deleted-face)
;;      (copy-face 'font-lock-keyword-face       'p4-depot-added-face)
;;      (copy-face 'font-lock-builtin-face       'p4-depot-branch-op-face)
;;      (setenv "P4DIFF" nil)))
;; ;;      (p4-detect-p4)))

;; (define-generic-mode 'p4-buffer-mode
;;   (list ?\#)
;;   nil
;;   (list
;;    (list "^\\([\\.A-Za-z0-9_]+\\):" 1 'font-lock-reference-face t))
;;   nil
;;   (list (lambda ()
;;           (font-lock-fontify-buffer)
;;           (setq indent-line-function 'indent-relative-maybe)
;;           (when (string-match "^\\*P4" (buffer-name))
;;             (setq p4-async-minor-mode t))
;;           (modify-syntax-entry ?/ ".")))
;;   "Generic mode for p4 buffers.")

;; (defun p4-pick-mode-hook ()
;;   "If this is a p4 buffer (p4 user, p4 client, p4 submit) switch into
;; our little p4-buffer-mode"
;;   (let ((specification
;;          (save-excursion
;;            (goto-char (point-min))
;;            (if (re-search-forward "^# A Perforce \\([A-za-z]+\\) Specification.$"
;;                                   (save-excursion (forward-line 5) (point))
;;                                   t)
;;                (match-string 1)
;;              ""))))
;;     (cond
;;      ((string= "Change" specification)
;;       (p4-buffer-mode)
;;       (turn-on-auto-fill)
;;       (re-search-forward "Description:\n\t" nil t))
;;      ((string= "User" specification)   (p4-buffer-mode))
;;      ((string= "Client" specification) (p4-buffer-mode) (auto-fill-mode 0))
;;      )))
;; (add-hook 'find-file-hooks 'p4-pick-mode-hook)
;; (eval-after-load "p4"
;;   '(add-hook 'p4-async-command-hook 'p4-pick-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; svn

(autoloads "psvn"
           'svn-status)

(eval-when-compile (require 'psvn))
(eval-after-load "psvn"
  '(setq svn-status-verbose nil))

(define-generic-mode 'svn-commit-mode
  nil
  nil
  (list
   (list "\\<JOB-[0-9]+\\>" 0 'font-lock-string-face)
   (list "^\\(--This line,.*--\\)" 1 'font-lock-comment-face)
   (list "^\\(A    .+\\)" 1 'font-lock-constant-face)
   (list "^\\(D    .+\\)" 1 'font-lock-type-face)
   (list "^\\([^AD]    .+\\)" 1 'font-lock-keyword-face))
  (list "/svn-commit\\(\\.[0-9]\\)?\\.tmp")
  (list (lambda ()
          (modify-syntax-entry ?/ ".")))
  "Generic mode for svn-commit buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abtags

(autoload 'abtags-key-map "abtags" "abtags-key-map" nil `keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xml

;;(load "rng-auto.el")
(eval-when-compile (require 'nxml-mode))
(defun my-nxml-setup ()
  (copy-face 'font-lock-string-face        'nxml-delimited-data-face)
  (copy-face 'font-lock-type-face          'nxml-name-face)
  (copy-face 'font-lock-constant-face      'nxml-ref-face)
  (copy-face 'font-lock-function-name-face 'nxml-delimiter-face)
  (copy-face 'font-lock-comment-face       'nxml-comment-content-face)
  (copy-face 'font-lock-constant-face       'nxml-comment-delimiter-face)
  (modify-syntax-entry ?= ".")
  (modify-syntax-entry ?& "w")
  ;; (modify-syntax-entry ?< ".")
  ;; (modify-syntax-entry ?> ".")
  ;; (modify-syntax-entry ?& ".")
  (setq nxml-child-indent 2)
  (setq nxml-slash-auto-complete-flag t))
(eval-after-load "nxml-mode"
  '(add-hook 'nxml-mode-hook 'my-nxml-setup))

(defun clever-nxml-tab (arg)
  "Tab that either indents or nxml completes."
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   ;;    (nxml-complete))
   (t (indent-for-tab-command))))

(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|tld\\|jsp\\|tag\\|xul\\|htm\\|html\\|rhtml\\)\\'" . nxml-mode))
(fset 'html-mode 'nxml-mode)
(fset 'sgml-mode 'nxml-mode)


;;
;; this goo is for SQL highlighting inside XML files
;;

(eval-after-load "nxml-mode"
  '(progn
     (put 'data          'nxml-fontify-rule '(my-nxml-fontify-region))
     (put 'cdata-section 'nxml-fontify-rule '(my-nxml-fontify-region))

     (defun my-nxml-fontify-region (start end)
       (save-excursion
         (let (matcher)
           (dolist (keyword sql-added-font-lock-keywords)
             (goto-char start)
             (while (and (< (point) end)
                         (re-search-forward (car keyword) end t))
               (put-text-property (match-beginning 0) (match-end 0)
                                  'face (cdr keyword)))))))

     (defun nxml-apply-fontify-rule (&optional type start end)
       (let ((rule (get (or type xmltok-type) 'nxml-fontify-rule)))
         (unless start (setq start xmltok-start))
         (unless end (setq end (point)))
         (while rule
           (let* ((action (car rule)))
             (setq rule (cdr rule))
             (cond ((vectorp action)
                    (nxml-set-face (let ((offset (aref action 0)))
                                     (cond ((not offset) start)
                                           ((< offset 0) (+ end offset))
                                           (t (+ start offset))))
                                   (let ((offset (aref action 1)))
                                     (cond ((not offset) end)
                                           ((< offset 0) (+ end offset))
                                           (t (+ start offset))))
                                   (aref action 2)))
                   ((and (consp action)
                         (eq (car action) 'element-qname))
                    (when xmltok-name-end ; maybe nil in partial-end-tag case
                      (nxml-fontify-qname (+ start (cdr action))
                                          xmltok-name-colon
                                          xmltok-name-end
                                          'nxml-element-prefix-face
                                          'nxml-element-colon-face
                                          'nxml-element-local-name-face)))

                                        ; added by amd
                   ((functionp action)
                    (funcall action start end))

                   ((eq action 'attributes)
                    (nxml-fontify-attributes))
                   ((eq action 'processing-instruction-content)
                    (nxml-set-face (+ start 2)
                                   xmltok-name-end
                                   'nxml-processing-instruction-target-face)
                    (nxml-set-face (save-excursion
                                     (goto-char xmltok-name-end)
                                     (skip-chars-forward " \t\r\n")
                                     (point))
                                   (- end 2)
                                   'nxml-processing-instruction-content-face))
                   ((eq action 'char-ref)
                    (nxml-char-ref-display-extra start
                                                 end
                                                 (xmltok-char-number start end)))
                   (t (error "Invalid nxml-fontify-rule action %s" action)))))))
     ))

(eval-after-load "rng-valid"
  '(progn
     (defun rng-process-tag-name ()
       (let* ((prefix (xmltok-start-tag-prefix))
              (local-name (xmltok-start-tag-local-name))
              (name
               (if prefix
                   (let ((ns (nxml-ns-get-prefix prefix)))
                     (cond (ns (cons ns local-name))
                           ((and (setq ns
                                       (rng-match-infer-start-tag-namespace
                                        local-name))
                                 (rng-match-start-tag-open (cons ns local-name)))
                            (nxml-ns-set-prefix prefix ns)
                            (rng-mark-start-tag-close "Missing xmlns:%s=\"%s\""
                                                      prefix
                                                      (nxml-namespace-name ns))
                            nil)
                           ((member prefix '("c" "decorator" "html" "fmt" "fn"
                                             "jfn" "javascript" "jsp"
                                             "pro" "report" "spring" "x" "str"
                                             "rss"))
                            (rng-match-unknown-start-tag-open)
                            nil)
                           (t
                            (rng-recover-bad-element-prefix)
                            nil)))
                 (cons (nxml-ns-get-default) local-name))))
         (when (and name
                    (not (rng-match-start-tag-open name)))
           (unless (and (not (car name))
                        (let ((ns (rng-match-infer-start-tag-namespace (cdr name))))
                          (and ns
                               (rng-match-start-tag-open (cons ns local-name))
                               (progn
                                 (nxml-ns-set-default ns)
                                 ;; XXX need to check we don't have xmlns=""
                                 (rng-mark-start-tag-close "Missing xmlns=\"%s\""
                                                           (nxml-namespace-name ns))
                                 t))))
             (rng-recover-start-tag-open name)))
         (cons prefix local-name)))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copyright-update settings.

;; (eval-when-compile (require 'copyright))
;; (if (getenv "ORGANIZATION")
;;     ;; The default doesn't account for (c), so we fix that...
;;     (setq copyright-regexp (concat "\\([\251\201\251]\\|"
;;                 "@copyright{}\\|"
;;                 "[Cc]opyright\\s *:?\\s *([Cc])\\|"
;;                 "[Cc]opyright\\s *:?\\s *[\251\201\251]\\)"
;;                 "\\s *\\([1-9][-0-9, ']*[0-9]+\\)"))
;;   (add-hook 'write-file-hooks 'copyright-update))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color-theme

(autoload 'color-theme-select "color-theme" "color-theme" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; antlr

(autoload 'antlr-mode "antlr-mode" nil t)
(setq auto-mode-alist (cons '("\\.g\\'" . antlr-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn in image modes

(auto-image-file-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby mode

(eval-when-compile (require 'ruby-mode)
                   ;; (require 'ruby-electric)
		   )

(defun my-ruby-setup ()
  (setq indent-tabs-mode nil)
  (define-key ruby-mode-map "\C-m" 'newline-and-indent)
  ;; (require 'ruby-electric)
  ;; (ruby-electric-mode)
  )
(add-hook 'ruby-mode-hook 'my-ruby-setup)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("/Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml\\'$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(define-generic-mode 'yaml-mode
  (list ?\#)
  nil
  (list
   '("^[ \t]*\\(.+\\)[ \t]*:[ \r\n]" 0 font-lock-type-face)
   '("\\(%YAML\\|# \\(.*\\)\\|\\(---\\|\\.\\.\\.\\)\\(.*\\)\\)" 0 font-lock-comment-face)
   '("\\(\\*\\|\\&\\)\\(.*\\)" 0 (cons font-lock-variable-name-face '(underline)))
   '("\\!\\!\\sw+[ \r\n]" 0 font-lock-function-name-face)
   )
  (list "\\.yml$")
  nil
  "Generic mode for yaml files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp

(setq tramp-default-method "ssh")
