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

(require 'filladapt)
(setq filladapt-mode-line-string nil)
(setq-default filladapt-mode t)


(add-to-list 'auto-mode-alist '("\\.\\(c\\|h\\|cpp\\|uc\\)$" . c++-mode))
(add-to-list 'auto-mode-alist '("/\\(MAP\\|VECTOR\\|LIST\\|XTREE\\|XMEMORY\\|FUNCTIONAL\\)$" . c++-mode))

;; objc

(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))


(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))


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
;; svn

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
