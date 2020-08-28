;; (setq debug-on-error t)

(defun path-with-home (path)
  (format "%s/%s" (getenv "HOME") path))

(defvar AMDELISP (format "%s/.emacs.d/amdelisp" (getenv "HOME")))

(require 'package)

(if (< emacs-major-version 24)
    ;; older
    (progn
      (load (format "%s/package" AMDELISP))
      (load (path-with-home ".emacs.d/cl-lib/cl-lib"))
      (setq required-packages
            '(haskell-mode
              magit
              mmm-mode
              oauth2
              org
              paredit
              protobuf-mode)))
  ;; emacs 24+
  (progn
    (require 'cl-lib)
    (setq required-packages
          '(cider
            clojure-mode
            company-emoji
            dockerfile-mode
            elixir-mode
            exec-path-from-shell
            go-mode
            gradle-mode
            graphql-mode
            groovy-mode
            haskell-mode
            haml-mode
            flycheck-haskell
            shakespeare-mode
            js2-mode
            lsp-mode
            magit
            markdown-mode
            mix
            mmm-mode
            oauth2
            org
            org-present
            org-trello
            paredit
            protobuf-mode
            ruby-electric
            sass-mode
            swift-mode
            thrift
            typescript-mode))))

(add-to-list 'exec-path
             (format "%s/bin" (getenv "HOME")))


(setenv "PATH" (concat (getenv "PATH")
                       (format ":%s/bin" (getenv "HOME"))))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "https://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;; install missing packages
(let ((not-installed (cl-remove-if 'package-installed-p required-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))

(load (format "%s/start" AMDELISP))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look and feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my colors, defined in prefs.el
(turn-on-color-theme-amd)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; spelling
(setq
 ispell-extra-args '("--mode=sgml")
 ispell-program-name "aspell"
 ispell-silently-savep t)
(set-default 'ispell-skip-html t)

;; turn on global-auto-revert-mode
(global-auto-revert-mode 1)
(setq global-auto-revert-mode-text "-AR"
      auto-revert-interval 5)

;; but leave it off for TAGS files
(defun turn-off-auto-revert-hook ()
  "If this is a TAGS file, turn off auto-revert."
  (when (string= (buffer-name) "TAGS")
    (setq global-auto-revert-ignore-buffer t)))
(add-hook 'find-file-hooks 'turn-off-auto-revert-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 abtags-keymap-prefix nil
 backward-delete-char-untabify-method 'all
 comint-input-ring-size 99
 completion-ignore-case t
 html-helper-do-write-file-hooks nil
 shell-dirtrack-verbose nil
 sort-fold-case t
 sql-oracle-program "sqlplus"
 tags-add-tables t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-buffer-nocreate ()
  "Switch to a buffer but don't create one if it doesn't exist."
  (interactive)
  (switch-to-buffer (read-buffer "Switch to buffer " (other-buffer) "t")))

;; ;; overridden to modify compilation-search-path
;; (defun compilation-find-file (marker filename dir &rest formats)
;;   "Find a buffer for file FILENAME.
;; Search the directories in `compilation-search-path'.
;; A nil in `compilation-search-path' means to try the
;; current directory, which is passed in DIR.
;; If FILENAME is not found at all, ask the user where to find it.
;; Pop up the buffer containing MARKER and scroll to MARKER if we ask the user."
;;   (or formats (setq formats '("%s")))
;;   (save-excursion
;;     (let ((dirs compilation-search-path)
;;           buffer thisdir fmts name)
;;       (if (file-name-absolute-p filename)
;;           ;; The file name is absolute.  Use its explicit directory as
;;           ;; the first in the search path, and strip it from FILENAME.
;;           (setq filename (abbreviate-file-name (expand-file-name filename))
;;                 dirs (cons (file-name-directory filename) dirs)
;;                 filename (file-name-nondirectory filename)))
;;       ;; Now search the path.
;;       (while (and dirs (null buffer))
;;         (setq thisdir (or (car dirs) dir)
;;               fmts formats)
;;         ;; For each directory, try each format string.
;;         (while (and fmts (null buffer))
;;           (setq name (expand-file-name (format (car fmts) filename) thisdir)
;;                 buffer (and (file-exists-p name)
;;                             (find-file-noselect name))
;;                 fmts (cdr fmts)))
;;         (setq dirs (cdr dirs)))
;;       (or buffer
;;           ;; The file doesn't exist.
;;           ;; Ask the user where to find it.
;;           ;; If he hits C-g, then the next time he does
;;           ;; next-error, he'll skip past it.
;;           (let* ((pop-up-windows t)
;;                  (w (display-buffer (marker-buffer marker))))
;;             (set-window-point w marker)
;;             (set-window-start w marker)
;;             (let ((name (expand-file-name
;;                          (read-file-name
;;                           (format "Find this error in: (default %s) "
;;                                   filename)
;;                           dir filename t))))
;;               (if (file-directory-p name)
;;                   (setq name (expand-file-name filename name)))

;;               ;; amd
;;               (setq compilation-search-path
;;                     (cons (file-name-directory name) compilation-search-path))

;;               (setq buffer (and (file-exists-p name)
;;                                 (find-file name))))))
;;       ;; Make intangible overlays tangible.
;;       (mapcar (function (lambda (ov)
;;                           (when (overlay-get ov 'intangible)
;;                             (overlay-put ov 'intangible nil))))
;;               (overlays-in (point-min) (point-max)))
;;       buffer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f7]  'abtags-find-file)
(global-set-key [f8]  'grep)
(global-set-key [f12] 'next-error)
(global-set-key "\C-xb" 'switch-to-buffer-nocreate)
(global-set-key "\C-\M-q" 'backward-up-list-indent)
(global-set-key "\M-," 'tags-search-tags-table)

;; use TAB key for completion everywhere

(global-set-key-override0 "\t" 'clever-hippie-tab)
(global-set-key-override "\t" 'clever-nxml-tab 'nxml-mode)

(add-hook 'nxml-mode 'hexcolour-add-to-font-lock)

(autoload 'org-present "org-present" nil t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(autoload 'awk-mode "cc-mode" nil t)

(when window-system (require 'linum))

(defun amd-tab-table (desc str)
  (let ((section (car (cdr desc))))
    (if (string-match (car desc) str)
        (substring str (match-beginning section) (match-end section))
      nil)))

(define-generic-mode 'dat-mode
  nil
  nil
  '(("^\\([^\t]*\\)\t\\([^\t]*\\)\t\\([^\t]*\\)\t\\([^\t]*\\)\t"
     (1 font-lock-reference-face) (2 font-lock-type-face)
     (3 font-lock-reference-face) (4 font-lock-type-face)))
  (list "\\.dat$")
  nil
  "Generic mode for dat files.")

; overridden to allow empty passwords
(eval-after-load "sql"
  '(defun sql-read-passwd (prompt &optional default)
     "Read a password using PROMPT.  Optional DEFAULT is password to start with."
     (read-passwd prompt nil (if (equal default "yyy")
                                 nil
                               default))))

(defun communicate-colors ()
  (interactive)
  (set-face-foreground 'modeline "white")
  (set-face-background 'modeline "red"))

;; (load "regex-tool" t)

(keyboard-translate ?\C-h ?\C-?)
(global-set-key-override "\177" 'backward-delete-char-untabify)

;; yes/no
(fset 'yes-or-no-p 'y-or-n-p)


;; to set this in .Xdefaults, use:
;; Emacs*font: dejavu sans mono-10
;; (when (member "DejaVu Sans Mono" (font-family-list))
;;   (set-default-font "DejaVu Sans Mono-10"))


(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

(mapc
 (lambda (mode)
   (let ((hook (intern (concat (symbol-name mode)
			       "-mode-hook"))))
     (add-hook hook (lambda () (paredit-mode +1)))))
 '(emacs-lisp lisp inferior-lisp slime slime-repl lisp-interaction clojure nrepl))

;; ;; twittering-mode
;; ;;(add-to-list 'load-path (path-with-home "code/twittering-mode"))
;; ;; (require 'twittering-mode)
;; ;; (setq twittering-use-master-password t)      ; Save OAuth token
;; ;; (setq twittering-icon-mode t)                ; Show icons
;; ;; (setq twittering-timer-interval (* 60 60))   ; Update automatically only once an hour

(setq auto-mode-alist
      (cl-concatenate 'list auto-mode-alist
                   '(
                     ;; generic Ruby extras
                     (".eye" . ruby-mode)
                     ("Vagrantfile" . ruby-mode)
                     (".eye.j2" . ruby-mode)
                     ("Gemfile" . ruby-mode)
                     ("\\.mirah" . ruby-mode)

                     ("\\.eex" . elixir-mode)
                     ("\\.ex" . elixir-mode)
                     ("\\.exs" . elixir-mode)
                     ("\\.go" . go-mode)

                     ;; bazel
                     ("WORKSPACE" . python-mode)
                     ("\\.bazel" . python-mode)
                     ("\\.bzl" . python-mode)
                     ;; buck
                     ("\\.bucklet" . python-mode)


                     ("\\.graphql" . graphql-mode)
                     ("\\.gradle" . groovy-mode)
                     ("\\.gradle" . gradle-mode)

                     ("\\.js.erb" . javascript-mode)
                     ("\\.jsx" . javascript-mode)

                     ("\\.markdown" . markdown-mode)
                     ("\\.md" . markdown-mode)
                     ("\\.org$" . org-mode)
                     ("\\.scss" . sass-mode)
                     ("\\.ts" . typescript-mode)
                     )))

(require 'lsp-mode)
;;(add-hook 'go-mode-hook #'lsp)

;; (use-package lsp-mode
;;              :commands lsp
;;              :init
;;              (lsp-register-client
;;               (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
;;                                :major-modes '(go-mode)
;;                                :server-id 'gopls)))

;; org-mode
;; TODO: emoji?
;; (add-hook 'org-mode-hook 'company-mode)
;; (add-hook 'org-mode-hook 'company-emoji-init)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-agenda-files (setq org-agenda-files '("~/Documents/org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(magit-status-buffer-switch-function 'pop-to-buffer)
 '(org-trello-current-prefix-keybinding "C-c o")
 '(org-trello-files '("~/Documents/org/todo.org"))
 '(package-selected-packages
   '(mix scala-mode flow-js2-mode prettier-js typescript-mode lsp-mode dhall-mode org-trello org-present exec-path-from-shell graphql-mode markdown-mode thrift swift-mode shakespeare-mode sass-mode ruby-electric protobuf-mode paredit oauth2 mmm-mode magit js2-mode groovy-mode gradle-mode flycheck-haskell elixir-mode company-emoji cider))
 '(safe-local-variable-values
   '((haskell-indent-spaces . 4)
     (haskell-indent-spaces . 2)
     (haskell-process-use-ghci . t)
     (hamlet/basic-offset . 2))))


;; define categories that should be excluded
(setq org-export-exclude-category (list "home" "contracting" "private" "work"))

;; export all scheduled TODOs
(setq org-icalendar-use-scheduled '(todo-start event-if-todo))

;; define filter. The filter is called on each entry in the agenda.
;; It defines a regexp to search for two timestamps, gets the start
;; and end point of the entry and does a regexp search. It also
;; checks if the category of the entry is in an exclude list and
;; returns either t or nil to skip or include the entry.

(defun org-mycal-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
;;  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (setq org-tstr-regexp org-tst-regexp)
  (save-excursion
    ;; get categories
    (setq mycategory (org-get-category))
    ;; get start and end of tree
    (org-back-to-heading t)
    (setq mystart (point))
    (org-end-of-subtree)
    (setq myend (point))
    (goto-char mystart)
    ;; search for timerange
    (setq myresult (re-search-forward org-tstr-regexp myend t))
    ;; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
    ;; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

(defun org-mycal-export-limit ()
  "Limit the export to items not in certain categories."
  (save-excursion
    (setq mycategory (org-get-category))
    (setq mycatp (member mycategory org-export-exclude-category))
    (if (and (not mycatp)) t nil)))

;;; activate filter and call export function
(defun org-mycal-export ()
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
    (org-export-icalendar-combine-agenda-files)))

(defvar org-directory (format "%s/Dropbox/" (getenv "HOME")))
;; (setq org-ghi-interesting-repos '("readabl/paperkarma" "readabl/paperkarma-server"))

;; (setq load-path (cons "~/code/org-ghi/" load-path))
;; (require 'org-ghi)

(autoload 'scpaste "scpaste" "Paste the current buffer." t nil)
(setq scpaste-http-destination "http://threebrothers.org/files"
      scpaste-scp-destination "threebrothers.org:threebrothers.org/files")

;;;;;;;;;;;;;;;;;;;;
;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; fix window splitting
(setq split-height-threshold nil)

;; nrepl
(setq nrepl-popup-stacktraces nil)

(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 2)))
(setq-default typescript-indent-level 2)
(defun my-javascript-mode-hook ()
  (setq indent-tabs-mode nil
        c-basic-offset 2
        js-indent-level 2
        ))

(add-hook 'javascript-mode-hook 'my-javascript-mode-hook)

(require 'shakespeare-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit)
(setq magit-push-always-verify nil)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(setq js-indent-level 2)
