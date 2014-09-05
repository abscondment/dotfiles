(defun path-with-home (path)
  (format "%s/%s" (getenv "HOME") path))

(defvar AMDELISP (format "%s/.emacs.d/amdelisp" (getenv "HOME")))
(load (format "%s/start" AMDELISP))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look and feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my colors, defined in prefs.el
(turn-on-color-theme-amd)
  
;; put the window on the right, make it 87 columns wide. Also, enable
;; the mouse wheel.
(when window-system
  (setq window-position 'right
        window-columns 87
        window-fudge '(0 12 0 55))

  (when is-win32
    (setq my-font (window-build-font "Andale" 10)))
  
  ;;  (setq my-font (window-build-font "Hells Programmer" 8))
  ;;  (setq my-font (window-build-font "Sheldon Narrow" 8))

  ;; turn on the mouse wheel
  (mouse-wheel-mode t)

  ;; blink the cursor
  (blink-cursor-mode 1)

  ;; highlight line-mode
  (global-hl-line-mode)
  )

;; ;; i don't like menus...
(menu-bar-mode 0)

;; personally i like transient-mark-mode
(transient-mark-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'complete)
(partial-completion-mode t)

;; turn of p4-check-mode unless absolutely necessary
(setq p4-file-refresh-timer-time 0
      p4-do-find-file nil)

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
 tags-add-tables t
 )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-buffer-nocreate ()
  "Switch to a buffer but don't create one if it doesn't exist."
  (interactive)
  (switch-to-buffer (read-buffer "Switch to buffer " (other-buffer) "t")))

;; hack to clear comint buffer when I use certain commands
(defun my-comint-filter (x)
  (when (string-match "\\(startup\\|units\\)\n" x)
    (kill-region (point-min) (point-max))
    (insert (format "(buffer cleared by my-comint-filter)\n> %s" x))))
(add-hook 'shell-mode-hook '(lambda () (add-hook 'comint-input-filter-functions 'my-comint-filter nil t)))

;; overridden to modify compilation-search-path
(defun compilation-find-file (marker filename dir &rest formats)
  "Find a buffer for file FILENAME.
Search the directories in `compilation-search-path'.
A nil in `compilation-search-path' means to try the
current directory, which is passed in DIR.
If FILENAME is not found at all, ask the user where to find it.
Pop up the buffer containing MARKER and scroll to MARKER if we ask the user."
  (or formats (setq formats '("%s")))
  (save-excursion
    (let ((dirs compilation-search-path)
          buffer thisdir fmts name)
      (if (file-name-absolute-p filename)
          ;; The file name is absolute.  Use its explicit directory as
          ;; the first in the search path, and strip it from FILENAME.
          (setq filename (abbreviate-file-name (expand-file-name filename))
                dirs (cons (file-name-directory filename) dirs)
                filename (file-name-nondirectory filename)))
      ;; Now search the path.
      (while (and dirs (null buffer))
        (setq thisdir (or (car dirs) dir)
              fmts formats)
        ;; For each directory, try each format string.
        (while (and fmts (null buffer))
          (setq name (expand-file-name (format (car fmts) filename) thisdir)
                buffer (and (file-exists-p name)
                            (find-file-noselect name))
                fmts (cdr fmts)))
        (setq dirs (cdr dirs)))
      (or buffer
          ;; The file doesn't exist.
          ;; Ask the user where to find it.
          ;; If he hits C-g, then the next time he does
          ;; next-error, he'll skip past it.
          (let* ((pop-up-windows t)
                 (w (display-buffer (marker-buffer marker))))
            (set-window-point w marker)
            (set-window-start w marker)
            (let ((name (expand-file-name
                         (read-file-name
                          (format "Find this error in: (default %s) "
                                  filename)
                          dir filename t))))
              (if (file-directory-p name)
                  (setq name (expand-file-name filename name)))

              ;; amd
              (setq compilation-search-path
                    (cons (file-name-directory name) compilation-search-path))
	       
              (setq buffer (and (file-exists-p name)
                                (find-file name))))))
      ;; Make intangible overlays tangible.
      (mapcar (function (lambda (ov)
                          (when (overlay-get ov 'intangible)
                            (overlay-put ov 'intangible nil))))
              (overlays-in (point-min) (point-max)))
      buffer)))



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
(global-set-key-override  "\t" 'clever-nxml-tab 'nxml-mode)



(autoload 'awk-mode "cc-mode" nil t)


(when window-system
  (require 'linum)
  (setq window-position 'right
        window-columns 240
        window-fudge '(5 5 20 5)))

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

(load "regex-tool" t)

(when (not is-win32)
  (keyboard-translate ?\C-h ?\C-?))
(global-set-key-override "\177" 'backward-delete-char-untabify)

;; yes/no
(fset 'yes-or-no-p 'y-or-n-p)


; to set this in .Xdefaults, use:
; Emacs*font: dejavu sans mono-10
(set-default-font "DejaVu Sans Mono-10")


(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

(mapc
 (lambda (mode)   
   (let ((hook (intern (concat (symbol-name mode)   
                               "-mode-hook"))))   
     (add-hook hook (lambda () (paredit-mode +1)))))   
 '(emacs-lisp lisp inferior-lisp slime slime-repl lisp-interaction clojure nrepl))

;; twittering-mode
;;(add-to-list 'load-path (path-with-home "code/twittering-mode"))
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)      ; Save OAuth token
;; (setq twittering-icon-mode t)                ; Show icons
;; (setq twittering-timer-interval (* 60 60))   ; Update automatically only once an hour


(add-to-list 'load-path (path-with-home "code/markdown-mode"))
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (concatenate 'list
                   auto-mode-alist 
                   '(("\\.markdown" . markdown-mode)
                     ("\\.md" . markdown-mode)
                     ("\\.mirah" . ruby-mode)
                     ("Vagrantfile" . ruby-mode)
                     ("Gemfile" . ruby-mode)
                     ("\\.org$" . org-mode))))

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-agenda-files (setq org-agenda-files '("~/Documents/org")))

;;; define categories that should be excluded
;(setq org-export-exclude-category (list "home" "contracting" "private" "work"))

;;; export all scheduled TODOs
(setq org-icalendar-use-scheduled '(todo-start event-if-todo))

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.

;; (defun org-mycal-export-limit ()
;;   "Limit the export to items that have a date, time and a range. Also exclude certain categories."
;;   (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
;; ;;  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
;;   (setq org-tstr-regexp org-tst-regexp)
;;   (save-excursion
;;     ;; get categories
;;     (setq mycategory (org-get-category))
;;     ;; get start and end of tree
;;     (org-back-to-heading t)
;;     (setq mystart (point))
;;     (org-end-of-subtree)
;;     (setq myend (point))
;;     (goto-char mystart)
;;     ;; search for timerange
;;     (setq myresult (re-search-forward org-tstr-regexp myend t))
;;     ;; search for categories to exclude
;;     (setq mycatp (member mycategory org-export-exclude-category))
;;     ;; return t if ok, nil when not ok
;;     (if (and myresult (not mycatp)) t nil)))

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

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)))

(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

(require 'magit)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-status-buffer-switch-function (quote pop-to-buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setenv "SSH_AUTH_SOCK" (concat (getenv "HOME") "/.ssh-auth-sock"))
