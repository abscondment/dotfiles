;;;
;;; This file is only loaded when window-system is true.
;;;

(require 'cl-lib)
(require 'font-lock)

(defun window-build-font (family points)
  "Given a font family and a point size, this function builds the magic
font string that emacs uses to represent that font. The mono-font-sizes
list is used to fill in the magic values for the font name."
  (let ((mono-font-sizes '((8 11 82) (9 12 90) (10 13 97)
                           (11 15 112) (12 16 120))))
    (format "-*-%s-normal-r-normal-normal-%s-%s-*-*-c-*-iso8859-15"
            family
            (nth 1 (assoc points mono-font-sizes))
            (nth 2 (assoc points mono-font-sizes)))))

;;; set or append a frame attribute in the default-frame-alist
(defun window-set-frame-default (key value)
  (let ((val (assoc key default-frame-alist)))
    (if (null val)
        (setcdr (last default-frame-alist) (list (cons key value)))
      (rplacd val value)))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function to setup the window properties

(defun window-setup ()
  (mouse-wheel-mode t)
  (blink-cursor-mode 1)
  ;; highlight line-mode
  (global-hl-line-mode)
  ;; no menus
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (transient-mark-mode t)
  (display-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Position the main window at startup.
;;; Executed after .emacs is loaded in the after-init-hook.

(defun window-layout ()
  (window-setup)
  (set-frame-size (selected-frame) 240 50))

(add-hook 'after-init-hook 'window-layout)
