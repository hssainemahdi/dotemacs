;; init.el --- Emacs configuration

;; EMACS AUTO AUTH PROXY
;; --------------------------------------

(setq package-check-signature nil)

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
;; Create site-lisp directory if it does not exist
(if (not (file-directory-p "~/.emacs.d/site-lisp"))
    (make-directory "~/.emacs.d/site-lisp"))

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Print welcome message
(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Emacs is powering up... Be patient, Master %s!" current-user)

;; Load proxy settings
(setq proxy-file "~/.emacs.d/site-lisp/proxy.el")
(when (file-exists-p proxy-file)
  (load proxy-file))

;; Make emacs start in fullscreen mode (usefull for EXWM)
(set-frame-parameter nil 'fullscreen 'fullboth)

;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is all kinds of necessary
(require 'package)
(setq package-enable-at-startup nil)

;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
(package-refresh-contents)
(package-install 'use-package)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Load custom theme
(add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-dark t)))

;; Load configuration from settings.org file
(require 'org)
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)

(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)


(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(define-key python-mode-map (kbd "C-c C-i") 'pyimport-insert-missing)
