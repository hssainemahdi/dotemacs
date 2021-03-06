;;; npm-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "npm-mode" "npm-mode.el" (0 0 0 0))
;;; Generated autoloads from npm-mode.el

(autoload 'npm-mode "npm-mode" "\
Minor mode for working with npm projects.

If called interactively, enable Npm mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'npm-global-mode 'globalized-minor-mode t)

(defvar npm-global-mode nil "\
Non-nil if Npm-Global mode is enabled.
See the `npm-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `npm-global-mode'.")

(custom-autoload 'npm-global-mode "npm-mode" nil)

(autoload 'npm-global-mode "npm-mode" "\
Toggle Npm mode in all buffers.
With prefix ARG, enable Npm-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Npm mode is enabled in all buffers where
`npm-mode' would do it.
See `npm-mode' for more information on Npm mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "npm-mode" '("npm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; npm-mode-autoloads.el ends here
