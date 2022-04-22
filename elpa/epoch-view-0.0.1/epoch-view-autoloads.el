;;; epoch-view-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "epoch-view" "epoch-view.el" (0 0 0 0))
;;; Generated autoloads from epoch-view.el

(autoload 'epoch-view-mode "epoch-view" "\
Visualize epoch (Unix) timestamps.

If called interactively, enable Epoch-View mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epoch-view" '("epoch-view-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; epoch-view-autoloads.el ends here
