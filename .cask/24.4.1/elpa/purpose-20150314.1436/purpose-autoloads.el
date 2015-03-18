;;; purpose-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "purpose" "purpose.el" (21769 14572 842668
;;;;;;  814000))
;;; Generated autoloads from purpose.el

(defvar purpose-mode nil "\
Non-nil if Purpose mode is enabled.
See the command `purpose-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `purpose-mode'.")

(custom-autoload 'purpose-mode "purpose" nil)

(autoload 'purpose-mode "purpose" "\
Toggle Purpose mode on or off.
With a prefix argument ARG, enable Purpose mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{purpose-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "purpose-configuration" "purpose-configuration.el"
;;;;;;  (21769 14572 842668 814000))
;;; Generated autoloads from purpose-configuration.el

(eieio-defclass-autoload 'purpose-conf 'nil "purpose-configuration" nil)

(autoload 'purpose-set-extension-configuration "purpose-configuration" "\
Set an extension's entry in `purpose-extended-configuration'.
KEYWORD should be a keyword used to identify the extension.
CONFIG is a `purpose-conf' object containing the extension's purpose 
configuration.
Example:
 (purpose-set-extension-configuration
     :python
     (purpose-conf :mode-purposes
                   '((python-mode . python)
                     (python-inferior-mode . interpreter))))

This function calls `purpose-compile-extended-configuration' when its
done.

\(fn KEYWORD CONFIG)" nil nil)

;;;***

;;;### (autoloads nil "purpose-x" "purpose-x.el" (21769 14572 850668
;;;;;;  713000))
;;; Generated autoloads from purpose-x.el

(autoload 'purpose-x-code1-setup "purpose-x" "\
Setup purpose-x-code1.
This setup includes 4 windows:
1. dedicated 'edit window
2. dedicated 'dired window.  This window shows the current buffer's
directory in a special window, using `dired' and
`dired-hide-details-mode' (if available).
3. dedicated 'buffers window.  This window shows the currently open
files, using `ibuffer'.
4. dedicated 'ilist window.  This window shows the current buffer's
imenu.

\(fn)" t nil)

(autoload 'purpose-x-magit-single-on "purpose-x" "\
Turn on magit-single purpose configuration.

\(fn)" t nil)

(autoload 'purpose-x-magit-multi-on "purpose-x" "\
Turn on magit-multi purpose configuration.

\(fn)" t nil)

(autoload 'purpose-x-golden-ratio-setup "purpose-x" "\
Make `golden-ratio-mode' aware of `purpose-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("purpose-core.el" "purpose-fixes.el" "purpose-layout.el"
;;;;;;  "purpose-pkg.el" "purpose-prefix-overload.el" "purpose-switch.el"
;;;;;;  "purpose-utils.el") (21769 14572 857469 857000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; purpose-autoloads.el ends here
