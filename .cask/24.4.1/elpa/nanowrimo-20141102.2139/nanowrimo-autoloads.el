;;; nanowrimo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "nanowrimo" "nanowrimo.el" (21748 25926 696173
;;;;;;  811000))
;;; Generated autoloads from nanowrimo.el

(autoload 'nanowrimo-mode "nanowrimo" "\
Display the number of words, WPM and estimate to finish in the mode line.

When called with prefix-argument, set today's goal to that value
instead of calling `nanowrimo-today-goal-calculation-function'.

\(fn &optional ARG)" t nil)

(autoload 'nanowrimo-insert-org-table "nanowrimo" "\
Insert an org-mode table for keeping track of progress.
If a table with a name of `nanowrimo-org-table-name' already exists
then it is merely updated to contain the correct number of days.

\(fn)" t nil)

(autoload 'nanowrimo-update-nanowrimo-org "nanowrimo" "\
Send the current word count to nanowrimo.org.
If WC is non-nil, use the numeric value as the word count.
Otherwise, send the value returned by `nanowrimo-count-words'.
If the variables `nanowrimo-username' or `nanowrimo-api-key' are
nil, the user will be prompted for values to be used (but not
stored).

\(fn WC)" t nil)

(autoload 'nanowrimo-redact-region "nanowrimo" "\
Convert all letters in the region to x and all numbers to 9.

The result is then suitable for sending to the word count
function of nanowrimo.org without fear that someone will
intercept your masterpiece.

\(fn BEG END)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nanowrimo-autoloads.el ends here
