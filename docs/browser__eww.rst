Browser - eww
=============

Emacs has build-in browser *eww* and I used it for reading articles
for its inability to process multi-media content and sometimes I turn
off the display of image, so I can fully focus on the context. 

.. code-block:: scheme

    ;; http://emacs.stackexchange.com/questions/561/how-can-i-toggle-displaying-images-in-eww-without-a-page-refresh
    (defvar-local endless/display-images t)

    (defun endless/toggle-image-display ()
      "Toggle images display on current buffer."
      (interactive)
      (setq endless/display-images
            (null endless/display-images))
      (endless/backup-display-property endless/display-images))

    (defun endless/backup-display-property (invert &optional object)
      "Move the 'display property at POS to 'display-backup.
    Only applies if display property is an image.
    If INVERT is non-nil, move from 'display-backup to 'display
    instead.
    Optional OBJECT specifies the string or buffer. Nil means current
    buffer."
      (let* ((inhibit-read-only t)
             (from (if invert 'display-backup 'display))
             (to (if invert 'display 'display-backup))
             (pos (point-min))
             left prop)
        (while (and pos (/= pos (point-max)))
          (if (get-text-property pos from object)
              (setq left pos)
            (setq left (next-single-property-change pos from object)))
          (if (or (null left) (= left (point-max)))
              (setq pos nil)
            (setq prop (get-text-property left from object))
            (setq pos (or (next-single-property-change left from object)
                          (point-max)))
            (when (eq (car prop) 'image)
              (add-text-properties left pos (list from nil to prop) object))))))
