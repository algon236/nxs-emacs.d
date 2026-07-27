;;; perinf-context.el --- Context workflow -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'perinf-i18n)
(require 'perinf-storage)

(defun perinf-context-create ()
  "Interactively create a context in the current project."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((title
          (string-trim
           (read-string (perinf-i18n 'context.title-prompt))))
         (description
          (string-trim
           (read-string (perinf-i18n 'context.description-prompt))))
         (context
          (perinf-storage-create
           'context
           `((title . ,title) (description . ,description))
           perinf-current-project)))
    (message "%s" (perinf-i18n 'context.created))
    (when (fboundp 'perinf-core-records)
      (perinf-core-records))
    context))

(provide 'perinf-context)

;;; perinf-context.el ends here
