;;; perinf-person.el --- Person workflow for Personal Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'perinf-i18n)
(require 'perinf-storage)

;;;###autoload
(defun perinf-person-create ()
  "Interactively create a person in the current Personal Information System project."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((name (read-string (perinf-i18n 'person.name-prompt)))
         (email (read-string (perinf-i18n 'person.email-prompt)))
         (phone (read-string (perinf-i18n 'person.phone-prompt)))
         (person
          (perinf-storage-create
           'person
           `((title . ,name)
             (email . ,email)
             (phone . ,phone))
           perinf-current-project)))
    (message "%s" (perinf-i18n 'person.created))
    (when (fboundp 'perinf-core-records)
      (perinf-core-records))
    person))

(provide 'perinf-person)

;;; perinf-person.el ends here
