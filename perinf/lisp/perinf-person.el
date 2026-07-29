;;; perinf-person.el --- Person workflow for Personal Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'perinf-i18n)
(require 'perinf-storage)
(require 'seq)

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

(defun perinf-person--find (person-id)
  "Return PERSON-ID from the current project or signal an error."
  (or
   (seq-find
    (lambda (person) (equal (perinf-object-id person) person-id))
    (perinf-storage-list 'person perinf-current-project))
   (signal 'perinf-object-not-found (list person-id))))

(defun perinf-person-archive (person-id)
  "Archive PERSON-ID while preserving its historical references."
  (interactive)
  (let ((person (perinf-person--find person-id)))
    (when
        (yes-or-no-p
         (format (perinf-i18n 'person.archive-confirmation)
                 (perinf-object-title person)))
      (perinf-storage-set-person-status
       person-id 'inactive perinf-current-project)
      (message "%s" (perinf-i18n 'person.archived))
      (when (fboundp 'perinf-core-administration)
        (perinf-core-administration)))))

(defun perinf-person-reactivate (person-id)
  "Reactivate archived PERSON-ID."
  (interactive)
  (let ((person (perinf-person--find person-id)))
    (when
        (yes-or-no-p
         (format (perinf-i18n 'person.reactivate-confirmation)
                 (perinf-object-title person)))
      (perinf-storage-set-person-status
       person-id 'active perinf-current-project)
      (message "%s" (perinf-i18n 'person.reactivated))
      (when (fboundp 'perinf-core-administration)
        (perinf-core-administration)))))

(defun perinf-person-delete (person-id)
  "Permanently delete unreferenced PERSON-ID after explicit confirmation."
  (interactive)
  (let* ((person (perinf-person--find person-id))
         (references
          (perinf-storage-person-references
           person-id perinf-current-project))
         (task-count (length (plist-get references :tasks)))
         (meeting-count (length (plist-get references :meetings))))
    (if (or (> task-count 0) (> meeting-count 0))
        (user-error
         (perinf-i18n 'person.delete-blocked)
         task-count meeting-count)
      (when
          (yes-or-no-p
           (format (perinf-i18n 'person.delete-confirmation)
                   (perinf-object-title person)))
        (perinf-storage-delete-person person-id perinf-current-project)
        (message "%s" (perinf-i18n 'person.deleted))
        (when (fboundp 'perinf-core-administration)
          (perinf-core-administration))))))

(provide 'perinf-person)

;;; perinf-person.el ends here
