;;; perinf-person.el --- Person workflow for Personal Work and Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'perinf-i18n)
(require 'perinf-storage)
(require 'seq)

;;;###autoload
(defun perinf-person-create ()
  "Interactively create a person in the current Personal Work and Information System project."
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
    (when (fboundp 'perinf-core-people)
      (perinf-core-people))
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
      (when (fboundp 'perinf-core-people)
        (perinf-core-people)))))

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
      (when (fboundp 'perinf-core-people)
        (perinf-core-people)))))

(defun perinf-person-edit (person-id)
  "Interactively edit PERSON-ID without changing its stable ID."
  (interactive)
  (let* ((person (perinf-person--find person-id))
         (properties (perinf-object-properties person))
         (name (read-string (perinf-i18n 'person.name-prompt)
                            (perinf-object-title person)))
         (email (read-string (perinf-i18n 'person.email-prompt)
                             (or (alist-get 'EMAIL properties) "")))
         (phone (read-string (perinf-i18n 'person.phone-prompt)
                             (or (alist-get 'PHONE properties) ""))))
    (perinf-storage-update-person
     person-id name email phone perinf-current-project)
    (message "%s" (perinf-i18n 'person.updated))
    (when (fboundp 'perinf-core-people) (perinf-core-people))))

(defun perinf-person--active-choices ()
  "Return completion choices for active people."
  (mapcar
   (lambda (person)
     (cons (perinf-object-title person) (perinf-object-id person)))
   (seq-filter
    (lambda (person) (eq (perinf-object-status person) 'active))
    (perinf-storage-list 'person perinf-current-project))))

(defun perinf-person--read-members (&optional initial-ids)
  "Prompt for zero or more people and return their IDs.
INITIAL-IDS are offered as the initial selection."
  (let* ((choices (perinf-person--active-choices))
         (initial-names
          (delq nil
                (mapcar (lambda (choice)
                          (and (member (cdr choice) initial-ids) (car choice)))
                        choices)))
         (selected
          (completing-read-multiple
           (perinf-i18n 'group.members-prompt) choices nil t
           (mapconcat #'identity initial-names ", "))))
    (delete-dups (mapcar (lambda (name) (cdr (assoc name choices))) selected))))

(defun perinf-person-group-create ()
  "Interactively create a reusable collection of people."
  (interactive)
  (let ((name (read-string (perinf-i18n 'group.name-prompt)))
        (member-ids (perinf-person--read-members)))
    (perinf-storage-create
     'person-group `((title . ,name) (member-ids . ,member-ids))
     perinf-current-project)
    (message "%s" (perinf-i18n 'group.created))
    (when (fboundp 'perinf-core-people) (perinf-core-people))))

(defun perinf-person-group--find (group-id)
  "Return GROUP-ID from the current project or signal an error."
  (or (seq-find
       (lambda (group) (equal (perinf-object-id group) group-id))
       (perinf-storage-list 'person-group perinf-current-project))
      (signal 'perinf-object-not-found (list group-id))))

(defun perinf-person-group-edit (group-id)
  "Interactively edit GROUP-ID and its members."
  (interactive)
  (let* ((group (perinf-person-group--find group-id))
         (name (read-string (perinf-i18n 'group.name-prompt)
                            (perinf-object-title group)))
         (member-ids
          (perinf-person--read-members
           (perinf-storage-group-member-ids group))))
    (perinf-storage-update-person-group
     group-id name member-ids perinf-current-project)
    (message "%s" (perinf-i18n 'group.updated))
    (when (fboundp 'perinf-core-people) (perinf-core-people))))

(defun perinf-person-group-archive (group-id)
  "Archive GROUP-ID while preserving it for later reactivation."
  (interactive)
  (let ((group (perinf-person-group--find group-id)))
    (when (yes-or-no-p
           (format (perinf-i18n 'group.archive-confirmation)
                   (perinf-object-title group)))
      (perinf-storage-set-person-group-status
       group-id 'inactive perinf-current-project)
      (message "%s" (perinf-i18n 'group.archived))
      (when (fboundp 'perinf-core-people) (perinf-core-people)))))

(defun perinf-person-group-reactivate (group-id)
  "Reactivate archived GROUP-ID."
  (interactive)
  (perinf-storage-set-person-group-status
   group-id 'active perinf-current-project)
  (message "%s" (perinf-i18n 'group.reactivated))
  (when (fboundp 'perinf-core-people) (perinf-core-people)))

(defun perinf-person-select-person-or-group (prompt)
  "Prompt with PROMPT for one active person or group and return person IDs."
  (let* ((people (perinf-person--active-choices))
         (groups
          (mapcar
           (lambda (group)
             (cons (format "%s: %s" (perinf-i18n 'group.label)
                           (perinf-object-title group))
                   group))
           (seq-filter
            (lambda (group) (eq (perinf-object-status group) 'active))
            (perinf-storage-list 'person-group perinf-current-project))))
         (choices
          (append
           (mapcar (lambda (person) (cons (car person) (list (cdr person)))) people)
           (mapcar
            (lambda (choice)
              (let* ((group (cdr choice))
                     (active-ids (mapcar #'cdr people)))
                (cons (car choice)
                      (seq-filter
                       (lambda (id) (member id active-ids))
                       (perinf-storage-group-member-ids group)))))
            groups)))
         (selected (and choices (completing-read prompt choices nil t))))
    (unless choices (user-error "%s" (perinf-i18n 'person.none)))
    (let ((ids (cdr (assoc selected choices))))
      (unless ids (user-error "%s" (perinf-i18n 'group.no-active-members)))
      ids)))

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
