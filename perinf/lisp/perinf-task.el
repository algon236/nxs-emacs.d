;;; perinf-task.el --- Task workflow for Personal Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'perinf-date)
(require 'perinf-i18n)
(require 'perinf-storage)
(require 'seq)

(defun perinf-task--project-setting (project property)
  "Return PROPERTY from PROJECT metadata."
  (alist-get property (perinf-storage-read-project project) nil nil #'eq))

;;;###autoload
(defun perinf-task-create (&optional decision-id)
  "Interactively create a task, optionally sourced from DECISION-ID."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((decision
          (and decision-id
               (seq-find
                (lambda (candidate)
                  (equal (perinf-object-id candidate) decision-id))
                (perinf-storage-list 'decision perinf-current-project))))
         (decision-properties
          (and decision (perinf-object-properties decision)))
         (format-name
          (perinf-task--project-setting
           perinf-current-project 'DATE_FORMAT))
         (date-format (intern format-name))
         (title
          (read-string
           (perinf-i18n 'task.title-prompt)
           (and decision (perinf-object-title decision))))
         (deadline-text
          (read-string
           (format "%s (%s): "
                   (perinf-i18n 'task.deadline-prompt)
                   (perinf-i18n
                    (intern (format "setting.%s" format-name))))))
         (deadline (perinf-date-normalize deadline-text date-format))
         (description (read-string (perinf-i18n 'task.notes-prompt)))
         (task
          (perinf-storage-create
           'task
           `((title . ,title)
             (deadline . ,deadline)
             (description . ,description)
             (decision-id . ,decision-id)
             (meeting-id . ,(alist-get 'MEETING_ID decision-properties))
             (minutes-id . ,(alist-get 'MINUTES_ID decision-properties)))
           perinf-current-project)))
    (message "%s" (perinf-i18n 'task.created))
    (when (fboundp 'perinf-core-work)
      (perinf-core-work))
    task))

(defun perinf-task-create-from-decision (decision-id)
  "Create a task sourced from DECISION-ID."
  (interactive)
  (unless (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) decision-id))
           (perinf-storage-list 'decision perinf-current-project))
    (signal 'perinf-object-not-found (list decision-id)))
  (perinf-task-create decision-id))

(defun perinf-task--select-person ()
  "Prompt for a registered person and return the stable ID."
  (let* ((people (perinf-storage-list 'person perinf-current-project))
         (choices
          (mapcar
           (lambda (person)
             (cons (perinf-object-title person)
                   (perinf-object-id person)))
           people)))
    (unless choices
      (user-error "%s" (perinf-i18n 'person.none)))
    (cdr
     (assoc
      (completing-read
       (perinf-i18n 'task.assignee-prompt) choices nil t)
      choices))))

(defun perinf-task-assign (task-id)
  "Assign TASK-ID to a registered person."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (perinf-storage-assign-task
   task-id (perinf-task--select-person) perinf-current-project)
  (message "%s" (perinf-i18n 'task.assigned))
  (when (fboundp 'perinf-core-work)
    (perinf-core-work)))

(defun perinf-task--select-context ()
  "Prompt for a context and return its stable ID."
  (let* ((contexts
          (perinf-storage-list 'context perinf-current-project))
         (choices
          (mapcar
           (lambda (context)
             (cons (perinf-object-title context)
                   (perinf-object-id context)))
           contexts)))
    (unless choices
      (user-error "%s" (perinf-i18n 'context.none)))
    (cdr
     (assoc
      (completing-read
       (perinf-i18n 'task.context-prompt) choices nil t)
      choices))))

(defun perinf-task-set-context (task-id)
  "Place TASK-ID in a registered context."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (perinf-storage-set-task-context
   task-id (perinf-task--select-context) perinf-current-project)
  (message "%s" (perinf-i18n 'task.context-set))
  (when (fboundp 'perinf-core-work)
    (perinf-core-work)))

(defun perinf-task--select-open-task ()
  "Prompt for a non-terminal task and return its ID."
  (let* ((tasks
          (seq-filter
           (lambda (task)
             (memq (perinf-object-status task) '(open active waiting)))
           (perinf-storage-list 'task perinf-current-project)))
         (choices
          (mapcar
           (lambda (task)
             (cons (perinf-object-title task)
                   (perinf-object-id task)))
           tasks)))
    (unless choices
      (user-error "%s" (perinf-i18n 'task.none-open)))
    (cdr (assoc
          (completing-read
           (perinf-i18n 'task.select-prompt)
           choices nil t)
          choices))))

;;;###autoload
(defun perinf-task-complete (&optional task-id)
  "Mark TASK-ID as completed through the storage API."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((id (or task-id (perinf-task--select-open-task))))
    (when (yes-or-no-p (perinf-i18n 'task.complete-confirmation))
      (perinf-storage-update
       id '((PERINF_STATUS . completed)) perinf-current-project)
      (message "%s" (perinf-i18n 'task.completed))
      (when (fboundp 'perinf-core-work)
        (perinf-core-work)))))

(defun perinf-task-set-status (task-id status)
  "Set TASK-ID to controlled STATUS and refresh the Work view."
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (when (or (not (memq status '(completed cancelled)))
            (yes-or-no-p
             (perinf-i18n
              (if (eq status 'completed)
                  'task.complete-confirmation
                'task.cancel-confirmation))))
    (perinf-storage-update
     task-id `((PERINF_STATUS . ,status)) perinf-current-project)
    (message "%s"
             (perinf-i18n (intern (format "task.%s" status))))
    (when (fboundp 'perinf-core-work)
      (perinf-core-work))))

(provide 'perinf-task)

;;; perinf-task.el ends here
