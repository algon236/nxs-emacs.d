;;; perinf-task.el --- Task workflow for Personal Work and Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'perinf-date)
(require 'perinf-i18n)
(require 'perinf-storage)
(require 'seq)

(defcustom perinf-task-activity-write-interval 60
  "Minimum seconds between persistent activity updates from one buffer."
  :type 'integer
  :group 'perinf)

(defcustom perinf-task-inactivity-timeout 900
  "Seconds without recorded activity before a running task timer stops."
  :type 'integer
  :group 'perinf)

(defcustom perinf-task-inactivity-check-interval 60
  "Seconds between checks for inactive running task timers."
  :type 'integer
  :group 'perinf)

(defvar perinf-task-inactivity-check-timer nil
  "Repeating Emacs timer used to stop inactive PerInf task timers.")

(defvar-local perinf-task-activity-task-id nil
  "Stable PerInf task ID associated with the current buffer.")

(defvar-local perinf-task-activity-last-write nil
  "Time of the most recent persistent activity update from this buffer.")

(defun perinf-task--activity-resource (&optional buffer)
  "Return the persistent activity resource identifying BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if buffer-file-name
        (cons 'file (expand-file-name buffer-file-name))
      (cons 'buffer (buffer-name)))))

(defun perinf-task--trackable-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is suitable for task activity tracking."
  (with-current-buffer (or buffer (current-buffer))
    (and (not (minibufferp))
         (not (derived-mode-p 'perinf-mode))
         (not (string-prefix-p " " (buffer-name))))))

(defun perinf-task--select-active-task ()
  "Prompt for an active task and return its stable ID."
  (let* ((tasks (seq-filter
                 (lambda (task) (eq (perinf-object-status task) 'active))
                 (perinf-storage-list 'task perinf-current-project)))
         (choices (mapcar (lambda (task)
                            (cons (perinf-object-title task)
                                  (perinf-object-id task)))
                          tasks)))
    (unless choices
      (user-error "%s" (perinf-i18n 'task.none-active)))
    (cdr (assoc (completing-read (perinf-i18n 'task.select-prompt)
                                 choices nil t)
                choices))))

(defun perinf-task--matching-resource-task-ids (resource)
  "Return active task IDs whose saved activity resource matches RESOURCE."
  (let ((property (if (eq (car resource) 'file)
                      'TASK_ACTIVITY_FILES
                    'TASK_ACTIVITY_BUFFERS)))
    (delq nil
          (mapcar
           (lambda (task)
             (when (and (eq (perinf-object-status task) 'active)
                        (member (cdr resource)
                                (alist-get property
                                           (perinf-object-properties task))))
               (perinf-object-id task)))
           (perinf-storage-list 'task perinf-current-project)))))

(defun perinf-task-auto-associate-current-buffer ()
  "Restore an unambiguous saved PerInf task association for this buffer."
  (when (and (not perinf-task-activity-task-id)
             (boundp 'perinf-current-project)
             perinf-current-project
             (perinf-task--trackable-buffer-p))
    (condition-case nil
        (let* ((resource (perinf-task--activity-resource))
               (matches (perinf-task--matching-resource-task-ids resource)))
          (when (= (length matches) 1)
            (setq-local perinf-task-activity-task-id (car matches))))
      (error nil))))

(defun perinf-task-record-buffer-activity ()
  "Record observed activity for the PerInf task associated with this buffer."
  (when (and perinf-task-activity-task-id
             (boundp 'perinf-current-project)
             perinf-current-project
             (or (not perinf-task-activity-last-write)
                 (>= (float-time
                      (time-subtract (current-time)
                                     perinf-task-activity-last-write))
                     perinf-task-activity-write-interval)))
    (let ((resource (perinf-task--activity-resource)))
      (condition-case nil
          (when (perinf-storage-touch-task-activity
                 perinf-task-activity-task-id (car resource) (cdr resource)
                 nil perinf-current-project)
            (setq-local perinf-task-activity-last-write (current-time)))
        (error nil)))))

(defun perinf-task-stop-inactive-timers (&optional now)
  "Stop task timers that exceeded the inactivity limit at NOW.
Each timer is stopped exactly at its last recorded activity plus
`perinf-task-inactivity-timeout'.  Return the titles of stopped tasks."
  (when (and (boundp 'perinf-current-project) perinf-current-project)
    (let ((check-time (or now (current-time)))
          stopped-titles)
      (dolist (task (perinf-storage-list 'task perinf-current-project))
        (let* ((properties (perinf-object-properties task))
               (started-at (alist-get 'TASK_TIMER_STARTED_AT properties))
               (last-activity-at
                (or (alist-get 'TASK_LAST_ACTIVITY_AT properties)
                    started-at)))
          (when (and (eq (perinf-object-status task) 'active)
                     started-at
                     last-activity-at
                     (>= (float-time
                          (time-subtract check-time
                                         (date-to-time last-activity-at)))
                         perinf-task-inactivity-timeout))
            (perinf-storage-stop-task-timer
             (perinf-object-id task)
             perinf-current-project
             (time-add (date-to-time last-activity-at)
                       (seconds-to-time perinf-task-inactivity-timeout)))
            (push (perinf-object-title task) stopped-titles))))
      (setq stopped-titles (nreverse stopped-titles))
      (when stopped-titles
        (message
         "%s"
         (mapconcat
          (lambda (title)
            (format (perinf-i18n 'task.timer-auto-stopped)
                    title (/ perinf-task-inactivity-timeout 60)))
          stopped-titles "; ")))
      stopped-titles)))

(defun perinf-task--run-inactivity-check ()
  "Run the periodic inactivity check without aborting Emacs timer dispatch."
  (condition-case error-data
      (perinf-task-stop-inactive-timers)
    (error
     (message "PerInf: %s"
              (format (perinf-i18n 'task.timer-auto-stop-error)
                      (error-message-string error-data))))))

(defun perinf-task-install-inactivity-check ()
  "Install or replace the repeating PerInf inactivity check timer."
  (when (timerp perinf-task-inactivity-check-timer)
    (cancel-timer perinf-task-inactivity-check-timer))
  (setq perinf-task-inactivity-check-timer
        (run-with-timer perinf-task-inactivity-check-interval
                        perinf-task-inactivity-check-interval
                        #'perinf-task--run-inactivity-check)))

(defun perinf-task-associate-buffer (task-id &optional buffer)
  "Associate BUFFER with active TASK-ID and save its stable identification."
  (interactive (list (perinf-task--select-active-task) (current-buffer)))
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((target (or buffer
                    (get-buffer
                     (read-buffer (perinf-i18n 'task.activity-buffer-prompt)
                                  nil t)))))
    (unless (buffer-live-p target)
      (user-error "%s" (perinf-i18n 'task.activity-buffer-missing)))
    (unless (perinf-task--trackable-buffer-p target)
      (user-error "%s" (perinf-i18n 'task.activity-buffer-untrackable)))
    (let ((resource (perinf-task--activity-resource target)))
      (perinf-storage-set-task-activity-resource
       task-id (car resource) (cdr resource) t perinf-current-project)
      (with-current-buffer target
        (setq-local perinf-task-activity-task-id task-id
                    perinf-task-activity-last-write nil)
        (perinf-task-record-buffer-activity))
      (message "%s" (perinf-i18n 'task.activity-buffer-associated)))))

(defun perinf-task-dissociate-current-buffer ()
  "Remove the current buffer's saved PerInf task association."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (unless perinf-task-activity-task-id
    (user-error "%s" (perinf-i18n 'task.activity-buffer-not-associated)))
  (let ((resource (perinf-task--activity-resource)))
    (perinf-storage-set-task-activity-resource
     perinf-task-activity-task-id (car resource) (cdr resource) nil
     perinf-current-project)
    (setq-local perinf-task-activity-task-id nil
                perinf-task-activity-last-write nil)
    (message "%s" (perinf-i18n 'task.activity-buffer-dissociated))))

(defun perinf-task-maybe-associate-current-buffer (task-id)
  "Associate the current trackable buffer with TASK-ID unless already linked."
  (when (and (perinf-task--trackable-buffer-p)
             (not (equal perinf-task-activity-task-id task-id)))
    (perinf-task-associate-buffer task-id (current-buffer))))

(add-hook 'find-file-hook #'perinf-task-auto-associate-current-buffer)
(add-hook 'after-change-major-mode-hook #'perinf-task-auto-associate-current-buffer)
(add-hook 'post-command-hook #'perinf-task-record-buffer-activity)
(perinf-task-install-inactivity-check)

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
  (let* ((people
          (seq-filter
           (lambda (person)
             (eq (perinf-object-status person) 'active))
           (perinf-storage-list 'person perinf-current-project)))
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

(defun perinf-task--select-timer-task (start-p)
  "Prompt for a task whose timer can be started or stopped per START-P."
  (let* ((tasks
          (seq-filter
           (lambda (task)
             (and (eq (perinf-object-status task) 'active)
                  (if start-p
                      (not (alist-get 'TASK_TIMER_STARTED_AT
                                      (perinf-object-properties task)))
                    (alist-get 'TASK_TIMER_STARTED_AT
                               (perinf-object-properties task)))))
           (perinf-storage-list 'task perinf-current-project)))
         (choices
          (mapcar (lambda (task)
                    (cons (perinf-object-title task)
                          (perinf-object-id task)))
                  tasks)))
    (unless choices
      (user-error "%s" (perinf-i18n (if start-p
                                        'task.no-startable-timer
                                      'task.no-running-timer))))
    (cdr (assoc (completing-read (perinf-i18n 'task.select-prompt)
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

(defun perinf-task-format-work-time (seconds)
  "Format whole SECONDS as hours, minutes, and seconds."
  (let* ((total (max 0 (or seconds 0)))
         (hours (/ total 3600))
         (minutes (/ (% total 3600) 60))
         (remaining (% total 60)))
    (format "%d:%02d:%02d" hours minutes remaining)))

(defun perinf-task-total-work-seconds (task &optional now)
  "Return TASK's accumulated work seconds, including its running interval."
  (let* ((properties (perinf-object-properties task))
         (stored (string-to-number
                  (or (alist-get 'TASK_WORK_SECONDS properties) "0")))
         (started-at (alist-get 'TASK_TIMER_STARTED_AT properties)))
    (+ stored
       (if started-at
           (max 0 (truncate
                   (float-time
                    (time-subtract (or now (current-time))
                                   (date-to-time started-at)))))
         0))))

(defun perinf-task-toggle-timer (task-id start-p)
  "Start or stop TASK-ID's work timer according to START-P."
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (if start-p
      (perinf-storage-start-task-timer task-id perinf-current-project)
    (perinf-storage-stop-task-timer task-id perinf-current-project))
  (message "%s" (perinf-i18n (if start-p 'task.timer-started
                                'task.timer-stopped)))
  (when (fboundp 'perinf-core-work)
    (perinf-core-work)))

(provide 'perinf-task)

;;; perinf-task.el ends here
