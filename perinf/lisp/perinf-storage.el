;;; perinf-storage.el --- Storage API boundary for Personal Work and Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file defines the public persistence boundary.  Persistent Org data must
;; be read and changed through this API as the implementation grows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-id)
(require 'perinf-project)

(define-error 'perinf-storage-error "Personal Work and Information System storage error")
(define-error 'perinf-object-not-found "Personal Work and Information System object not found"
  'perinf-storage-error)

(cl-defstruct perinf-object
  id type title status properties sections file position checksum modified-p)

(defun perinf-storage-read-project (directory)
  "Return project metadata for the Personal Work and Information System project in DIRECTORY."
  (perinf-project-read-metadata directory))

(defun perinf-storage--iso-now ()
  "Return the current time as an ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))

(defun perinf-storage--safe-line (value)
  "Return VALUE without line breaks."
  (string-trim (replace-regexp-in-string "[\n\r]+" " " value)))

(defun perinf-storage--atomic-write-buffer (buffer file)
  "Atomically write BUFFER to FILE."
  (let* ((directory (file-name-directory file))
         (temporary
          (make-temp-file (expand-file-name ".perinf-write-" directory))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) temporary nil 'silent)))
          (rename-file temporary file t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun perinf-storage--create-task (data project-directory)
  "Create a task from DATA in PROJECT-DIRECTORY."
  (let* ((file (expand-file-name "data/tasks.org" project-directory))
         (title (perinf-storage--safe-line (or (alist-get 'title data) "")))
         (description (string-trim (or (alist-get 'description data) "")))
         (deadline (alist-get 'deadline data))
         (decision-id (alist-get 'decision-id data))
         (meeting-id (alist-get 'meeting-id data))
         (minutes-id (alist-get 'minutes-id data))
         (context-id (alist-get 'context-id data))
         (id (concat "task-" (org-id-uuid)))
         (now (perinf-storage--iso-now)))
    (when (string-empty-p title)
      (user-error "Task title must not be empty"))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Task storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* TODO " title "\n")
      (when deadline
        (insert "DEADLINE: <" deadline ">\n"))
      (insert ":PROPERTIES:\n"
              ":ID:            " id "\n"
              ":PERINF_TYPE:   task\n"
              ":PERINF_STATUS: open\n"
              (if decision-id
                  (concat ":DECISION_ID:    " decision-id "\n")
                "")
              (if meeting-id
                  (concat ":MEETING_ID:     " meeting-id "\n")
                "")
              (if minutes-id
                  (concat ":MINUTES_ID:     " minutes-id "\n")
                "")
              (if context-id
                  (concat ":CONTEXT_ID:     " context-id "\n")
                "")
              ":CREATED_AT:    " now "\n"
              ":MODIFIED_AT:   " now "\n"
              ":END:\n")
      (unless (string-empty-p description)
        (insert "\n" description "\n"))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (make-perinf-object
     :id id :type 'task :title title :status 'open
     :properties `((DEADLINE . ,deadline)
                   (DECISION_ID . ,decision-id)
                   (MEETING_ID . ,meeting-id)
                   (MINUTES_ID . ,minutes-id)
                   (CONTEXT_ID . ,context-id)
                   (CREATED_AT . ,now)
                   (MODIFIED_AT . ,now))
     :file file)))

(defun perinf-storage--datetime (date time)
  "Combine DATE and TIME into a local ISO 8601 datetime."
  (unless (and (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
                date)
               (string-match
                "\\`\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):[0-9]\\{2\\}\\'"
                time))
    (error "Invalid normalized date or time"))
  (let* ((year (string-to-number (substring date 0 4)))
         (month (string-to-number (substring date 5 7)))
         (day (string-to-number (substring date 8 10)))
         (hour (string-to-number (substring time 0 2)))
         (minute (string-to-number (substring time 3 5)))
         (value (encode-time 0 minute hour day month year)))
    (format-time-string "%Y-%m-%dT%H:%M:%S%:z" value)))

(defun perinf-storage--safe-file-name (value)
  "Return a conservative file-name component derived from VALUE."
  (let ((name (downcase (perinf-storage--safe-line value))))
    (setq name (replace-regexp-in-string "[æä]" "ae" name)
          name (replace-regexp-in-string "[øö]" "oe" name)
          name (replace-regexp-in-string "[å]" "aa" name)
          name (replace-regexp-in-string "[^[:alnum:]]+" "-" name)
          name (replace-regexp-in-string "\\`-+\\|-+\\'" "" name))
    (if (string-empty-p name) "meeting" (substring name 0 (min 60 (length name))))))

(defun perinf-storage--create-meeting (data project-directory)
  "Create a meeting from DATA in PROJECT-DIRECTORY."
  (let* ((title (perinf-storage--safe-line (or (alist-get 'title data) "")))
         (date (alist-get 'date data))
         (start-time (alist-get 'start-time data))
         (finish-time (alist-get 'finish-time data))
         (location (perinf-storage--safe-line
                    (or (alist-get 'location data) "")))
         (start-at (perinf-storage--datetime date start-time))
         (finish-at (and finish-time
                         (perinf-storage--datetime date finish-time)))
         (id (concat "meeting-" (org-id-uuid)))
         (now (perinf-storage--iso-now))
         (directory (expand-file-name
                     (format "data/meetings/%s" (substring date 0 4))
                     project-directory))
         (file (expand-file-name
                (format "%s-%s-%s.org"
                        date
                        (perinf-storage--safe-file-name title)
                        (substring id (- (length id) 6)))
                directory)))
    (when (string-empty-p title)
      (user-error "Meeting title must not be empty"))
    (when (and finish-at (not (string< start-at finish-at)))
      (user-error "Meeting finish time must be after start time"))
    (make-directory directory t)
    (with-temp-buffer
      (insert (format
               (concat "#+title: %s\n"
                       "#+date: %s\n"
                       "#+startup: overview\n\n"
                       "* %s\n"
                       ":PROPERTIES:\n"
                       ":ID:            %s\n"
                       ":PERINF_TYPE:   meeting\n"
                       ":PERINF_STATUS: planned\n"
                       ":START_AT:      %s\n"
                       "%s"
                       ":MEETING_MODE:  physical\n"
                       ":LOCATION:      %s\n"
                       ":CREATED_AT:    %s\n"
                       ":MODIFIED_AT:   %s\n"
                       ":END:\n\n"
                       "** Participants\n"
                       ":PROPERTIES:\n"
                       ":PERINF_SECTION: participants\n"
                       ":END:\n\n"
                       "** Agenda\n"
                       ":PROPERTIES:\n"
                       ":PERINF_SECTION: agenda\n"
                       ":END:\n")
               title date title id start-at
               (if finish-at (format ":FINISH_AT:     %s\n" finish-at) "")
               location now now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (make-perinf-object
     :id id :type 'meeting :title title :status 'planned
     :properties `((START_AT . ,start-at)
                   (FINISH_AT . ,finish-at)
                   (LOCATION . ,location))
     :file file)))

(defun perinf-storage--create-person (data project-directory)
  "Create a person from DATA in PROJECT-DIRECTORY."
  (let* ((file (expand-file-name "data/people.org" project-directory))
         (name (perinf-storage--safe-line (or (alist-get 'title data) "")))
         (email (perinf-storage--safe-line (or (alist-get 'email data) "")))
         (phone (perinf-storage--safe-line (or (alist-get 'phone data) "")))
         (id (concat "person-" (org-id-uuid)))
         (now (perinf-storage--iso-now)))
    (when (string-empty-p name)
      (user-error "Person name must not be empty"))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Person storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* " name "\n"
              ":PROPERTIES:\n"
              ":ID:            " id "\n"
              ":PERINF_TYPE:   person\n"
              ":PERINF_STATUS: active\n")
      (unless (string-empty-p email)
        (insert ":EMAIL:         " email "\n"))
      (unless (string-empty-p phone)
        (insert ":PHONE:         " phone "\n"))
      (insert ":CREATED_AT:    " now "\n"
              ":MODIFIED_AT:   " now "\n"
              ":END:\n")
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (make-perinf-object
     :id id :type 'person :title name :status 'active
     :properties `((EMAIL . ,email)
                   (PHONE . ,phone)
                   (CREATED_AT . ,now)
                   (MODIFIED_AT . ,now))
     :file file)))

(defun perinf-storage--split-ids (value)
  "Return the stable IDs encoded in VALUE."
  (and value (split-string value "[ ,]+" t)))

(defun perinf-storage--join-ids (ids)
  "Encode stable IDS for an Org property."
  (mapconcat #'identity (delete-dups (delq nil (copy-sequence ids))) " "))

(defun perinf-storage--create-person-group (data project-directory)
  "Create a person group from DATA in PROJECT-DIRECTORY."
  (let* ((file (expand-file-name "data/people.org" project-directory))
         (name (perinf-storage--safe-line (or (alist-get 'title data) "")))
         (member-ids (or (alist-get 'member-ids data) nil))
         (id (concat "person-group-" (org-id-uuid)))
         (now (perinf-storage--iso-now)))
    (when (string-empty-p name)
      (user-error "Group name must not be empty"))
    (dolist (person-id member-ids)
      (unless (seq-find
               (lambda (person) (equal (perinf-object-id person) person-id))
               (perinf-storage-list 'person project-directory))
        (signal 'perinf-object-not-found (list person-id))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* " name "\n"
              ":PROPERTIES:\n"
              ":ID:            " id "\n"
              ":PERINF_TYPE:   person-group\n"
              ":PERINF_STATUS: active\n"
              ":MEMBER_IDS:    " (perinf-storage--join-ids member-ids) "\n"
              ":CREATED_AT:    " now "\n"
              ":MODIFIED_AT:   " now "\n"
              ":END:\n")
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (make-perinf-object
     :id id :type 'person-group :title name :status 'active
     :properties `((MEMBER_IDS . ,(perinf-storage--join-ids member-ids)))
     :file file)))

(defun perinf-storage--create-decision (data project-directory)
  "Create a decision from DATA in PROJECT-DIRECTORY."
  (let* ((file (expand-file-name "data/decisions.org" project-directory))
         (title (perinf-storage--safe-line (or (alist-get 'title data) "")))
         (date (alist-get 'date data))
         (rationale (string-trim (or (alist-get 'rationale data) "")))
         (meeting-id (alist-get 'meeting-id data))
         (minutes-id (alist-get 'minutes-id data))
         (id (concat "decision-" (org-id-uuid)))
         (now (perinf-storage--iso-now)))
    (when (string-empty-p title)
      (user-error "Decision title must not be empty"))
    (unless (and date
                 (string-match-p
                  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date))
      (user-error "Decision date must be a normalized ISO date"))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Decision storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* " title "\n"
              ":PROPERTIES:\n"
              ":ID:            " id "\n"
              ":PERINF_TYPE:   decision\n"
              ":PERINF_STATUS: active\n"
              ":DECIDED_ON:    " date "\n"
              (if meeting-id
                  (concat ":MEETING_ID:    " meeting-id "\n")
                "")
              (if minutes-id
                  (concat ":MINUTES_ID:    " minutes-id "\n")
                "")
              ":CREATED_AT:    " now "\n"
              ":MODIFIED_AT:   " now "\n"
              ":END:\n")
      (unless (string-empty-p rationale)
        (insert "\n" rationale "\n"))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (make-perinf-object
     :id id :type 'decision :title title :status 'active
     :properties `((DECIDED_ON . ,date)
                   (RATIONALE . ,rationale)
                   (MEETING_ID . ,meeting-id)
                   (MINUTES_ID . ,minutes-id))
     :file file)))

(defun perinf-storage--create-context (data project-directory)
  "Create a context from DATA in PROJECT-DIRECTORY."
  (let* ((file (expand-file-name "data/contexts.org" project-directory))
         (title (perinf-storage--safe-line (or (alist-get 'title data) "")))
         (description (string-trim (or (alist-get 'description data) "")))
         (id (concat "context-" (org-id-uuid)))
         (now (perinf-storage--iso-now)))
    (when (string-empty-p title)
      (user-error "Context title must not be empty"))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Context storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* " title "\n"
              ":PROPERTIES:\n"
              ":ID:            " id "\n"
              ":PERINF_TYPE:   context\n"
              ":PERINF_STATUS: active\n"
              ":CREATED_AT:    " now "\n"
              ":MODIFIED_AT:   " now "\n"
              ":END:\n")
      (unless (string-empty-p description)
        (insert "\n" description "\n"))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (make-perinf-object
     :id id :type 'context :title title :status 'active
     :properties `((DESCRIPTION . ,description))
     :file file)))

(defun perinf-storage-create (type data &optional project-directory)
  "Create an object of TYPE from DATA in PROJECT-DIRECTORY."
  (let ((project
         (or project-directory
             (signal 'perinf-storage-error
                     '("No project directory supplied")))))
    (unless (perinf-project-p project)
      (signal 'perinf-storage-error
              (list (format "Not a Personal Work and Information System project: %s" project))))
    (pcase type
      ('task (perinf-storage--create-task data project))
      ('meeting (perinf-storage--create-meeting data project))
      ('person (perinf-storage--create-person data project))
      ('person-group (perinf-storage--create-person-group data project))
      ('decision (perinf-storage--create-decision data project))
      ('context (perinf-storage--create-context data project))
      (_ (signal 'perinf-storage-error
                 (list (format "Creation is not implemented for: %S"
                               type)))))))

(defun perinf-storage-read (id)
  "Read the object identified by ID.
Object persistence is intentionally unavailable in the bootstrap milestone."
  (signal 'perinf-object-not-found (list id)))

(defun perinf-storage--find-id (id)
  "Move point to the Org entry with ID and return non-nil when found."
  (let (found)
    (org-map-entries
     (lambda ()
       (when (and (not found)
                  (equal (org-entry-get nil "ID") id))
         (setq found (point)))))
    (when found
      (goto-char found))
    found))

(defun perinf-storage--task-work-seconds-at-point ()
  "Return the stored whole work seconds for the task at point."
  (let ((value (org-entry-get nil "TASK_WORK_SECONDS")))
    (if (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)
      0)))

(defun perinf-storage--decode-string-list (value)
  "Decode a printed list of strings from Org property VALUE."
  (when (and value (not (string-empty-p value)))
    (condition-case nil
        (let ((decoded (car (read-from-string value))))
          (when (and (listp decoded) (seq-every-p #'stringp decoded))
            decoded))
      (error nil))))

(defun perinf-storage--encode-string-list (values)
  "Encode string list VALUES for storage in one Org property."
  (prin1-to-string (sort (delete-dups (copy-sequence values)) #'string-lessp)))

(defun perinf-storage--task-activity-resources-at-point ()
  "Return the task activity resources at point as an alist."
  `((file . ,(perinf-storage--decode-string-list
              (org-entry-get nil "TASK_ACTIVITY_FILES")))
    (buffer . ,(perinf-storage--decode-string-list
                (org-entry-get nil "TASK_ACTIVITY_BUFFERS")))))

(defun perinf-storage-set-task-activity-resource
    (id kind identifier add-p &optional project-directory)
  "Add or remove a task activity resource.
ID identifies the task, KIND is `file' or `buffer', and IDENTIFIER is its
persistent name.  When ADD-P is non-nil, remove the same resource from other
tasks first so automatic activity attribution remains unambiguous."
  (unless (memq kind '(file buffer))
    (signal 'perinf-storage-error (list (format "Unsupported resource kind: %s" kind))))
  (unless (and (stringp identifier) (not (string-empty-p identifier)))
    (user-error "Activity resource must not be empty"))
  (let* ((project (or project-directory
                      (signal 'perinf-storage-error
                              '("No project directory supplied"))))
         (file (expand-file-name "data/tasks.org" project))
         (property (if (eq kind 'file)
                       "TASK_ACTIVITY_FILES"
                     "TASK_ACTIVITY_BUFFERS"))
         found)
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Task storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get nil "PERINF_TYPE") "task")
           (let ((current-id (org-entry-get nil "ID"))
                 (values (perinf-storage--decode-string-list
                          (org-entry-get nil property))))
             (when (equal current-id id)
               (setq found t))
             (when (or (and add-p (member identifier values))
                       (equal current-id id))
               (setq values (delete identifier values))
               (when (and add-p (equal current-id id))
                 (push identifier values))
               (if values
                   (org-entry-put nil property
                                  (perinf-storage--encode-string-list values))
                 (org-entry-delete nil property))
               (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now)))))))
      (unless found
        (signal 'perinf-object-not-found (list id)))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find (lambda (object) (equal (perinf-object-id object) id))
              (perinf-storage-list 'task project))))

(defun perinf-storage-touch-task-activity
    (id kind identifier &optional activity-at project-directory)
  "Record observed KIND and IDENTIFIER activity for running task ID.
Return non-nil when the timestamp was stored.  A stopped timer is deliberately
left unchanged because activity cannot then be counted as timed task work.
The resource must still be associated with the task, preventing stale buffers
from attributing work after a file has been reassigned to another task."
  (unless (memq kind '(file buffer))
    (signal 'perinf-storage-error (list (format "Unsupported resource kind: %s" kind))))
  (let* ((project (or project-directory
                      (signal 'perinf-storage-error
                              '("No project directory supplied"))))
         (file (expand-file-name "data/tasks.org" project))
         stored)
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Task storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id id)
        (signal 'perinf-object-not-found (list id)))
      (let* ((property (if (eq kind 'file)
                           "TASK_ACTIVITY_FILES"
                         "TASK_ACTIVITY_BUFFERS"))
             (associated
              (member identifier
                      (perinf-storage--decode-string-list
                       (org-entry-get nil property)))))
        (when (and associated
                   (equal (org-entry-get nil "PERINF_STATUS") "active")
                   (org-entry-get nil "TASK_TIMER_STARTED_AT"))
        (let ((now (or activity-at (perinf-storage--iso-now))))
          (org-entry-put nil "TASK_LAST_ACTIVITY_AT" now)
          (org-entry-put nil "TASK_LAST_ACTIVITY_RESOURCE"
                         (perinf-storage--safe-line
                          (if (eq kind 'file)
                              identifier
                            (format "Buffer: %s" identifier))))
          (perinf-storage--atomic-write-buffer (current-buffer) file)
            (setq stored t))))
      stored)))

(defun perinf-storage--stop-task-timer-at-point (&optional stopped-at)
  "Stop the task timer at point at STOPPED-AT and return total seconds."
  (let ((started-at (org-entry-get nil "TASK_TIMER_STARTED_AT")))
    (unless started-at
      (user-error "Task timer is not running"))
    (let* ((finish (or stopped-at (current-time)))
           (elapsed (max 0 (truncate
                            (float-time
                             (time-subtract finish
                                            (date-to-time started-at))))))
           (total (+ (perinf-storage--task-work-seconds-at-point) elapsed)))
      (org-entry-put nil "TASK_WORK_SECONDS" (number-to-string total))
      (org-entry-delete nil "TASK_TIMER_STARTED_AT")
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      total)))

(defun perinf-storage-update (id changes &optional project-directory)
  "Apply CHANGES to object ID in PROJECT-DIRECTORY.
This implementation supports controlled task status changes."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (file (expand-file-name "data/tasks.org" project))
         (new-status (alist-get 'PERINF_STATUS changes)))
    (unless (memq new-status '(open active waiting completed cancelled))
      (signal 'perinf-storage-error
              (list (format "Unsupported task status: %s" new-status))))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Task storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id id)
        (signal 'perinf-object-not-found (list id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "task")
        (signal 'perinf-storage-error
                (list (format "Object is not a task: %s" id))))
      (let* ((current-status
              (intern (org-entry-get nil "PERINF_STATUS")))
             (allowed
              (pcase current-status
                ('open '(active waiting completed cancelled))
                ('active '(open waiting completed cancelled))
                ('waiting '(open active completed cancelled))
                ('completed '(open))
                ('cancelled '(open))
                (_ nil))))
        (unless (memq new-status allowed)
          (user-error "Invalid task status transition: %s to %s"
                      current-status new-status))
        (when (org-entry-get nil "TASK_TIMER_STARTED_AT")
          (perinf-storage--stop-task-timer-at-point))
        (let ((org-log-done nil)
              (org-log-into-drawer nil))
          (org-todo (if (eq new-status 'completed) "DONE" "TODO"))
          (if (eq new-status 'completed)
              (org-add-planning-info 'closed (current-time))
            (save-excursion
              (org-back-to-heading t)
              (let ((entry-end
                     (copy-marker
                      (save-excursion (org-end-of-subtree t t)))))
                (forward-line 1)
                (when (re-search-forward "^CLOSED:.*\n" entry-end t)
                  (replace-match ""))))))
        (org-entry-put nil "PERINF_STATUS" (symbol-name new-status))
        (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now)))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (car (seq-filter
          (lambda (object) (equal (perinf-object-id object) id))
          (perinf-storage-list 'task project)))))

(defun perinf-storage-start-task-timer (id &optional project-directory)
  "Start the work timer for active task ID in PROJECT-DIRECTORY."
  (let* ((project (or project-directory
                      (signal 'perinf-storage-error
                              '("No project directory supplied"))))
         (file (expand-file-name "data/tasks.org" project)))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Task storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id id)
        (signal 'perinf-object-not-found (list id)))
      (unless (equal (org-entry-get nil "PERINF_STATUS") "active")
        (user-error "Task must be active before its timer can start"))
      (when (org-entry-get nil "TASK_TIMER_STARTED_AT")
        (user-error "Task timer is already running"))
      (let ((now (perinf-storage--iso-now)))
        (org-entry-put nil "TASK_TIMER_STARTED_AT" now)
        (org-entry-put nil "TASK_LAST_ACTIVITY_AT" now)
        (org-entry-put nil "TASK_LAST_ACTIVITY_RESOURCE" "PerInf timer")
        (org-entry-put nil "MODIFIED_AT" now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find (lambda (object) (equal (perinf-object-id object) id))
              (perinf-storage-list 'task project))))

(defun perinf-storage-stop-task-timer
    (id &optional project-directory stopped-at)
  "Stop task ID's work timer and add its time.
Use PROJECT-DIRECTORY for storage.  When STOPPED-AT is non-nil, calculate the
elapsed interval up to that time; this supports an exact inactivity boundary."
  (let* ((project (or project-directory
                      (signal 'perinf-storage-error
                              '("No project directory supplied"))))
         (file (expand-file-name "data/tasks.org" project)))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Task storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id id)
        (signal 'perinf-object-not-found (list id)))
      (perinf-storage--stop-task-timer-at-point stopped-at)
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find (lambda (object) (equal (perinf-object-id object) id))
              (perinf-storage-list 'task project))))

(defun perinf-storage-update-meeting
    (meeting-id data &optional project-directory)
  "Update MEETING-ID from DATA in PROJECT-DIRECTORY.
DATA may contain `title', `date', `start-time', `finish-time', and `location'.
The meeting ID, status, linked children, and imported artifacts are preserved."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) meeting-id))
           (perinf-storage-list 'meeting project)))
         (title (perinf-storage--safe-line
                 (or (alist-get 'title data) "")))
         (date (alist-get 'date data))
         (start-time (alist-get 'start-time data))
         (finish-time (alist-get 'finish-time data))
         (location (perinf-storage--safe-line
                    (or (alist-get 'location data) "")))
         (start-at (and date start-time
                        (perinf-storage--datetime date start-time)))
         (finish-at (and date finish-time
                         (perinf-storage--datetime date finish-time)))
         (file (and meeting (perinf-object-file meeting))))
    (unless meeting
      (signal 'perinf-object-not-found (list meeting-id)))
    (when (string-empty-p title)
      (user-error "Meeting title must not be empty"))
    (unless (and date start-time)
      (user-error "Meeting date and start time must not be empty"))
    (when (and finish-at (not (string< start-at finish-at)))
      (user-error "Meeting finish time must be after start time"))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Meeting storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id meeting-id)
        (signal 'perinf-object-not-found (list meeting-id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "meeting")
        (signal 'perinf-storage-error
                (list (format "Object is not a meeting: %s" meeting-id))))
      (org-edit-headline title)
      (org-entry-put nil "START_AT" start-at)
      (if finish-at
          (org-entry-put nil "FINISH_AT" finish-at)
        (org-entry-delete nil "FINISH_AT"))
      (org-entry-put nil "LOCATION" location)
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+title:.*$" nil t)
          (replace-match (concat "#+title: " title) t t))
        (goto-char (point-min))
        (when (re-search-forward "^#\\+date:.*$" nil t)
          (replace-match (concat "#+date: " date) t t)))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find
     (lambda (candidate)
       (equal (perinf-object-id candidate) meeting-id))
     (perinf-storage-list 'meeting project))))

(defun perinf-storage-person-references (person-id &optional project-directory)
  "Return tasks and meetings that refer to PERSON-ID in PROJECT-DIRECTORY."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (tasks
          (seq-filter
           (lambda (task)
             (member person-id (perinf-storage-task-assignee-ids task)))
           (perinf-storage-list 'task project)))
         (meetings
          (seq-filter
           (lambda (meeting)
             (seq-some
              (lambda (participant)
                (equal
                 (alist-get
                  'PERSON_ID (perinf-object-properties participant))
                 person-id))
              (perinf-storage-list-children
               (perinf-object-id meeting) 'participants project)))
           (perinf-storage-list 'meeting project))))
    (list :tasks tasks :meetings meetings)))

(defun perinf-storage-set-person-status
    (person-id status &optional project-directory)
  "Set PERSON-ID to active or inactive STATUS in PROJECT-DIRECTORY."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (file (expand-file-name "data/people.org" project)))
    (unless (memq status '(active inactive))
      (signal 'perinf-storage-error
              (list (format "Unsupported person status: %s" status))))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Person storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id person-id)
        (signal 'perinf-object-not-found (list person-id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "person")
        (signal 'perinf-storage-error
                (list (format "Object is not a person: %s" person-id))))
      (org-entry-put nil "PERINF_STATUS" (symbol-name status))
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find
     (lambda (person) (equal (perinf-object-id person) person-id))
     (perinf-storage-list 'person project))))

(defun perinf-storage-update-person
    (person-id name email phone &optional project-directory)
  "Update PERSON-ID with NAME, EMAIL and PHONE in PROJECT-DIRECTORY."
  (let* ((project (or project-directory
                      (signal 'perinf-storage-error
                              '("No project directory supplied"))))
         (file (expand-file-name "data/people.org" project))
         (safe-name (perinf-storage--safe-line name))
         (safe-email (perinf-storage--safe-line email))
         (safe-phone (perinf-storage--safe-line phone)))
    (when (string-empty-p safe-name)
      (user-error "Person name must not be empty"))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id person-id)
        (signal 'perinf-object-not-found (list person-id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "person")
        (signal 'perinf-storage-error (list "Object is not a person")))
      (org-edit-headline safe-name)
      (if (string-empty-p safe-email)
          (org-entry-delete nil "EMAIL")
        (org-entry-put nil "EMAIL" safe-email))
      (if (string-empty-p safe-phone)
          (org-entry-delete nil "PHONE")
        (org-entry-put nil "PHONE" safe-phone))
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find (lambda (person) (equal (perinf-object-id person) person-id))
              (perinf-storage-list 'person project))))

(defun perinf-storage-group-member-ids (group)
  "Return the person IDs stored in GROUP."
  (perinf-storage--split-ids
   (alist-get 'MEMBER_IDS (perinf-object-properties group))))

(defun perinf-storage-update-person-group
    (group-id name member-ids &optional project-directory)
  "Update GROUP-ID with NAME and MEMBER-IDS in PROJECT-DIRECTORY."
  (let* ((project (or project-directory
                      (signal 'perinf-storage-error
                              '("No project directory supplied"))))
         (file (expand-file-name "data/people.org" project))
         (safe-name (perinf-storage--safe-line name)))
    (when (string-empty-p safe-name)
      (user-error "Group name must not be empty"))
    (dolist (person-id member-ids)
      (unless (seq-find
               (lambda (person) (equal (perinf-object-id person) person-id))
               (perinf-storage-list 'person project))
        (signal 'perinf-object-not-found (list person-id))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id group-id)
        (signal 'perinf-object-not-found (list group-id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "person-group")
        (signal 'perinf-storage-error (list "Object is not a person group")))
      (org-edit-headline safe-name)
      (org-entry-put nil "MEMBER_IDS" (perinf-storage--join-ids member-ids))
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find (lambda (group) (equal (perinf-object-id group) group-id))
              (perinf-storage-list 'person-group project))))

(defun perinf-storage-set-person-group-status
    (group-id status &optional project-directory)
  "Set GROUP-ID to active or inactive STATUS in PROJECT-DIRECTORY."
  (let* ((project (or project-directory
                      (signal 'perinf-storage-error
                              '("No project directory supplied"))))
         (file (expand-file-name "data/people.org" project)))
    (unless (memq status '(active inactive))
      (signal 'perinf-storage-error (list "Unsupported group status")))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id group-id)
        (signal 'perinf-object-not-found (list group-id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "person-group")
        (signal 'perinf-storage-error (list "Object is not a person group")))
      (org-entry-put nil "PERINF_STATUS" (symbol-name status))
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find (lambda (group) (equal (perinf-object-id group) group-id))
              (perinf-storage-list 'person-group project))))

(defun perinf-storage-delete-person (person-id &optional project-directory)
  "Permanently delete unreferenced PERSON-ID from PROJECT-DIRECTORY.
Signal an error when tasks or meetings still refer to the person."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (references (perinf-storage-person-references person-id project))
         (tasks (plist-get references :tasks))
         (meetings (plist-get references :meetings))
         (file (expand-file-name "data/people.org" project)))
    (when (or tasks meetings)
      (user-error
       "Person is still referenced by %d task(s) and %d meeting(s)"
       (length tasks) (length meetings)))
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Person storage is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id person-id)
        (signal 'perinf-object-not-found (list person-id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "person")
        (signal 'perinf-storage-error
                (list (format "Object is not a person: %s" person-id))))
      (org-cut-subtree)
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    t))

(defun perinf-storage-list (type &optional project-directory)
  "Return all objects of TYPE from PROJECT-DIRECTORY."
  (let ((project
         (or project-directory
             (signal 'perinf-storage-error
                     '("No project directory supplied")))))
    (pcase type
      ('task
       (let ((file (expand-file-name "data/tasks.org" project))
             objects)
         (unless (file-readable-p file)
           (signal 'perinf-storage-error
                   (list (format "Task storage is not readable: %s" file))))
         (with-temp-buffer
           (insert-file-contents file)
           (org-mode)
           (org-map-entries
            (lambda ()
              (when (equal (org-entry-get nil "PERINF_TYPE") "task")
                (let* ((deadline-time (org-get-deadline-time (point)))
                       (deadline
                        (and deadline-time
                             (format-time-string "%Y-%m-%d" deadline-time))))
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'task
                    :title (org-get-heading t t t t)
                    :status (intern
                             (or (org-entry-get nil "PERINF_STATUS")
                                 "open"))
                    :properties
                    `((DEADLINE . ,deadline)
                      (DECISION_ID . ,(org-entry-get nil "DECISION_ID"))
                      (ASSIGNEE_ID . ,(org-entry-get nil "ASSIGNEE_ID"))
                      (ASSIGNEE_IDS . ,(org-entry-get nil "ASSIGNEE_IDS"))
                      (MEETING_ID . ,(org-entry-get nil "MEETING_ID"))
                      (MINUTES_ID . ,(org-entry-get nil "MINUTES_ID"))
                      (CONTEXT_ID . ,(org-entry-get nil "CONTEXT_ID"))
                      (TASK_TIMER_STARTED_AT
                       . ,(org-entry-get nil "TASK_TIMER_STARTED_AT"))
                      (TASK_WORK_SECONDS
                       . ,(org-entry-get nil "TASK_WORK_SECONDS"))
                      (TASK_LAST_ACTIVITY_AT
                       . ,(org-entry-get nil "TASK_LAST_ACTIVITY_AT"))
                      (TASK_LAST_ACTIVITY_RESOURCE
                       . ,(org-entry-get nil "TASK_LAST_ACTIVITY_RESOURCE"))
                      (TASK_ACTIVITY_FILES
                       . ,(perinf-storage--decode-string-list
                           (org-entry-get nil "TASK_ACTIVITY_FILES")))
                      (TASK_ACTIVITY_BUFFERS
                       . ,(perinf-storage--decode-string-list
                           (org-entry-get nil "TASK_ACTIVITY_BUFFERS"))))
                    :file file
                    :position (point))
                   objects))))))
         (nreverse objects)))
      ('meeting
       (let ((directory (expand-file-name "data/meetings" project))
             objects)
         (dolist (file (and (file-directory-p directory)
                            (directory-files-recursively directory "\\.org\\'")))
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (goto-char (point-min))
             (org-map-entries
              (lambda ()
                (when (equal (org-entry-get nil "PERINF_TYPE") "meeting")
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'meeting
                    :title (org-get-heading t t t t)
                    :status
                    (intern (or (org-entry-get nil "PERINF_STATUS")
                                "planned"))
                    :properties
                    `((START_AT . ,(org-entry-get nil "START_AT"))
                      (FINISH_AT . ,(org-entry-get nil "FINISH_AT"))
                      (LOCATION . ,(org-entry-get nil "LOCATION"))
                      (ACTUAL_START_AT
                       . ,(org-entry-get nil "ACTUAL_START_AT"))
                      (ACTUAL_FINISH_AT
                       . ,(org-entry-get nil "ACTUAL_FINISH_AT"))
                      (AUDIO_ID . ,(org-entry-get nil "AUDIO_ID"))
                      (AUDIO_STATUS . ,(org-entry-get nil "AUDIO_STATUS"))
                      (TRANSCRIPT_ID
                       . ,(org-entry-get nil "TRANSCRIPT_ID"))
                      (TRANSCRIPT_STATUS
                       . ,(org-entry-get nil "TRANSCRIPT_STATUS"))
                      (MINUTES_ID . ,(org-entry-get nil "MINUTES_ID"))
                      (MINUTES_STATUS
                       . ,(org-entry-get nil "MINUTES_STATUS")))
                    :file file
                    :position (point))
                   objects))))))
         (sort objects
               (lambda (left right)
                 (string<
                  (or (alist-get 'START_AT
                                 (perinf-object-properties left))
                      "")
                  (or (alist-get 'START_AT
                                 (perinf-object-properties right))
                      ""))))))
      ('person
       (let ((file (expand-file-name "data/people.org" project))
             objects)
         (unless (file-readable-p file)
           (signal 'perinf-storage-error
                   (list (format "Person storage is not readable: %s" file))))
         (with-temp-buffer
           (insert-file-contents file)
           (org-mode)
           (org-map-entries
            (lambda ()
              (when (equal (org-entry-get nil "PERINF_TYPE") "person")
                (push
                 (make-perinf-object
                  :id (org-entry-get nil "ID")
                  :type 'person
                  :title (org-get-heading t t t t)
                  :status
                  (intern (or (org-entry-get nil "PERINF_STATUS") "active"))
                  :properties
                  `((EMAIL . ,(org-entry-get nil "EMAIL"))
                    (PHONE . ,(org-entry-get nil "PHONE")))
                  :file file
                  :position (point))
                 objects)))))
         (sort objects
               (lambda (left right)
                 (string-lessp (perinf-object-title left)
                               (perinf-object-title right))))))
      ('person-group
       (let ((file (expand-file-name "data/people.org" project)) objects)
         (unless (file-readable-p file)
           (signal 'perinf-storage-error
                   (list (format "Person storage is not readable: %s" file))))
         (with-temp-buffer
           (insert-file-contents file)
           (org-mode)
           (org-map-entries
            (lambda ()
              (when (equal (org-entry-get nil "PERINF_TYPE") "person-group")
                (push
                 (make-perinf-object
                  :id (org-entry-get nil "ID")
                  :type 'person-group
                  :title (org-get-heading t t t t)
                  :status (intern (or (org-entry-get nil "PERINF_STATUS") "active"))
                  :properties `((MEMBER_IDS . ,(org-entry-get nil "MEMBER_IDS")))
                  :file file :position (point))
                 objects)))))
         (sort objects (lambda (left right)
                         (string-lessp (perinf-object-title left)
                                       (perinf-object-title right))))))
      ('decision
       (let ((file (expand-file-name "data/decisions.org" project))
             objects)
         (unless (file-readable-p file)
           (signal 'perinf-storage-error
                   (list (format "Decision storage is not readable: %s" file))))
         (with-temp-buffer
           (insert-file-contents file)
           (org-mode)
           (org-map-entries
            (lambda ()
              (when (equal (org-entry-get nil "PERINF_TYPE") "decision")
                (let ((start (progn (org-end-of-meta-data t) (point)))
                      (finish (save-excursion
                                (org-end-of-subtree t t) (point))))
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'decision
                    :title (org-get-heading t t t t)
                    :status
                    (intern (or (org-entry-get nil "PERINF_STATUS")
                                "active"))
                    :properties
                    `((DECIDED_ON . ,(org-entry-get nil "DECIDED_ON"))
                      (MEETING_ID . ,(org-entry-get nil "MEETING_ID"))
                      (MINUTES_ID . ,(org-entry-get nil "MINUTES_ID"))
                      (RATIONALE
                       . ,(string-trim
                           (buffer-substring-no-properties start finish))))
                    :file file
                    :position (point))
                   objects))))))
         (sort objects
               (lambda (left right)
                 (string>
                  (or (alist-get 'DECIDED_ON
                                 (perinf-object-properties left)) "")
                  (or (alist-get 'DECIDED_ON
                                 (perinf-object-properties right)) ""))))))
      ('context
       (let ((file (expand-file-name "data/contexts.org" project))
             objects)
         (unless (file-readable-p file)
           (signal 'perinf-storage-error
                   (list (format "Context storage is not readable: %s" file))))
         (with-temp-buffer
           (insert-file-contents file)
           (org-mode)
           (org-map-entries
            (lambda ()
              (when (equal (org-entry-get nil "PERINF_TYPE") "context")
                (let ((start (progn (org-end-of-meta-data t) (point)))
                      (finish (save-excursion
                                (org-end-of-subtree t t) (point))))
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'context
                    :title (org-get-heading t t t t)
                    :status
                    (intern (or (org-entry-get nil "PERINF_STATUS")
                                "active"))
                    :properties
                    `((DESCRIPTION
                       . ,(string-trim
                           (buffer-substring-no-properties start finish))))
                    :file file
                    :position (point))
                   objects))))))
         (sort objects
               (lambda (left right)
                 (string-lessp
                  (perinf-object-title left)
                  (perinf-object-title right))))))
      ('audio-recording
       (let ((file (expand-file-name
                    "media/audio/audio-index.org" project))
             objects)
         (when (file-readable-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (org-map-entries
              (lambda ()
                (when (equal
                       (org-entry-get nil "PERINF_TYPE")
                       "audio-recording")
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'audio-recording
                    :title (org-get-heading t t t t)
                    :status
                    (intern
                     (or (org-entry-get nil "PERINF_STATUS")
                         "available"))
                    :properties
                    `((MEETING_ID . ,(org-entry-get nil "MEETING_ID"))
                      (FILE_REFERENCE
                       . ,(org-entry-get nil "FILE_REFERENCE"))
                      (ORIGINAL_FILE_NAME
                       . ,(org-entry-get nil "ORIGINAL_FILE_NAME"))
                      (CHECKSUM_SHA256
                       . ,(org-entry-get nil "CHECKSUM_SHA256"))
                      (FILE_SIZE_BYTES
                       . ,(org-entry-get nil "FILE_SIZE_BYTES")))
                    :file file
                    :position (point))
                   objects))))))
         (nreverse objects)))
      ('document
       (let ((file (expand-file-name
                    "media/documents/document-index.org" project))
             objects)
         (when (file-readable-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (org-map-entries
              (lambda ()
                (when (equal (org-entry-get nil "PERINF_TYPE") "document")
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'document
                    :title (org-get-heading t t t t)
                    :status (intern (or (org-entry-get nil "PERINF_STATUS")
                                        "available"))
                    :properties
                    `((MEETING_ID . ,(org-entry-get nil "MEETING_ID"))
                      (AGENDA_ITEM_ID
                       . ,(org-entry-get nil "AGENDA_ITEM_ID"))
                      (PARENT_TYPE . ,(org-entry-get nil "PARENT_TYPE"))
                      (PARENT_ID . ,(org-entry-get nil "PARENT_ID"))
                      (FILE_REFERENCE
                       . ,(org-entry-get nil "FILE_REFERENCE"))
                      (ORIGINAL_FILE_NAME
                       . ,(org-entry-get nil "ORIGINAL_FILE_NAME"))
                      (CHECKSUM_SHA256
                       . ,(org-entry-get nil "CHECKSUM_SHA256"))
                      (FILE_SIZE_BYTES
                       . ,(org-entry-get nil "FILE_SIZE_BYTES")))
                    :file file
                    :position (point))
                   objects))))))
         (nreverse objects)))
      ('transcript
       (let ((directory (expand-file-name "data/transcripts" project))
             objects)
         (dolist (file
                  (and (file-directory-p directory)
                       (directory-files-recursively directory "\\.org\\'")))
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (goto-char (point-min))
             (org-map-entries
              (lambda ()
                (when (equal (org-entry-get nil "PERINF_TYPE")
                             "transcript")
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'transcript
                    :title (org-get-heading t t t t)
                    :status
                    (intern
                     (or (org-entry-get nil "PERINF_STATUS") "raw"))
                    :properties
                    `((MEETING_ID . ,(org-entry-get nil "MEETING_ID"))
                      (AUDIO_ID . ,(org-entry-get nil "AUDIO_ID"))
                      (TRANSCRIPTION_METHOD
                       . ,(org-entry-get nil "TRANSCRIPTION_METHOD"))
                      (ORIGINAL_FILE_NAME
                       . ,(org-entry-get nil "ORIGINAL_FILE_NAME"))
                      (SOURCE_CHECKSUM_SHA256
                       . ,(org-entry-get nil "SOURCE_CHECKSUM_SHA256"))
                      (CONTENT_CHECKSUM_SHA256
                       . ,(org-entry-get nil "CONTENT_CHECKSUM_SHA256")))
                    :file file
                    :position (point))
                   objects))))))
         (nreverse objects)))
      ('minutes
       (let ((directory (expand-file-name "data/minutes" project))
             objects)
         (dolist (file
                  (and (file-directory-p directory)
                       (directory-files-recursively directory "\\.org\\'")))
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (org-map-entries
              (lambda ()
                (when (equal (org-entry-get nil "PERINF_TYPE") "minutes")
                  (push
                   (make-perinf-object
                    :id (org-entry-get nil "ID")
                    :type 'minutes
                    :title (org-get-heading t t t t)
                    :status
                    (intern (or (org-entry-get nil "PERINF_STATUS")
                                "ai-draft"))
                    :properties
                    `((MEETING_ID . ,(org-entry-get nil "MEETING_ID"))
                      (TRANSCRIPT_ID
                       . ,(org-entry-get nil "TRANSCRIPT_ID"))
                      (GENERATION_METHOD
                       . ,(org-entry-get nil "GENERATION_METHOD"))
                      (SOURCE_CHECKSUM_SHA256
                       . ,(org-entry-get nil "SOURCE_CHECKSUM_SHA256"))
                      (CONTENT_CHECKSUM_SHA256
                       . ,(org-entry-get nil "CONTENT_CHECKSUM_SHA256"))
                      (APPROVED_CONTENT_CHECKSUM_SHA256
                       . ,(org-entry-get
                           nil "APPROVED_CONTENT_CHECKSUM_SHA256"))
                      (APPROVED_AT . ,(org-entry-get nil "APPROVED_AT"))
                      (APPROVED_BY . ,(org-entry-get nil "APPROVED_BY"))
                      (SUBMITTED_AT . ,(org-entry-get nil "SUBMITTED_AT"))
                      (SUBMITTED_BY . ,(org-entry-get nil "SUBMITTED_BY"))
                      (REJECTED_AT . ,(org-entry-get nil "REJECTED_AT"))
                      (REJECTED_BY . ,(org-entry-get nil "REJECTED_BY"))
                      (REJECTION_REASON
                       . ,(org-entry-get nil "REJECTION_REASON")))
                    :file file
                    :position (point))
                   objects))))))
         (nreverse objects)))
      (_ (signal 'perinf-storage-error
                 (list (format "Listing is not implemented for: %S"
                               type)))))))

(defun perinf-storage--file-sha256 (file)
  "Return the SHA-256 checksum for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun perinf-storage--append-review-event (event actor timestamp &optional reason)
  "Append review EVENT by ACTOR at TIMESTAMP to the current minutes entry."
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n"))
  (insert "\n** Review event — " event "\n"
          ":PROPERTIES:\n"
          ":PERINF_SECTION: review-event\n"
          ":REVIEW_EVENT:   " event "\n"
          ":REVIEW_ACTOR:   " actor "\n"
          ":REVIEWED_AT:    " timestamp "\n")
  (when reason
    (insert ":REVIEW_REASON: " reason "\n"))
  (insert ":END:\n"))

(defun perinf-storage-attach-audio
    (meeting-id source-file &optional project-directory)
  "Copy SOURCE-FILE into the project and attach it to MEETING-ID."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id meeting-id project))
         (meeting-file (perinf-object-file meeting))
         (audio-id (concat "audio-" (org-id-uuid)))
         (start-at
          (alist-get 'START_AT (perinf-object-properties meeting)))
         (year (if start-at (substring start-at 0 4)
                 (format-time-string "%Y")))
         (extension (or (file-name-extension source-file) "audio"))
         (relative
          (format "media/audio/%s/%s.%s"
                  year audio-id (downcase extension)))
         (destination (expand-file-name relative project))
         (index-file
          (expand-file-name "media/audio/audio-index.org" project))
         (original-name (file-name-nondirectory source-file))
         (now (perinf-storage--iso-now)))
    (unless (file-readable-p source-file)
      (signal 'perinf-storage-error
              (list (format "Audio file is not readable: %s" source-file))))
    (when (alist-get 'AUDIO_ID (perinf-object-properties meeting))
      (user-error "This meeting already has an audio recording"))
    (make-directory (file-name-directory destination) t)
    (copy-file source-file destination nil)
    (let ((checksum (perinf-storage--file-sha256 destination))
          (size (file-attribute-size (file-attributes destination))))
      (unless (file-exists-p index-file)
        (with-temp-file index-file
          (insert "#+title: Audio recordings\n#+startup: overview\n")))
      (with-temp-buffer
        (insert-file-contents index-file)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n* " (perinf-storage--safe-line original-name) "\n"
                ":PROPERTIES:\n"
                ":ID:                 " audio-id "\n"
                ":PERINF_TYPE:        audio-recording\n"
                ":PERINF_STATUS:      available\n"
                ":MEETING_ID:         " meeting-id "\n"
                ":FILE_REFERENCE:     " relative "\n"
                ":ORIGINAL_FILE_NAME: " original-name "\n"
                ":CHECKSUM_SHA256:    " checksum "\n"
                ":FILE_SIZE_BYTES:    " (number-to-string size) "\n"
                ":IMPORTED_AT:        " now "\n"
                ":END:\n")
        (perinf-storage--atomic-write-buffer (current-buffer) index-file))
      (with-temp-buffer
        (insert-file-contents meeting-file)
        (org-mode)
        (unless (perinf-storage--find-id meeting-id)
          (signal 'perinf-object-not-found (list meeting-id)))
        (org-entry-put nil "AUDIO_ID" audio-id)
        (org-entry-put nil "AUDIO_STATUS" "available")
        (org-entry-put nil "MODIFIED_AT" now)
        (perinf-storage--atomic-write-buffer
         (current-buffer) meeting-file))
      (make-perinf-object
       :id audio-id
       :type 'audio-recording
       :title original-name
       :status 'available
       :properties
       `((MEETING_ID . ,meeting-id)
         (FILE_REFERENCE . ,relative)
         (ORIGINAL_FILE_NAME . ,original-name)
         (CHECKSUM_SHA256 . ,checksum)
         (FILE_SIZE_BYTES . ,(number-to-string size)))
       :file index-file))))

(defun perinf-storage-attach-document
    (meeting-id agenda-item-id source-file &optional project-directory)
  "Copy SOURCE-FILE into managed storage and attach it to a meeting or agenda item."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id meeting-id project))
         (agenda-item
          (and agenda-item-id
               (seq-find
                (lambda (item)
                  (equal (perinf-object-id item) agenda-item-id))
                (perinf-storage--list-agenda-items meeting-id project))))
         (document-id (concat "document-" (org-id-uuid)))
         (start-at (alist-get 'START_AT (perinf-object-properties meeting)))
         (year (if start-at (substring start-at 0 4)
                 (format-time-string "%Y")))
         (extension (file-name-extension source-file))
         (relative
          (format "media/documents/%s/%s%s"
                  year document-id
                  (if extension (concat "." (downcase extension)) "")))
         (destination (expand-file-name relative project))
         (index-file
          (expand-file-name "media/documents/document-index.org" project))
         (original-name (file-name-nondirectory source-file))
         (parent-type (if agenda-item-id "agenda-item" "meeting"))
         (parent-id (or agenda-item-id meeting-id))
         (now (perinf-storage--iso-now)))
    (unless (file-readable-p source-file)
      (signal 'perinf-storage-error
              (list (format "Document is not readable: %s" source-file))))
    (when (and agenda-item-id (null agenda-item))
      (signal 'perinf-object-not-found (list agenda-item-id)))
    (make-directory (file-name-directory destination) t)
    (copy-file source-file destination nil)
    (let ((checksum (perinf-storage--file-sha256 destination))
          (size (file-attribute-size (file-attributes destination))))
      (unless (file-exists-p index-file)
        (with-temp-file index-file
          (insert "#+title: Documents\n#+startup: overview\n")))
      (with-temp-buffer
        (insert-file-contents index-file)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n* " (perinf-storage--safe-line original-name) "\n"
                ":PROPERTIES:\n"
                ":ID:                 " document-id "\n"
                ":PERINF_TYPE:        document\n"
                ":PERINF_STATUS:      available\n"
                ":MEETING_ID:         " meeting-id "\n"
                (if agenda-item-id
                    (concat ":AGENDA_ITEM_ID:     " agenda-item-id "\n") "")
                ":PARENT_TYPE:        " parent-type "\n"
                ":PARENT_ID:          " parent-id "\n"
                ":FILE_REFERENCE:     " relative "\n"
                ":ORIGINAL_FILE_NAME: " original-name "\n"
                ":CHECKSUM_SHA256:    " checksum "\n"
                ":FILE_SIZE_BYTES:    " (number-to-string size) "\n"
                ":IMPORTED_AT:        " now "\n"
                ":END:\n")
        (perinf-storage--atomic-write-buffer (current-buffer) index-file))
      (make-perinf-object
       :id document-id :type 'document :title original-name :status 'available
       :properties
       `((MEETING_ID . ,meeting-id)
         (AGENDA_ITEM_ID . ,agenda-item-id)
         (PARENT_TYPE . ,parent-type)
         (PARENT_ID . ,parent-id)
         (FILE_REFERENCE . ,relative)
         (ORIGINAL_FILE_NAME . ,original-name)
         (CHECKSUM_SHA256 . ,checksum)
         (FILE_SIZE_BYTES . ,(number-to-string size)))
       :file index-file))))

(defun perinf-storage-import-transcript
    (meeting-id source-file &optional project-directory)
  "Import SOURCE-FILE as an immutable raw transcript for MEETING-ID."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id meeting-id project))
         (meeting-properties (perinf-object-properties meeting))
         (meeting-file (perinf-object-file meeting))
         (audio-id (alist-get 'AUDIO_ID meeting-properties))
         (start-at (alist-get 'START_AT meeting-properties))
         (year (if start-at (substring start-at 0 4)
                 (format-time-string "%Y")))
         (transcript-id (concat "transcript-" (org-id-uuid)))
         (directory
          (expand-file-name (format "data/transcripts/%s" year) project))
         (file
          (expand-file-name (format "%s.org" transcript-id) directory))
         (original-name (file-name-nondirectory source-file))
         (now (perinf-storage--iso-now))
         content source-checksum content-checksum)
    (unless audio-id
      (user-error "Attach an audio recording before importing a transcript"))
    (when (alist-get 'TRANSCRIPT_ID meeting-properties)
      (user-error "This meeting already has a raw transcript"))
    (unless (file-readable-p source-file)
      (signal 'perinf-storage-error
              (list
               (format "Transcript file is not readable: %s" source-file))))
    (setq source-checksum (perinf-storage--file-sha256 source-file))
    (with-temp-buffer
      (insert-file-contents source-file)
      (setq content (buffer-string)))
    (when (string-empty-p (string-trim content))
      (user-error "The transcript file is empty"))
    (setq content-checksum
          (secure-hash 'sha256 (encode-coding-string content 'utf-8)))
    (make-directory directory t)
    (with-temp-buffer
      (insert "#+title: Raw transcript — "
              (perinf-object-title meeting)
              "\n#+startup: content\n\n"
              "* Raw transcript — " (perinf-object-title meeting) "\n"
              ":PROPERTIES:\n"
              ":ID:                      " transcript-id "\n"
              ":PERINF_TYPE:             transcript\n"
              ":PERINF_STATUS:           raw\n"
              ":MEETING_ID:              " meeting-id "\n"
              ":AUDIO_ID:                " audio-id "\n"
              ":TRANSCRIPTION_METHOD:    imported\n"
              ":ORIGINAL_FILE_NAME:      " original-name "\n"
              ":SOURCE_CHECKSUM_SHA256:  " source-checksum "\n"
              ":CONTENT_CHECKSUM_SHA256: " content-checksum "\n"
              ":CREATED_AT:              " now "\n"
              ":END:\n\n"
              "** Raw content\n"
              ":PROPERTIES:\n"
              ":PERINF_SECTION: raw-content\n"
              ":END:\n\n"
              content)
      (unless (bolp) (insert "\n"))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (with-temp-buffer
      (insert-file-contents meeting-file)
      (org-mode)
      (unless (perinf-storage--find-id meeting-id)
        (signal 'perinf-object-not-found (list meeting-id)))
      (org-entry-put nil "TRANSCRIPT_ID" transcript-id)
      (org-entry-put nil "TRANSCRIPT_STATUS" "raw")
      (org-entry-put nil "MODIFIED_AT" now)
      (perinf-storage--atomic-write-buffer (current-buffer) meeting-file))
    (make-perinf-object
     :id transcript-id
     :type 'transcript
     :title (format "Raw transcript — %s"
                    (perinf-object-title meeting))
     :status 'raw
     :properties
     `((MEETING_ID . ,meeting-id)
       (AUDIO_ID . ,audio-id)
       (TRANSCRIPTION_METHOD . "imported")
       (ORIGINAL_FILE_NAME . ,original-name)
       (SOURCE_CHECKSUM_SHA256 . ,source-checksum)
       (CONTENT_CHECKSUM_SHA256 . ,content-checksum))
     :file file)))

(defun perinf-storage-transcript-content (transcript)
  "Return the immutable raw content stored for TRANSCRIPT."
  (unless (eq (perinf-object-type transcript) 'transcript)
    (signal 'perinf-storage-error
            '("Object is not a transcript")))
  (let ((file (perinf-object-file transcript))
        content)
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Transcript is not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-map-entries
       (lambda ()
         (when (and (not content)
                    (equal
                     (org-entry-get nil "PERINF_SECTION")
                     "raw-content"))
           (org-end-of-meta-data t)
           (let ((start (point))
                 (finish (save-excursion
                           (org-end-of-subtree t t)
                           (point))))
             (setq content
                   (string-trim-right
                    (buffer-substring-no-properties start finish)))))))
      (or content ""))))

(defun perinf-storage-import-generated-minutes
    (meeting-id source-file &optional project-directory)
  "Import SOURCE-FILE as generated draft minutes for MEETING-ID.
Generation itself belongs behind the plugin boundary; the core records the
result, its source transcript, checksums, and approval state."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id meeting-id project))
         (meeting-properties (perinf-object-properties meeting))
         (meeting-file (perinf-object-file meeting))
         (transcript-id (alist-get 'TRANSCRIPT_ID meeting-properties))
         (transcript
          (and transcript-id
               (seq-find
                (lambda (candidate)
                  (equal (perinf-object-id candidate) transcript-id))
                (perinf-storage-list 'transcript project))))
         (start-at (alist-get 'START_AT meeting-properties))
         (year (if start-at (substring start-at 0 4)
                 (format-time-string "%Y")))
         (minutes-id (concat "minutes-" (org-id-uuid)))
         (directory (expand-file-name (format "data/minutes/%s" year) project))
         (file (expand-file-name (format "%s.org" minutes-id) directory))
         (now (perinf-storage--iso-now))
         content content-checksum)
    (unless transcript
      (user-error "Import a raw transcript before importing generated minutes"))
    (when (alist-get 'MINUTES_ID meeting-properties)
      (user-error "This meeting already has minutes"))
    (unless (file-readable-p source-file)
      (signal 'perinf-storage-error
              (list (format "Minutes file is not readable: %s" source-file))))
    (with-temp-buffer
      (insert-file-contents source-file)
      (setq content (buffer-string)))
    (when (string-empty-p (string-trim content))
      (user-error "The minutes file is empty"))
    (setq content-checksum
          (secure-hash 'sha256 (encode-coding-string content 'utf-8)))
    (make-directory directory t)
    (with-temp-buffer
      (insert "#+title: Draft minutes — " (perinf-object-title meeting)
              "\n#+startup: content\n\n"
              "* Draft minutes — " (perinf-object-title meeting) "\n"
              ":PROPERTIES:\n"
              ":ID:                      " minutes-id "\n"
              ":PERINF_TYPE:             minutes\n"
              ":PERINF_STATUS:           ai-draft\n"
              ":MEETING_ID:              " meeting-id "\n"
              ":TRANSCRIPT_ID:           " transcript-id "\n"
              ":GENERATION_METHOD:       external-plugin\n"
              ":SOURCE_CHECKSUM_SHA256:  "
              (or (alist-get 'CONTENT_CHECKSUM_SHA256
                             (perinf-object-properties transcript)) "")
              "\n:CONTENT_CHECKSUM_SHA256: " content-checksum "\n"
              ":CREATED_AT:              " now "\n"
              ":MODIFIED_AT:             " now "\n"
              ":END:\n\n"
              "** Minutes content\n"
              ":PROPERTIES:\n"
              ":PERINF_SECTION: minutes-content\n"
              ":END:\n\n"
              content)
      (unless (bolp) (insert "\n"))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (with-temp-buffer
      (insert-file-contents meeting-file)
      (org-mode)
      (unless (perinf-storage--find-id meeting-id)
        (signal 'perinf-object-not-found (list meeting-id)))
      (org-entry-put nil "MINUTES_ID" minutes-id)
      (org-entry-put nil "MINUTES_STATUS" "ai-draft")
      (org-entry-put nil "MODIFIED_AT" now)
      (perinf-storage--atomic-write-buffer (current-buffer) meeting-file))
    (car (seq-filter
          (lambda (object) (equal (perinf-object-id object) minutes-id))
          (perinf-storage-list 'minutes project)))))

(defun perinf-storage-approve-minutes
    (minutes-id approved-by &optional project-directory)
  "Record human approval of MINUTES-ID by APPROVED-BY."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (minutes
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) minutes-id))
           (perinf-storage-list 'minutes project)))
         (approver (perinf-storage--safe-line (string-trim approved-by)))
         (now (perinf-storage--iso-now))
         (submitted-checksum
          (and minutes
               (alist-get
                'CONTENT_CHECKSUM_SHA256
                (perinf-object-properties minutes))))
         (current-checksum
          (and minutes
               (secure-hash
                'sha256
                (encode-coding-string
                 (perinf-storage-minutes-content minutes) 'utf-8)))))
    (unless minutes
      (signal 'perinf-object-not-found (list minutes-id)))
    (unless (eq (perinf-object-status minutes) 'awaiting-final-approval)
      (user-error
       "Minutes must be submitted for final approval before approval"))
    (when (string-empty-p approver)
      (user-error "The approver name must not be empty"))
    (unless (and submitted-checksum
                 (equal submitted-checksum current-checksum))
      (user-error
       "Minutes changed after submission; reject and resubmit them"))
    (with-temp-buffer
      (insert-file-contents (perinf-object-file minutes))
      (org-mode)
      (unless (perinf-storage--find-id minutes-id)
        (signal 'perinf-object-not-found (list minutes-id)))
      (org-entry-put nil "PERINF_STATUS" "final-approved")
      (org-entry-put nil "APPROVED_AT" now)
      (org-entry-put nil "APPROVED_BY" approver)
      (org-entry-put
       nil "APPROVED_CONTENT_CHECKSUM_SHA256" current-checksum)
      (org-entry-put nil "MODIFIED_AT" now)
      (perinf-storage--append-review-event "approved" approver now)
      (perinf-storage--atomic-write-buffer
       (current-buffer) (perinf-object-file minutes)))
    (let* ((meeting-id
            (alist-get 'MEETING_ID (perinf-object-properties minutes)))
           (meeting (perinf-storage--meeting-by-id meeting-id project)))
      (with-temp-buffer
        (insert-file-contents (perinf-object-file meeting))
        (org-mode)
        (unless (perinf-storage--find-id meeting-id)
          (signal 'perinf-object-not-found (list meeting-id)))
        (org-entry-put nil "MINUTES_STATUS" "final-approved")
        (org-entry-put nil "MODIFIED_AT" now)
        (perinf-storage--atomic-write-buffer
         (current-buffer) (perinf-object-file meeting))))
    (seq-find
     (lambda (candidate) (equal (perinf-object-id candidate) minutes-id))
     (perinf-storage-list 'minutes project))))

(defun perinf-storage-submit-minutes
    (minutes-id submitted-by &optional project-directory)
  "Submit reviewed MINUTES-ID for final approval by SUBMITTED-BY.
The current minutes content is checksummed at submission time."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (minutes
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) minutes-id))
           (perinf-storage-list 'minutes project)))
         (submitter
          (perinf-storage--safe-line (string-trim submitted-by)))
         (now (perinf-storage--iso-now)))
    (unless minutes
      (signal 'perinf-object-not-found (list minutes-id)))
    (unless (memq (perinf-object-status minutes)
                  '(ai-draft manual-draft under-review rejected))
      (user-error "Minutes cannot be submitted from their current status"))
    (when (string-empty-p submitter)
      (user-error "The submitter name must not be empty"))
    (let ((checksum
           (secure-hash
            'sha256
            (encode-coding-string
             (perinf-storage-minutes-content minutes) 'utf-8))))
      (with-temp-buffer
        (insert-file-contents (perinf-object-file minutes))
        (org-mode)
        (unless (perinf-storage--find-id minutes-id)
          (signal 'perinf-object-not-found (list minutes-id)))
        (org-entry-put nil "PERINF_STATUS" "awaiting-final-approval")
        (org-entry-put nil "CONTENT_CHECKSUM_SHA256" checksum)
        (org-entry-put nil "SUBMITTED_AT" now)
        (org-entry-put nil "SUBMITTED_BY" submitter)
        (org-entry-put nil "MODIFIED_AT" now)
        (perinf-storage--append-review-event "submitted" submitter now)
        (perinf-storage--atomic-write-buffer
         (current-buffer) (perinf-object-file minutes))))
    (let* ((meeting-id
            (alist-get 'MEETING_ID (perinf-object-properties minutes)))
           (meeting (perinf-storage--meeting-by-id meeting-id project)))
      (with-temp-buffer
        (insert-file-contents (perinf-object-file meeting))
        (org-mode)
        (unless (perinf-storage--find-id meeting-id)
          (signal 'perinf-object-not-found (list meeting-id)))
        (org-entry-put nil "MINUTES_STATUS" "awaiting-final-approval")
        (org-entry-put nil "MODIFIED_AT" now)
        (perinf-storage--atomic-write-buffer
         (current-buffer) (perinf-object-file meeting))))
    (seq-find
     (lambda (candidate) (equal (perinf-object-id candidate) minutes-id))
     (perinf-storage-list 'minutes project))))

(defun perinf-storage-reject-minutes
    (minutes-id rejected-by reason &optional project-directory)
  "Reject submitted MINUTES-ID by REJECTED-BY with REASON."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (minutes
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) minutes-id))
           (perinf-storage-list 'minutes project)))
         (reviewer (perinf-storage--safe-line (string-trim rejected-by)))
         (explanation (perinf-storage--safe-line (string-trim reason)))
         (now (perinf-storage--iso-now)))
    (unless minutes
      (signal 'perinf-object-not-found (list minutes-id)))
    (unless (eq (perinf-object-status minutes) 'awaiting-final-approval)
      (user-error
       "Only minutes awaiting final approval can be rejected"))
    (when (string-empty-p reviewer)
      (user-error "The reviewer name must not be empty"))
    (when (string-empty-p explanation)
      (user-error "A rejection reason is required"))
    (with-temp-buffer
      (insert-file-contents (perinf-object-file minutes))
      (org-mode)
      (unless (perinf-storage--find-id minutes-id)
        (signal 'perinf-object-not-found (list minutes-id)))
      (org-entry-put nil "PERINF_STATUS" "rejected")
      (org-entry-put nil "REJECTED_AT" now)
      (org-entry-put nil "REJECTED_BY" reviewer)
      (org-entry-put nil "REJECTION_REASON" explanation)
      (org-entry-put nil "MODIFIED_AT" now)
      (perinf-storage--append-review-event
       "rejected" reviewer now explanation)
      (perinf-storage--atomic-write-buffer
       (current-buffer) (perinf-object-file minutes)))
    (let* ((meeting-id
            (alist-get 'MEETING_ID (perinf-object-properties minutes)))
           (meeting (perinf-storage--meeting-by-id meeting-id project)))
      (with-temp-buffer
        (insert-file-contents (perinf-object-file meeting))
        (org-mode)
        (unless (perinf-storage--find-id meeting-id)
          (signal 'perinf-object-not-found (list meeting-id)))
        (org-entry-put nil "MINUTES_STATUS" "rejected")
        (org-entry-put nil "MODIFIED_AT" now)
        (perinf-storage--atomic-write-buffer
         (current-buffer) (perinf-object-file meeting))))
    (seq-find
     (lambda (candidate) (equal (perinf-object-id candidate) minutes-id))
     (perinf-storage-list 'minutes project))))

(defun perinf-storage-minutes-content (minutes)
  "Return the content stored for MINUTES."
  (unless (eq (perinf-object-type minutes) 'minutes)
    (signal 'perinf-storage-error '("Object is not minutes")))
  (let ((file (perinf-object-file minutes))
        content)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-map-entries
       (lambda ()
         (when (and (not content)
                    (equal (org-entry-get nil "PERINF_SECTION")
                           "minutes-content"))
           (org-end-of-meta-data t)
           (setq content
                 (string-trim-right
                  (buffer-substring-no-properties
                   (point)
                   (save-excursion (org-end-of-subtree t t) (point))))))))
      (or content ""))))

(defun perinf-storage-list-review-events (minutes)
  "Return the chronological review events stored for MINUTES."
  (unless (eq (perinf-object-type minutes) 'minutes)
    (signal 'perinf-storage-error '("Object is not minutes")))
  (let ((file (perinf-object-file minutes))
        events)
    (unless (file-readable-p file)
      (signal 'perinf-storage-error
              (list (format "Minutes are not readable: %s" file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get nil "PERINF_SECTION") "review-event")
           (push
            `((event . ,(org-entry-get nil "REVIEW_EVENT"))
              (actor . ,(org-entry-get nil "REVIEW_ACTOR"))
              (reviewed-at . ,(org-entry-get nil "REVIEWED_AT"))
              (reason . ,(org-entry-get nil "REVIEW_REASON")))
            events)))))
    (nreverse events)))

(defun perinf-storage--meeting-by-id (meeting-id project)
  "Return meeting MEETING-ID from PROJECT or signal an error."
  (or (seq-find
       (lambda (meeting)
         (equal (perinf-object-id meeting) meeting-id))
       (perinf-storage-list 'meeting project))
      (signal 'perinf-object-not-found (list meeting-id))))

(defun perinf-storage-set-meeting-status
    (meeting-id new-status &optional project-directory)
  "Set MEETING-ID to NEW-STATUS using the controlled meeting workflow."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id meeting-id project))
         (current-status (perinf-object-status meeting))
         (allowed
          (pcase current-status
            ('planned '(in-progress held postponed cancelled))
            ('postponed '(planned cancelled))
            ('in-progress '(held))
            (_ nil)))
         (now (perinf-storage--iso-now)))
    (unless (memq new-status allowed)
      (user-error "Invalid meeting status transition: %s to %s"
                  current-status new-status))
    (with-temp-buffer
      (insert-file-contents (perinf-object-file meeting))
      (org-mode)
      (unless (perinf-storage--find-id meeting-id)
        (signal 'perinf-object-not-found (list meeting-id)))
      (org-entry-put nil "PERINF_STATUS" (symbol-name new-status))
      (pcase new-status
        ('in-progress
         (org-entry-put nil "ACTUAL_START_AT" now))
        ('held
         (unless (org-entry-get nil "ACTUAL_START_AT")
           (org-entry-put nil "ACTUAL_START_AT" now))
         (org-entry-put nil "ACTUAL_FINISH_AT" now)))
      (org-entry-put nil "MODIFIED_AT" now)
      (perinf-storage--atomic-write-buffer
       (current-buffer) (perinf-object-file meeting)))
    (perinf-storage--meeting-by-id meeting-id project)))

(defun perinf-storage-task-assignee-ids (task)
  "Return all stable person IDs assigned to TASK, including legacy data."
  (let ((properties (perinf-object-properties task)))
    (delete-dups
     (append
      (perinf-storage--split-ids (alist-get 'ASSIGNEE_IDS properties))
      (let ((legacy (alist-get 'ASSIGNEE_ID properties)))
        (and legacy (list legacy)))))))

(defun perinf-storage-assign-task
    (task-id person-ids &optional project-directory)
  "Assign TASK-ID to PERSON-IDS in PROJECT-DIRECTORY.
PERSON-IDS may be one stable ID or a list of IDs."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (task
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) task-id))
           (perinf-storage-list 'task project)))
         (ids (if (listp person-ids) person-ids (list person-ids)))
         (people (perinf-storage-list 'person project))
         (file (expand-file-name "data/tasks.org" project)))
    (unless task
      (signal 'perinf-object-not-found (list task-id)))
    (unless ids
      (user-error "At least one person must be assigned"))
    (dolist (person-id ids)
      (unless (seq-find
               (lambda (candidate)
                 (equal (perinf-object-id candidate) person-id))
               people)
        (signal 'perinf-object-not-found (list person-id))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id task-id)
        (signal 'perinf-object-not-found (list task-id)))
      (org-entry-put nil "ASSIGNEE_IDS" (perinf-storage--join-ids ids))
      (if (= (length ids) 1)
          (org-entry-put nil "ASSIGNEE_ID" (car ids))
        (org-entry-delete nil "ASSIGNEE_ID"))
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find
     (lambda (candidate)
       (equal (perinf-object-id candidate) task-id))
     (perinf-storage-list 'task project))))

(defun perinf-storage-set-task-context
    (task-id context-id &optional project-directory)
  "Place TASK-ID in CONTEXT-ID in PROJECT-DIRECTORY."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (task
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) task-id))
           (perinf-storage-list 'task project)))
         (context
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) context-id))
           (perinf-storage-list 'context project)))
         (file (expand-file-name "data/tasks.org" project)))
    (unless task
      (signal 'perinf-object-not-found (list task-id)))
    (unless context
      (signal 'perinf-object-not-found (list context-id)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id task-id)
        (signal 'perinf-object-not-found (list task-id)))
      (org-entry-put nil "CONTEXT_ID" context-id)
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find
     (lambda (candidate)
       (equal (perinf-object-id candidate) task-id))
     (perinf-storage-list 'task project))))

(defun perinf-storage-set-attendance
    (meeting-id participant-id attendance &optional project-directory)
  "Set PARTICIPANT-ID attendance in MEETING-ID to ATTENDANCE."
  (unless (memq attendance '(invited attended absent excused))
    (user-error "Unsupported attendance status: %s" attendance))
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id meeting-id project))
         (file (perinf-object-file meeting)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (unless (perinf-storage--find-id participant-id)
        (signal 'perinf-object-not-found (list participant-id)))
      (unless (equal (org-entry-get nil "PERINF_TYPE") "participant")
        (signal 'perinf-storage-error
                (list (format "Object is not a participant: %s"
                              participant-id))))
      (org-entry-put
       nil "ATTENDANCE_STATUS" (symbol-name attendance))
      (org-entry-put nil "MODIFIED_AT" (perinf-storage--iso-now))
      (perinf-storage--atomic-write-buffer (current-buffer) file))
    (seq-find
     (lambda (participant)
       (equal (perinf-object-id participant) participant-id))
     (perinf-storage-list-children
      meeting-id 'participants project))))

(defun perinf-storage--add-participant
    (parent-id data project-directory)
  "Add participant DATA below meeting PARENT-ID."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id parent-id project))
         (file (perinf-object-file meeting))
         (person-id (alist-get 'PERSON_ID data))
         (role (or (alist-get 'PARTICIPANT_ROLE data) 'participant))
         (attendance
          (or (alist-get 'ATTENDANCE_STATUS data) 'invited))
         (person
          (seq-find
           (lambda (candidate)
             (equal (perinf-object-id candidate) person-id))
           (perinf-storage-list 'person project)))
         (participant-id (concat "participant-" (org-id-uuid))))
    (unless person
      (signal 'perinf-object-not-found (list person-id)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let (section-position duplicate)
        (org-map-entries
         (lambda ()
           (when (equal (org-entry-get nil "PERINF_SECTION") "participants")
             (setq section-position (point)))
           (when (and (equal (org-entry-get nil "PERINF_TYPE") "participant")
                      (equal (org-entry-get nil "PERSON_ID") person-id))
             (setq duplicate t))))
        (when duplicate
          (user-error "This person is already a participant"))
        (unless section-position
          (signal 'perinf-storage-error
                  (list "Meeting has no participants section")))
        (goto-char section-position)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert "\n*** " (perinf-object-title person) "\n"
                ":PROPERTIES:\n"
                ":ID:                " participant-id "\n"
                ":PERINF_TYPE:       participant\n"
                ":PERSON_ID:         " person-id "\n"
                ":PARTICIPANT_ROLE:  " (symbol-name role) "\n"
                ":ATTENDANCE_STATUS: " (symbol-name attendance) "\n"
                ":END:\n")
        (perinf-storage--atomic-write-buffer (current-buffer) file)))
    participant-id))

(defun perinf-storage--list-participants
    (parent-id project-directory)
  "List participant children below meeting PARENT-ID."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id parent-id project))
         (file (perinf-object-file meeting))
         children)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get nil "PERINF_TYPE") "participant")
           (push
            (make-perinf-object
             :id (org-entry-get nil "ID")
             :type 'participant
             :title (org-get-heading t t t t)
             :status nil
             :properties
             `((PERSON_ID . ,(org-entry-get nil "PERSON_ID"))
               (PARTICIPANT_ROLE
                . ,(intern (org-entry-get nil "PARTICIPANT_ROLE")))
               (ATTENDANCE_STATUS
                . ,(intern (org-entry-get nil "ATTENDANCE_STATUS"))))
             :file file
             :position (point))
            children)))))
    (nreverse children)))

(defun perinf-storage--add-agenda-item
    (parent-id data project-directory)
  "Add agenda item DATA below meeting PARENT-ID."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id parent-id project))
         (file (perinf-object-file meeting))
         (title (perinf-storage--safe-line (or (alist-get 'title data) "")))
         (number (perinf-storage--safe-line
                  (or (alist-get 'AGENDA_NUMBER data) "")))
         (kind (or (alist-get 'AGENDA_KIND data) 'discussion))
         (item-id (concat "agenda-item-" (org-id-uuid))))
    (when (or (string-empty-p title) (string-empty-p number))
      (user-error "Agenda number and title are required"))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let (section-position duplicate)
        (org-map-entries
         (lambda ()
           (when (equal (org-entry-get nil "PERINF_SECTION") "agenda")
             (setq section-position (point)))
           (when (and (equal (org-entry-get nil "PERINF_TYPE") "agenda-item")
                      (equal (org-entry-get nil "AGENDA_NUMBER") number))
             (setq duplicate t))))
        (when duplicate
          (user-error "This agenda number already exists"))
        (unless section-position
          (signal 'perinf-storage-error
                  (list "Meeting has no agenda section")))
        (goto-char section-position)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert "\n*** " number ". " title "\n"
                ":PROPERTIES:\n"
                ":ID:            " item-id "\n"
                ":PERINF_TYPE:   agenda-item\n"
                ":AGENDA_NUMBER: " number "\n"
                ":AGENDA_KIND:   " (symbol-name kind) "\n"
                ":END:\n")
        (perinf-storage--atomic-write-buffer (current-buffer) file)))
    item-id))

(defun perinf-storage--list-agenda-items
    (parent-id project-directory)
  "List agenda item children below meeting PARENT-ID."
  (let* ((project
          (or project-directory
              (signal 'perinf-storage-error
                      '("No project directory supplied"))))
         (meeting (perinf-storage--meeting-by-id parent-id project))
         (file (perinf-object-file meeting))
         children)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get nil "PERINF_TYPE") "agenda-item")
           (push
            (make-perinf-object
             :id (org-entry-get nil "ID")
             :type 'agenda-item
             :title (replace-regexp-in-string
                     "\\`[^.]+\\.[[:space:]]*" ""
                     (org-get-heading t t t t))
             :properties
             `((AGENDA_NUMBER . ,(org-entry-get nil "AGENDA_NUMBER"))
               (AGENDA_KIND
                . ,(intern (org-entry-get nil "AGENDA_KIND"))))
             :file file
             :position (point))
            children)))))
    (sort children
          (lambda (left right)
            (string-lessp
             (alist-get 'AGENDA_NUMBER (perinf-object-properties left))
             (alist-get 'AGENDA_NUMBER (perinf-object-properties right)))))))

(defun perinf-storage-add-child
    (parent-id section child-type data &optional project-directory)
  "Add CHILD-TYPE from DATA below SECTION of PARENT-ID."
  (pcase (list section child-type)
    (`(participants participant)
     (perinf-storage--add-participant parent-id data project-directory))
    (`(agenda agenda-item)
     (perinf-storage--add-agenda-item parent-id data project-directory))
    (_ (signal 'perinf-storage-error
               (list "Unsupported child type or section")))))

(defun perinf-storage-list-children
    (parent-id section &optional project-directory)
  "List child objects below SECTION of PARENT-ID."
  (pcase section
    ('participants
     (perinf-storage--list-participants parent-id project-directory))
    ('agenda
     (perinf-storage--list-agenda-items parent-id project-directory))
    (_ (signal 'perinf-storage-error
               (list "Unsupported child section")))))

(provide 'perinf-storage)

;;; perinf-storage.el ends here
