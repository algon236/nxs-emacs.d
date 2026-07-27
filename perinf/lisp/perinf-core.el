;;; perinf-core.el --- Main entry point for Personal Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'button)
(require 'perinf-i18n)
(require 'perinf-storage)
(require 'perinf-project)
(require 'perinf-date)
(require 'perinf-context)
(require 'perinf-decision)
(require 'perinf-task)
(require 'perinf-time)
(require 'perinf-meeting)
(require 'perinf-person)
(require 'perinf-object-types)
(require 'perinf-properties)
(require 'perinf-statuses)

(defconst perinf-version "0.1.0"
  "Current Personal Information System application version.")

(defvar perinf-current-project nil
  "Directory of the current Personal Information System project, or nil.")

(defcustom perinf-last-project-directory nil
  "Most recently opened Personal Information System project directory."
  :type '(choice (const :tag "None" nil) directory)
  :group 'perinf)

(defcustom perinf-state-file
  (locate-user-emacs-file "perinf/state.el")
  "File containing local, non-project Personal Information System state.
This file stores convenience data such as the last opened project.  It is not
part of the persistent shared Org data."
  :type 'file
  :group 'perinf)

(defvar perinf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'perinf-core-home)
    (define-key map (kbd "w") #'perinf-core-work)
    (define-key map (kbd "m") #'perinf-core-meetings)
    (define-key map (kbd "r") #'perinf-core-records)
    (define-key map (kbd "a") #'perinf-core-administration)
    (define-key map (kbd "s") #'perinf-core-search)
    (define-key map (kbd "g") #'perinf-core-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `perinf-mode'.")

(defvar-local perinf-current-view 'home
  "View rendered in the current Personal Information System buffer.")

(defvar-local perinf-search-query nil
  "Most recent search query displayed in the current buffer.")

(defvar-local perinf-search-results nil
  "Objects matching `perinf-search-query' in the current buffer.")

(defvar-local perinf-selected-object nil
  "Object displayed in the detail view of the current buffer.")

(define-derived-mode perinf-mode special-mode "Personal Information System"
  "Major mode for the Personal Information System start page.")

(defun perinf-core--insert-button (label action &rest properties)
  "Insert a button with LABEL and ACTION using PROPERTIES."
  (apply #'insert-text-button label
         'action action
         'follow-link t
         properties))

(defun perinf-core--insert-navigation ()
  "Insert the translated Personal Information System main navigation."
  (dolist (entry '((home . perinf-core-home)
                   (work . perinf-core-work)
                   (meetings . perinf-core-meetings)
                   (records . perinf-core-records)
                   (administration . perinf-core-administration)))
    (perinf-core--insert-button
     (perinf-i18n (intern (format "navigation.%s" (car entry))))
     (lambda (button)
       (call-interactively (button-get button 'perinf-command)))
     'perinf-command (cdr entry)
     'face (if (eq perinf-current-view (car entry))
               '(:weight bold :underline t)
             'link))
    (insert "   "))
  (insert "\n"
          (make-string 72 ?─)
          "\n\n"))

(defun perinf-core--project-metadata ()
  "Return metadata for the current project, or nil."
  (when perinf-current-project
    (perinf-storage-read-project perinf-current-project)))

(defun perinf-core--metadata-value (property)
  "Return PROPERTY from current project metadata."
  (alist-get property (perinf-core--project-metadata) nil nil #'eq))

(defun perinf-core--display-setting (property)
  "Return a translated display value for project setting PROPERTY."
  (let* ((value (perinf-core--metadata-value property))
         (key (and value (intern (format "setting.%s" value)))))
    (if key (perinf-i18n key) "")))

(defun perinf-core--task-overdue-p (task)
  "Return non-nil when open TASK has a deadline before today."
  (let ((deadline
         (alist-get 'DEADLINE (perinf-object-properties task))))
    (and deadline
         (not (memq (perinf-object-status task) '(completed cancelled)))
         (string< deadline (format-time-string "%Y-%m-%d")))))

(defun perinf-core--task-sort-key (task)
  "Return a stable work-order key for TASK."
  (let ((deadline
         (alist-get 'DEADLINE (perinf-object-properties task))))
    (cond
     ((memq (perinf-object-status task) '(completed cancelled))
      (format "3-%s" (perinf-object-title task)))
     ((perinf-core--task-overdue-p task)
      (format "0-%s-%s" deadline (perinf-object-title task)))
     (deadline
      (format "1-%s-%s" deadline (perinf-object-title task)))
     (t
      (format "2-%s" (perinf-object-title task))))))

(defun perinf-core--sort-tasks (tasks)
  "Return a work-ordered copy of TASKS."
  (sort
   (copy-sequence tasks)
   (lambda (left right)
     (string-lessp
      (perinf-core--task-sort-key left)
      (perinf-core--task-sort-key right)))))

(defun perinf-core--task-status-label (task)
  "Return the localized workflow status for TASK."
  (perinf-i18n
   (intern (format "status.%s" (perinf-object-status task)))))

(defun perinf-core--render-dashboard-summary
    (tasks meetings people transcripts minutes)
  "Insert a dashboard summary for the supplied core object collections."
  (let ((open-tasks
         (perinf-core--sort-tasks
          (seq-filter
          (lambda (task)
            (not (memq (perinf-object-status task) '(completed cancelled))))
          tasks)))
        (pending-approvals
         (seq-filter
          (lambda (minutes-object)
            (eq (perinf-object-status minutes-object)
                'awaiting-final-approval))
          minutes))
        (overdue-tasks
         (seq-filter #'perinf-core--task-overdue-p tasks))
        (date-format
         (intern (perinf-core--metadata-value 'DATE_FORMAT)))
        (time-format
         (intern (perinf-core--metadata-value 'TIME_FORMAT))))
    (insert "\n"
            (propertize (perinf-i18n 'home.overview) 'face 'bold)
            "\n"
            (format "%s: %d\n" (perinf-i18n 'task.count) (length tasks))
            (format "%s: %d\n"
                    (perinf-i18n 'home.open-tasks)
                    (length open-tasks))
            (format "%s: %d\n"
                    (perinf-i18n 'meeting.count)
                    (length meetings))
            (format "%s: %d\n"
                    (perinf-i18n 'person.count)
                    (length people)))
    (insert (format "%s: " (perinf-i18n 'home.transcripts)))
    (perinf-core--insert-button
     (number-to-string (length transcripts))
     (lambda (_button) (perinf-core-records)))
    (insert "\n" (format "%s: " (perinf-i18n 'home.minutes)))
    (perinf-core--insert-button
     (number-to-string (length minutes))
     (lambda (_button) (perinf-core-records)))
    (insert "\n")
    (insert (format "%s: " (perinf-i18n 'home.pending-approvals)))
    (perinf-core--insert-button
     (number-to-string (length pending-approvals))
     (lambda (_button) (perinf-core-work)))
    (insert "\n")
    (insert (format "%s: " (perinf-i18n 'home.overdue-tasks)))
    (perinf-core--insert-button
     (number-to-string (length overdue-tasks))
     (lambda (_button) (perinf-core-work)))
    (insert "\n")
    (when open-tasks
      (insert "\n"
              (propertize (perinf-i18n 'home.open-tasks) 'face 'bold)
              "\n")
      (dolist (task (seq-take open-tasks 5))
        (insert "☐ " (perinf-object-title task) "\n")))
    (when meetings
      (insert "\n"
              (propertize (perinf-i18n 'home.meetings) 'face 'bold)
              "\n")
      (dolist (meeting
               (seq-take
                (sort (copy-sequence meetings)
                      (lambda (left right)
                        (string<
                         (or (alist-get
                              'START_AT
                              (perinf-object-properties left))
                             "")
                         (or (alist-get
                              'START_AT
                              (perinf-object-properties right))
                             ""))))
                3))
        (let* ((start-at
                (alist-get 'START_AT (perinf-object-properties meeting)))
               (date (and start-at (substring start-at 0 10))))
          (insert "• "
                  (perinf-object-title meeting)
                  (if date
                      (format "  —  %s %s"
                              (perinf-date-format date date-format)
                              (perinf-time-format start-at time-format))
                    "")
                  "\n"))))))

(defun perinf-core--render-home ()
  "Insert the home view."
  (if perinf-current-project
      (let ((tasks (perinf-storage-list 'task perinf-current-project))
            (meetings
             (perinf-storage-list 'meeting perinf-current-project))
            (people (perinf-storage-list 'person perinf-current-project))
            (transcripts
             (perinf-storage-list 'transcript perinf-current-project))
            (minutes
             (perinf-storage-list 'minutes perinf-current-project)))
        (insert (propertize (perinf-i18n 'home.welcome)
                            'face '(:height 1.2 :weight bold))
                "\n\n")
        (insert (format "%s: %s\n\n"
                        (perinf-i18n 'project.title)
                        (perinf-core--metadata-value 'PROJECT_TITLE)))
        (insert (propertize (perinf-i18n 'home.quick-actions)
                            'face 'bold)
                "\n")
        (dolist (entry '((action.new-task . task)
                         (action.new-meeting . meeting)
                         (action.new-person . person)
                         (action.new-decision . decision)
                         (action.new-context . context)
                         (action.find-object . search)))
          (pcase (cdr entry)
            ('task
             (perinf-core--insert-button
              (perinf-i18n (car entry))
              (lambda (_button)
                (call-interactively #'perinf-task-create))))
            ('meeting
             (perinf-core--insert-button
              (perinf-i18n (car entry))
              (lambda (_button)
                (call-interactively #'perinf-meeting-create))))
            ('person
             (perinf-core--insert-button
              (perinf-i18n (car entry))
              (lambda (_button)
                (call-interactively #'perinf-person-create))))
            ('decision
             (perinf-core--insert-button
              (perinf-i18n (car entry))
              (lambda (_button)
                (call-interactively #'perinf-decision-create))))
            ('context
             (perinf-core--insert-button
              (perinf-i18n (car entry))
              (lambda (_button)
                (call-interactively #'perinf-context-create))))
            (_
             (perinf-core--insert-button
              (perinf-i18n (car entry))
              (lambda (_button)
                (call-interactively #'perinf-core-search)))))
          (insert "\n"))
        (if (or tasks meetings people transcripts minutes)
            (perinf-core--render-dashboard-summary
             tasks meetings people transcripts minutes)
          (insert "\n"
                  (propertize (perinf-i18n 'home.empty-project)
                              'face 'shadow)
                  "\n")))
    (insert (perinf-i18n 'home.no-project) "\n\n")
    (perinf-core--insert-button
     (perinf-i18n 'project.create)
     (lambda (_button) (call-interactively #'perinf-core-create-project)))
    (insert "   ")
    (perinf-core--insert-button
     (perinf-i18n 'project.open)
     (lambda (_button) (call-interactively #'perinf-core-select-project)))
    (insert "\n")))

(defun perinf-core--render-work ()
  "Insert the work view."
  (insert (propertize (perinf-i18n 'work.title)
                      'face '(:height 1.2 :weight bold))
          "\n\n")
  (perinf-core--insert-button
   (perinf-i18n 'action.new-task)
   (lambda (_button) (call-interactively #'perinf-task-create)))
  (insert "\n\n")
  (if (not perinf-current-project)
      (insert (perinf-i18n 'home.no-project) "\n")
    (let* ((format-name (perinf-core--metadata-value 'DATE_FORMAT))
           (date-format (intern format-name))
           (time-format
            (intern (perinf-core--metadata-value 'TIME_FORMAT)))
           (tasks
            (perinf-core--sort-tasks
             (perinf-storage-list 'task perinf-current-project)))
           (people (perinf-storage-list 'person perinf-current-project))
           (contexts (perinf-storage-list 'context perinf-current-project))
           (approvals
            (seq-filter
             (lambda (minutes)
               (eq (perinf-object-status minutes)
                   'awaiting-final-approval))
             (perinf-storage-list 'minutes perinf-current-project))))
      (if tasks
          (progn
            (insert (format "%s: %d\n\n"
                            (perinf-i18n 'task.count)
                            (length tasks)))
            (dolist (task tasks)
              (let ((deadline
                     (alist-get 'DEADLINE
                                (perinf-object-properties task)))
                    (completed
                     (eq (perinf-object-status task) 'completed))
                    (terminal
                     (memq (perinf-object-status task)
                           '(completed cancelled)))
                    (assignee
                     (let ((assignee-id
                            (alist-get
                             'ASSIGNEE_ID
                             (perinf-object-properties task))))
                       (and assignee-id
                            (seq-find
                             (lambda (person)
                               (equal
                                (perinf-object-id person) assignee-id))
                             people))))
                    (context
                     (let ((context-id
                            (alist-get
                             'CONTEXT_ID
                             (perinf-object-properties task))))
                       (and context-id
                            (seq-find
                             (lambda (candidate)
                               (equal
                                (perinf-object-id candidate) context-id))
                             contexts)))))
                (insert (if completed "☑ " "☐ "))
                (perinf-core--insert-button
                 (perinf-object-title task)
                 (lambda (button)
                   (perinf-core-show-object
                    (button-get button 'perinf-object)))
                 'perinf-object task
                 'face 'bold)
                (when deadline
                  (insert "  —  "
                          (perinf-i18n 'task.deadline)
                          ": "
                          (perinf-date-format deadline date-format)))
                (insert "  —  " (perinf-core--task-status-label task))
                (when (perinf-core--task-overdue-p task)
                  (insert "  —  "
                          (propertize (perinf-i18n 'task.overdue)
                                      'face 'error)))
                (when assignee
                  (insert "  —  "
                          (perinf-i18n 'task.assignee)
                          ": "
                          (perinf-object-title assignee)))
                (when context
                  (insert "  —  "
                          (perinf-i18n 'task.context)
                          ": "
                          (perinf-object-title context)))
                (unless terminal
                  (insert "   ")
                  (perinf-core--insert-button
                   (perinf-i18n 'action.complete-task)
                   (lambda (button)
                     (perinf-task-complete
                      (button-get button 'perinf-task-id)))
                   'perinf-task-id (perinf-object-id task)))
                (insert "\n"))))
        (insert (perinf-i18n 'task.none) "\n"))
      (insert "\n"
              (propertize (perinf-i18n 'work.approvals) 'face 'bold)
              "\n\n")
      (if approvals
          (dolist (minutes approvals)
            (let* ((properties (perinf-object-properties minutes))
                   (submitted-at (alist-get 'SUBMITTED_AT properties))
                   (submitted-date
                    (and submitted-at (substring submitted-at 0 10))))
              (insert "☐ ")
              (perinf-core--insert-button
               (perinf-object-title minutes)
               (lambda (button)
                 (perinf-core-show-object
                  (button-get button 'perinf-object)))
               'perinf-object minutes
               'face 'bold)
              (when submitted-at
                (insert "  —  "
                        (perinf-date-format submitted-date date-format)
                        " "
                        (perinf-time-format submitted-at time-format)))
              (unless
                  (string-empty-p
                   (or (alist-get 'SUBMITTED_BY properties) ""))
                (insert "  —  "
                        (perinf-i18n 'minutes.submitted-by)
                        ": "
                        (alist-get 'SUBMITTED_BY properties)))
              (insert "\n  ")
              (perinf-core--insert-button
               (perinf-i18n 'minutes.approve)
               (lambda (button)
                 (perinf-meeting-approve-minutes
                  (button-get button 'perinf-minutes-id)))
               'perinf-minutes-id (perinf-object-id minutes))
              (insert "   ")
              (perinf-core--insert-button
               (perinf-i18n 'minutes.reject)
               (lambda (button)
                 (perinf-meeting-reject-minutes
                  (button-get button 'perinf-minutes-id)))
               'perinf-minutes-id (perinf-object-id minutes))
              (insert "\n\n")))
        (insert (perinf-i18n 'work.no-approvals) "\n")))))

(defun perinf-core--render-meetings ()
  "Insert the meetings view."
  (insert (propertize (perinf-i18n 'meetings.title)
                      'face '(:height 1.2 :weight bold))
          "\n\n")
  (perinf-core--insert-button
   (perinf-i18n 'action.new-meeting)
   (lambda (_button) (call-interactively #'perinf-meeting-create)))
  (insert "\n\n")
  (if (not perinf-current-project)
      (insert (perinf-i18n 'home.no-project) "\n")
    (let* ((date-format
            (intern (perinf-core--metadata-value 'DATE_FORMAT)))
           (time-format
            (intern (perinf-core--metadata-value 'TIME_FORMAT)))
           (meetings
            (perinf-storage-list 'meeting perinf-current-project)))
      (if meetings
          (progn
            (insert (format "%s: %d\n\n"
                            (perinf-i18n 'meeting.count)
                            (length meetings)))
            (dolist (meeting meetings)
              (let* ((properties (perinf-object-properties meeting))
                     (start-at (alist-get 'START_AT properties))
                     (finish-at (alist-get 'FINISH_AT properties))
                     (location (alist-get 'LOCATION properties))
                     (date (and start-at (substring start-at 0 10)))
                     (participants
                      (perinf-storage-list-children
                       (perinf-object-id meeting)
                       'participants
                       perinf-current-project))
                     (agenda-items
                      (perinf-storage-list-children
                       (perinf-object-id meeting)
                       'agenda
                       perinf-current-project)))
                (perinf-core--insert-button
                 (perinf-object-title meeting)
                 (lambda (button)
                   (perinf-core-show-object
                    (button-get button 'perinf-object)))
                 'perinf-object meeting
                 'face 'bold)
                (insert "\n  "
                        (perinf-date-format date date-format)
                        "  "
                        (perinf-time-format start-at time-format))
                (when finish-at
                  (insert "–" (perinf-time-format finish-at time-format)))
                (unless (string-empty-p (or location ""))
                  (insert "  —  " location))
                (insert "  —  "
                        (perinf-i18n
                         (intern
                          (format "status.%s"
                                  (perinf-object-status meeting)))))
                (insert "\n  "
                        (format "%s: %d"
                                (perinf-i18n 'meeting.participants)
                                (length participants))
                        "   ")
                (perinf-core--insert-button
                 (perinf-i18n 'action.add-participant)
                 (lambda (button)
                   (perinf-meeting-add-participant
                    (button-get button 'perinf-meeting-id)))
                 'perinf-meeting-id (perinf-object-id meeting))
                (insert "\n  "
                        (format "%s: %d"
                                (perinf-i18n 'agenda.items)
                                (length agenda-items))
                        "   ")
                (perinf-core--insert-button
                 (perinf-i18n 'action.add-agenda-item)
                 (lambda (button)
                   (perinf-meeting-add-agenda-item
                    (button-get button 'perinf-meeting-id)))
                 'perinf-meeting-id (perinf-object-id meeting))
                (when agenda-items
                  (insert "\n")
                  (dolist (item agenda-items)
                    (insert "    "
                            (alist-get
                             'AGENDA_NUMBER
                             (perinf-object-properties item))
                            ". "
                            (perinf-object-title item)
                            "  —  "
                            (perinf-i18n
                             (intern
                              (format
                               "agenda.kind.%s"
                               (alist-get
                                'AGENDA_KIND
                                (perinf-object-properties item)))))
                            "\n")))
                (insert "\n\n"))))
        (insert (perinf-i18n 'meeting.none) "\n")))))

(defun perinf-core--render-records ()
  "Insert the records view."
  (insert (propertize (perinf-i18n 'records.title)
                      'face '(:height 1.2 :weight bold))
          "\n\n")
  (perinf-core--insert-button
   (perinf-i18n 'action.new-person)
   (lambda (_button) (call-interactively #'perinf-person-create)))
  (insert "   ")
  (perinf-core--insert-button
   (perinf-i18n 'action.new-decision)
   (lambda (_button) (call-interactively #'perinf-decision-create)))
  (insert "   ")
  (perinf-core--insert-button
   (perinf-i18n 'action.new-context)
   (lambda (_button) (call-interactively #'perinf-context-create)))
  (insert "\n\n")
  (if (not perinf-current-project)
      (insert (perinf-i18n 'home.no-project) "\n")
    (let ((people
           (perinf-storage-list 'person perinf-current-project))
          (transcripts
           (perinf-storage-list 'transcript perinf-current-project))
          (minutes
           (perinf-storage-list 'minutes perinf-current-project))
          (decisions
           (perinf-storage-list 'decision perinf-current-project))
          (contexts
           (perinf-storage-list 'context perinf-current-project)))
      (insert (propertize (perinf-i18n 'person.register)
                          'face 'bold)
              "\n\n")
      (if people
          (progn
            (insert (format "%s: %d\n\n"
                            (perinf-i18n 'person.count)
                            (length people)))
            (dolist (person people)
              (let* ((properties (perinf-object-properties person))
                     (email (alist-get 'EMAIL properties))
                     (phone (alist-get 'PHONE properties)))
                (insert "• ")
                (perinf-core--insert-button
                 (perinf-object-title person)
                 (lambda (button)
                   (perinf-core-show-object
                    (button-get button 'perinf-object)))
                 'perinf-object person
                 'face 'bold)
                (unless (string-empty-p (or email ""))
                  (insert "  —  " email))
                (unless (string-empty-p (or phone ""))
                  (insert "  —  " phone))
                (insert "\n"))))
        (insert (perinf-i18n 'person.none) "\n"))
      (insert "\n"
              (propertize (perinf-i18n 'records.decisions) 'face 'bold)
              "\n\n")
      (if decisions
          (dolist (decision decisions)
            (let* ((properties (perinf-object-properties decision))
                   (date (alist-get 'DECIDED_ON properties)))
              (insert "• ")
              (perinf-core--insert-button
               (perinf-object-title decision)
               (lambda (button)
                 (perinf-core-show-object
                  (button-get button 'perinf-object)))
               'perinf-object decision
               'face 'bold)
              (when date
                (insert "  —  "
                        (perinf-date-format
                         date
                         (intern
                          (perinf-core--metadata-value 'DATE_FORMAT)))))
              (insert "\n")))
        (insert (perinf-i18n 'decision.none) "\n"))
      (insert "\n"
              (propertize (perinf-i18n 'records.contexts) 'face 'bold)
              "\n\n")
      (if contexts
          (dolist (context contexts)
            (insert "• ")
            (perinf-core--insert-button
             (perinf-object-title context)
             (lambda (button)
               (perinf-core-show-object
                (button-get button 'perinf-object)))
             'perinf-object context
             'face 'bold)
            (insert "\n"))
        (insert (perinf-i18n 'context.none) "\n"))
      (insert "\n"
              (propertize (perinf-i18n 'records.transcripts) 'face 'bold)
              "\n\n")
      (if transcripts
          (dolist (transcript transcripts)
            (insert "• ")
            (perinf-core--insert-button
             (perinf-object-title transcript)
             (lambda (button)
               (perinf-core-show-object
                (button-get button 'perinf-object)))
             'perinf-object transcript
             'face 'bold)
            (insert "  —  "
                    (perinf-i18n
                     (intern
                      (format "status.%s"
                              (perinf-object-status transcript))))
                    "\n"))
        (insert (perinf-i18n 'records.no-transcripts) "\n"))
      (insert "\n"
              (propertize (perinf-i18n 'records.minutes) 'face 'bold)
              "\n\n")
      (if minutes
          (dolist (minutes-object minutes)
            (insert "• ")
            (perinf-core--insert-button
             (perinf-object-title minutes-object)
             (lambda (button)
               (perinf-core-show-object
                (button-get button 'perinf-object)))
             'perinf-object minutes-object
             'face 'bold)
            (insert "  —  "
                    (perinf-i18n
                     (intern
                      (format "status.%s"
                              (perinf-object-status minutes-object))))
                    "\n"))
        (insert (perinf-i18n 'records.no-minutes) "\n")))))

(defun perinf-core--object-type-label (object)
  "Return a translated type label for OBJECT."
  (perinf-i18n
   (intern (format "object.%s" (perinf-object-type object)))))

(defun perinf-core--return-to-object-list (object)
  "Return to the list containing OBJECT."
  (pcase (perinf-object-type object)
    ('task (perinf-core-work))
    ('meeting (perinf-core-meetings))
    ('person (perinf-core-records))
    ('decision (perinf-core-records))
    ('context (perinf-core-records))
    ('transcript
     (let* ((meeting-id
             (alist-get
              'MEETING_ID
              (perinf-object-properties object)))
            (meeting
             (seq-find
              (lambda (candidate)
                (equal (perinf-object-id candidate) meeting-id))
              (perinf-storage-list 'meeting perinf-current-project))))
       (if meeting
           (perinf-core-show-object meeting)
         (perinf-core-meetings))))
    ('minutes
     (let* ((meeting-id
             (alist-get 'MEETING_ID (perinf-object-properties object)))
            (meeting
             (seq-find
              (lambda (candidate)
                (equal (perinf-object-id candidate) meeting-id))
              (perinf-storage-list 'meeting perinf-current-project))))
       (if meeting
           (perinf-core-show-object meeting)
         (perinf-core-meetings))))
    (_ (perinf-core-home))))

(defun perinf-core--render-search ()
  "Insert the latest search and its results."
  (insert (propertize (perinf-i18n 'search.title)
                      'face '(:height 1.2 :weight bold))
          "\n\n")
  (perinf-core--insert-button
   (perinf-i18n 'search.new)
   (lambda (_button) (call-interactively #'perinf-core-search)))
  (insert "\n\n")
  (if (not perinf-search-query)
      (insert (perinf-i18n 'search.instructions) "\n")
    (insert (format "%s: %s\n\n"
                    (perinf-i18n 'search.query)
                    perinf-search-query))
    (if perinf-search-results
        (progn
          (insert (format "%s: %d\n\n"
                          (perinf-i18n 'search.result-count)
                          (length perinf-search-results)))
          (dolist (object perinf-search-results)
            (insert (format "%s  "
                            (perinf-core--object-type-label object)))
            (perinf-core--insert-button
             (perinf-object-title object)
             (lambda (button)
               (perinf-core-show-object
                (button-get button 'perinf-object)))
             'perinf-object object)
            (insert "\n")))
      (insert (perinf-i18n 'search.no-results) "\n"))))

(defun perinf-core--search-objects (query)
  "Return core objects whose titles contain QUERY."
  (let ((case-fold-search t)
        results)
    (dolist (type
             '(task meeting person decision context transcript minutes))
      (dolist (object (perinf-storage-list type perinf-current-project))
        (when (string-match-p
               (regexp-quote query)
               (perinf-object-title object))
          (push object results))))
    (sort results
          (lambda (left right)
            (string-lessp
             (perinf-object-title left)
             (perinf-object-title right))))))

(defun perinf-core-search (query)
  "Search task, meeting, and person titles for QUERY."
  (interactive
   (list (string-trim
          (read-string (perinf-i18n 'search.prompt)
                       perinf-search-query))))
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (when (string-empty-p query)
    (user-error "%s" (perinf-i18n 'search.empty-query)))
  (let ((buffer (get-buffer-create "*Personal Information System*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'perinf-mode)
        (perinf-mode))
      (setq perinf-search-query query
            perinf-search-results (perinf-core--search-objects query)
            perinf-current-view 'search)
      (perinf-core--render))
    (pop-to-buffer buffer)))

(defun perinf-core--detail-value (label value)
  "Insert LABEL and VALUE when VALUE is not empty."
  (unless (string-empty-p (or value ""))
    (insert (format "%s: %s\n" label value))))

(defun perinf-core--render-object-detail ()
  "Insert details for `perinf-selected-object'."
  (let* ((object perinf-selected-object)
         (type (and object (perinf-object-type object)))
         (properties (and object (perinf-object-properties object)))
         (date-format
          (intern (perinf-core--metadata-value 'DATE_FORMAT)))
         (time-format
          (intern (perinf-core--metadata-value 'TIME_FORMAT))))
    (unless object
      (user-error "%s" (perinf-i18n 'details.no-object)))
    (insert (propertize (perinf-object-title object)
                        'face '(:height 1.2 :weight bold))
            "\n\n")
    (perinf-core--insert-button
     (perinf-i18n 'details.back)
     (lambda (button)
       (perinf-core--return-to-object-list
        (button-get button 'perinf-object)))
     'perinf-object object)
    (insert "\n\n")
    (perinf-core--detail-value
     (perinf-i18n 'details.type)
     (perinf-core--object-type-label object))
    (perinf-core--detail-value
     (perinf-i18n 'details.status)
     (perinf-i18n
      (intern (format "status.%s" (perinf-object-status object)))))
    (pcase type
      ('task
       (let* ((deadline (alist-get 'DEADLINE properties))
              (decision-id (alist-get 'DECISION_ID properties))
              (source-decision
               (and decision-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate) decision-id))
                     (perinf-storage-list
                      'decision perinf-current-project))))
              (assignee-id (alist-get 'ASSIGNEE_ID properties))
              (assignee
               (and assignee-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate) assignee-id))
                     (perinf-storage-list
                      'person perinf-current-project))))
              (context-id (alist-get 'CONTEXT_ID properties))
              (context
               (and context-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate) context-id))
                     (perinf-storage-list
                      'context perinf-current-project)))))
         (when deadline
           (perinf-core--detail-value
            (perinf-i18n 'task.deadline)
            (perinf-date-format deadline date-format)))
         (when source-decision
           (insert (perinf-i18n 'task.source-decision) ": ")
           (perinf-core--insert-button
            (perinf-object-title source-decision)
            (lambda (button)
              (perinf-core-show-object
               (button-get button 'perinf-object)))
            'perinf-object source-decision)
           (insert "\n"))
         (when assignee
           (insert (perinf-i18n 'task.assignee) ": ")
           (perinf-core--insert-button
            (perinf-object-title assignee)
            (lambda (button)
              (perinf-core-show-object
               (button-get button 'perinf-object)))
            'perinf-object assignee)
           (insert "\n"))
         (perinf-core--insert-button
          (perinf-i18n
           (if assignee 'task.change-assignee 'task.assign))
          (lambda (button)
            (perinf-task-assign
             (button-get button 'perinf-task-id)))
         'perinf-task-id (perinf-object-id object))
         (insert "   ")
         (perinf-core--insert-button
          (perinf-i18n
           (if context 'task.change-context 'task.set-context))
          (lambda (button)
            (perinf-task-set-context
             (button-get button 'perinf-task-id)))
          'perinf-task-id (perinf-object-id object))
         (when context
           (insert "\n" (perinf-i18n 'task.context) ": ")
           (perinf-core--insert-button
            (perinf-object-title context)
            (lambda (button)
              (perinf-core-show-object
               (button-get button 'perinf-object)))
            'perinf-object context))
         (insert "\n\n")
         (dolist
             (action
              (pcase (perinf-object-status object)
                ('open
                 '((action.start-task . active)
                   (action.wait-task . waiting)
                   (action.complete-task . completed)
                   (action.cancel-task . cancelled)))
                ('active
                 '((action.wait-task . waiting)
                   (action.complete-task . completed)
                   (action.cancel-task . cancelled)))
                ('waiting
                 '((action.start-task . active)
                   (action.reopen-task . open)
                   (action.complete-task . completed)
                   (action.cancel-task . cancelled)))
                ((or 'completed 'cancelled)
                 '((action.reopen-task . open)))))
           (perinf-core--insert-button
            (perinf-i18n (car action))
            (lambda (button)
              (perinf-task-set-status
               (button-get button 'perinf-task-id)
               (button-get button 'perinf-task-status)))
            'perinf-task-id (perinf-object-id object)
            'perinf-task-status (cdr action))
           (insert "   "))
         (insert "\n")))
      ('person
       (perinf-core--detail-value
        (perinf-i18n 'details.email)
        (alist-get 'EMAIL properties))
       (perinf-core--detail-value
        (perinf-i18n 'details.phone)
        (alist-get 'PHONE properties))
       (let* ((assigned-tasks
              (seq-filter
               (lambda (task)
                 (equal
                  (alist-get
                   'ASSIGNEE_ID (perinf-object-properties task))
                  (perinf-object-id object)))
               (perinf-storage-list 'task perinf-current-project)))
              (meeting-memberships
               (delq
                nil
                (mapcar
                 (lambda (meeting)
                   (let ((participant
                          (seq-find
                           (lambda (candidate)
                             (equal
                              (alist-get
                               'PERSON_ID
                               (perinf-object-properties candidate))
                              (perinf-object-id object)))
                           (perinf-storage-list-children
                            (perinf-object-id meeting)
                            'participants
                            perinf-current-project))))
                     (and participant (cons meeting participant))))
                 (perinf-storage-list
                  'meeting perinf-current-project)))))
         (insert "\n"
                 (propertize (perinf-i18n 'person.assigned-tasks)
                             'face 'bold)
                 "\n")
         (if assigned-tasks
             (dolist (task assigned-tasks)
               (insert "• ")
               (perinf-core--insert-button
                (perinf-object-title task)
                (lambda (button)
                  (perinf-core-show-object
                   (button-get button 'perinf-object)))
                'perinf-object task)
               (insert "  —  "
                       (perinf-core--task-status-label task)
                       "\n"))
           (insert (perinf-i18n 'person.no-assigned-tasks) "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'person.meetings) 'face 'bold)
                 "\n")
         (if meeting-memberships
             (dolist (membership meeting-memberships)
               (let* ((meeting (car membership))
                      (participant (cdr membership))
                      (role
                       (alist-get
                        'PARTICIPANT_ROLE
                        (perinf-object-properties participant)))
                      (attendance
                       (alist-get
                        'ATTENDANCE_STATUS
                        (perinf-object-properties participant)))
                      (start-at
                       (alist-get
                        'START_AT (perinf-object-properties meeting))))
                 (insert "• ")
                 (perinf-core--insert-button
                  (perinf-object-title meeting)
                  (lambda (button)
                    (perinf-core-show-object
                     (button-get button 'perinf-object)))
                  'perinf-object meeting)
                 (when start-at
                   (insert "  —  "
                           (perinf-date-format
                            (substring start-at 0 10) date-format)))
                 (insert "  —  "
                         (perinf-i18n
                          (intern (format "role.%s" role)))
                         "  —  "
                         (perinf-i18n
                          (intern
                           (format "attendance.%s" attendance)))
                         "  —  "
                         (perinf-i18n
                          (intern
                           (format "status.%s"
                                   (perinf-object-status meeting))))
                         "\n")))
           (insert (perinf-i18n 'person.no-meetings) "\n"))))
      ('decision
       (let ((date (alist-get 'DECIDED_ON properties)))
         (when date
           (perinf-core--detail-value
            (perinf-i18n 'decision.date)
            (perinf-date-format date date-format))))
       (perinf-core--detail-value
        (perinf-i18n 'decision.rationale)
        (alist-get 'RATIONALE properties))
       (let* ((meeting-id (alist-get 'MEETING_ID properties))
              (minutes-id (alist-get 'MINUTES_ID properties))
              (decision-tasks
               (seq-filter
                (lambda (task)
                  (equal
                   (alist-get
                    'DECISION_ID (perinf-object-properties task))
                   (perinf-object-id object)))
                (perinf-storage-list 'task perinf-current-project)))
              (meeting
               (and meeting-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate) meeting-id))
                     (perinf-storage-list
                      'meeting perinf-current-project))))
              (source-minutes
               (and minutes-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate) minutes-id))
                     (perinf-storage-list
                      'minutes perinf-current-project)))))
         (when meeting
           (insert (perinf-i18n 'decision.source-meeting) ": ")
           (perinf-core--insert-button
            (perinf-object-title meeting)
            (lambda (button)
              (perinf-core-show-object
               (button-get button 'perinf-object)))
            'perinf-object meeting)
           (insert "\n"))
         (when source-minutes
           (insert (perinf-i18n 'decision.source-minutes) ": ")
           (perinf-core--insert-button
            (perinf-object-title source-minutes)
            (lambda (button)
              (perinf-core-show-object
               (button-get button 'perinf-object)))
            'perinf-object source-minutes)
           (insert "\n"))
         (insert "\n")
         (perinf-core--insert-button
          (perinf-i18n 'task.create-from-decision)
          (lambda (button)
            (perinf-task-create-from-decision
             (button-get button 'perinf-decision-id)))
         'perinf-decision-id (perinf-object-id object))
         (insert "\n\n"
                 (propertize (perinf-i18n 'decision.tasks) 'face 'bold)
                 "\n")
         (if decision-tasks
             (dolist (task decision-tasks)
               (insert "• ")
               (perinf-core--insert-button
                (perinf-object-title task)
                (lambda (button)
                  (perinf-core-show-object
                   (button-get button 'perinf-object)))
                'perinf-object task)
               (insert "  —  "
                       (perinf-core--task-status-label task)
                       "\n"))
           (insert (perinf-i18n 'decision.no-tasks) "\n"))))
      ('context
       (perinf-core--detail-value
        (perinf-i18n 'context.description)
        (alist-get 'DESCRIPTION properties))
       (let ((context-tasks
              (seq-filter
               (lambda (task)
                 (equal
                  (alist-get
                   'CONTEXT_ID (perinf-object-properties task))
                  (perinf-object-id object)))
               (perinf-storage-list 'task perinf-current-project))))
         (insert "\n"
                 (propertize (perinf-i18n 'context.tasks) 'face 'bold)
                 "\n")
         (if context-tasks
             (dolist (task context-tasks)
               (insert "• ")
               (perinf-core--insert-button
                (perinf-object-title task)
                (lambda (button)
                  (perinf-core-show-object
                   (button-get button 'perinf-object)))
                'perinf-object task)
               (insert "\n"))
           (insert (perinf-i18n 'context.no-tasks) "\n"))))
      ('meeting
       (let* ((start-at (alist-get 'START_AT properties))
              (finish-at (alist-get 'FINISH_AT properties))
              (actual-start-at (alist-get 'ACTUAL_START_AT properties))
              (actual-finish-at (alist-get 'ACTUAL_FINISH_AT properties))
              (audio-id (alist-get 'AUDIO_ID properties))
              (audio
               (and audio-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate) audio-id))
                     (perinf-storage-list
                      'audio-recording perinf-current-project))))
              (transcript-id (alist-get 'TRANSCRIPT_ID properties))
              (transcript
               (and transcript-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate)
                              transcript-id))
                     (perinf-storage-list
                     'transcript perinf-current-project))))
              (minutes-id (alist-get 'MINUTES_ID properties))
              (minutes
               (and minutes-id
                    (seq-find
                     (lambda (candidate)
                       (equal (perinf-object-id candidate) minutes-id))
                     (perinf-storage-list
                      'minutes perinf-current-project))))
              (date (and start-at (substring start-at 0 10)))
              (participants
               (perinf-storage-list-children
                (perinf-object-id object)
                'participants
                perinf-current-project))
              (agenda-items
               (perinf-storage-list-children
                (perinf-object-id object)
                'agenda
                perinf-current-project))
              (meeting-decisions
               (seq-filter
                (lambda (decision)
                  (equal
                   (alist-get
                    'MEETING_ID (perinf-object-properties decision))
                   (perinf-object-id object)))
                (perinf-storage-list
                 'decision perinf-current-project)))
              (meeting-tasks
               (seq-filter
                (lambda (task)
                  (equal
                   (alist-get
                    'MEETING_ID (perinf-object-properties task))
                   (perinf-object-id object)))
                (perinf-storage-list 'task perinf-current-project))))
         (pcase (perinf-object-status object)
           ('planned
            (perinf-core--insert-button
             (perinf-i18n 'meeting.start)
             (lambda (button)
               (perinf-meeting-start
                (button-get button 'perinf-meeting-id)))
             'perinf-meeting-id (perinf-object-id object))
            (insert "   ")
            (perinf-core--insert-button
             (perinf-i18n 'meeting.mark-held)
             (lambda (button)
               (perinf-meeting-finish
                (button-get button 'perinf-meeting-id)))
             'perinf-meeting-id (perinf-object-id object))
            (insert "   ")
            (perinf-core--insert-button
             (perinf-i18n 'meeting.postpone)
             (lambda (button)
               (perinf-meeting-postpone
                (button-get button 'perinf-meeting-id)))
             'perinf-meeting-id (perinf-object-id object))
            (insert "   ")
            (perinf-core--insert-button
             (perinf-i18n 'meeting.cancel)
             (lambda (button)
               (perinf-meeting-cancel
                (button-get button 'perinf-meeting-id)))
             'perinf-meeting-id (perinf-object-id object))
            (insert "\n\n"))
           ('postponed
            (perinf-core--insert-button
             (perinf-i18n 'meeting.resume-planning)
             (lambda (button)
               (perinf-meeting-resume-planning
                (button-get button 'perinf-meeting-id)))
             'perinf-meeting-id (perinf-object-id object))
            (insert "   ")
            (perinf-core--insert-button
             (perinf-i18n 'meeting.cancel)
             (lambda (button)
               (perinf-meeting-cancel
                (button-get button 'perinf-meeting-id)))
             'perinf-meeting-id (perinf-object-id object))
            (insert "\n\n"))
           ('in-progress
            (perinf-core--insert-button
             (perinf-i18n 'meeting.finish)
             (lambda (button)
               (perinf-meeting-finish
                (button-get button 'perinf-meeting-id)))
             'perinf-meeting-id (perinf-object-id object))
            (insert "\n\n")))
         (when date
           (perinf-core--detail-value
            (perinf-i18n 'details.date)
            (perinf-date-format date date-format)))
         (when start-at
           (perinf-core--detail-value
            (perinf-i18n 'details.start)
            (perinf-time-format start-at time-format)))
         (when finish-at
           (perinf-core--detail-value
            (perinf-i18n 'details.finish)
            (perinf-time-format finish-at time-format)))
         (when actual-start-at
           (perinf-core--detail-value
            (perinf-i18n 'meeting.actual-start)
            (concat
             (perinf-date-format
              (substring actual-start-at 0 10) date-format)
             " "
             (perinf-time-format actual-start-at time-format))))
         (when actual-finish-at
           (perinf-core--detail-value
            (perinf-i18n 'meeting.actual-finish)
            (concat
             (perinf-date-format
              (substring actual-finish-at 0 10) date-format)
             " "
             (perinf-time-format actual-finish-at time-format))))
         (perinf-core--detail-value
          (perinf-i18n 'details.location)
          (alist-get 'LOCATION properties))
         (insert "\n"
                 (propertize (perinf-i18n 'audio.title) 'face 'bold)
                 "\n")
         (if audio
             (progn
               (perinf-core--detail-value
                (perinf-i18n 'audio.file)
                (alist-get
                 'ORIGINAL_FILE_NAME
                 (perinf-object-properties audio)))
               (perinf-core--detail-value
                (perinf-i18n 'details.status)
                (perinf-i18n 'audio.status.available)))
           (insert (perinf-i18n 'audio.none) "   ")
           (perinf-core--insert-button
            (perinf-i18n 'audio.attach)
            (lambda (button)
              (perinf-meeting-attach-audio
               (button-get button 'perinf-meeting-id)))
           'perinf-meeting-id (perinf-object-id object))
           (insert "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'transcript.title)
                             'face 'bold)
                 "\n")
         (cond
          (transcript
           (perinf-core--insert-button
            (perinf-i18n 'transcript.open)
            (lambda (button)
              (perinf-core-show-object
               (button-get button 'perinf-object)))
            'perinf-object transcript)
           (insert "\n")
           (perinf-core--detail-value
            (perinf-i18n 'details.status)
            (perinf-i18n 'transcript.status.raw)))
          ((not audio)
           (insert (perinf-i18n 'transcript.requires-audio) "\n"))
          (t
           (insert (perinf-i18n 'transcript.none) "   ")
           (perinf-core--insert-button
            (perinf-i18n 'transcript.import)
            (lambda (button)
              (perinf-meeting-import-transcript
               (button-get button 'perinf-meeting-id)))
            'perinf-meeting-id (perinf-object-id object))
           (insert "\n")))
         (insert "\n"
                 (propertize (perinf-i18n 'minutes.title) 'face 'bold)
                 "\n")
         (cond
          (minutes
           (perinf-core--insert-button
            (perinf-i18n 'minutes.open)
            (lambda (button)
              (perinf-core-show-object
               (button-get button 'perinf-object)))
            'perinf-object minutes)
           (insert "\n")
           (perinf-core--detail-value
            (perinf-i18n 'details.status)
            (perinf-i18n
             (intern (format "status.%s"
                             (perinf-object-status minutes)))))
           (pcase (perinf-object-status minutes)
             ((or 'ai-draft 'manual-draft 'under-review 'rejected)
              (perinf-core--insert-button
               (perinf-i18n 'minutes.edit)
               (lambda (button)
                 (perinf-meeting-edit-minutes
                  (button-get button 'perinf-minutes-id)))
               'perinf-minutes-id (perinf-object-id minutes))
              (insert "   ")
              (perinf-core--insert-button
               (perinf-i18n 'minutes.submit)
               (lambda (button)
                 (perinf-meeting-submit-minutes
                  (button-get button 'perinf-minutes-id)))
               'perinf-minutes-id (perinf-object-id minutes))
              (insert "\n"))
             ('awaiting-final-approval
             (perinf-core--insert-button
              (perinf-i18n 'minutes.approve)
              (lambda (button)
                (perinf-meeting-approve-minutes
                 (button-get button 'perinf-minutes-id)))
              'perinf-minutes-id (perinf-object-id minutes))
              (insert "   ")
              (perinf-core--insert-button
               (perinf-i18n 'minutes.reject)
               (lambda (button)
                 (perinf-meeting-reject-minutes
                  (button-get button 'perinf-minutes-id)))
               'perinf-minutes-id (perinf-object-id minutes))
              (insert "\n"))))
          ((not transcript)
           (insert (perinf-i18n 'minutes.requires-transcript) "\n"))
          (t
           (insert (perinf-i18n 'minutes.none) "   ")
           (perinf-core--insert-button
            (perinf-i18n 'minutes.import)
            (lambda (button)
              (perinf-meeting-import-generated-minutes
               (button-get button 'perinf-meeting-id)))
            'perinf-meeting-id (perinf-object-id object))
           (insert "\n")))
         (insert "\n"
                 (propertize (perinf-i18n 'meeting.participants)
                             'face 'bold)
                 "\n")
         (if participants
             (dolist (participant participants)
               (let* ((participant-properties
                       (perinf-object-properties participant))
                      (role
                       (alist-get
                        'PARTICIPANT_ROLE participant-properties))
                      (attendance
                       (alist-get
                        'ATTENDANCE_STATUS participant-properties)))
                 (insert "• "
                         (perinf-object-title participant)
                         "  —  "
                         (perinf-i18n
                          (intern (format "role.%s" role)))
                         "  —  "
                         (perinf-i18n
                          (intern
                           (format "attendance.%s" attendance)))
                         "\n  ")
                 (dolist
                     (choice
                      `((attended . attendance.mark-attended)
                        (absent . attendance.mark-absent)
                        (excused . attendance.mark-excused)))
                   (perinf-core--insert-button
                    (perinf-i18n (cdr choice))
                    (lambda (button)
                      (perinf-meeting-set-attendance
                       (button-get button 'perinf-meeting-id)
                       (button-get button 'perinf-participant-id)
                       (button-get button 'perinf-attendance)))
                    'perinf-meeting-id (perinf-object-id object)
                    'perinf-participant-id
                    (perinf-object-id participant)
                    'perinf-attendance (car choice))
                   (insert "   "))
                 (insert "\n")))
           (insert (perinf-i18n 'details.none) "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'agenda.items) 'face 'bold)
                 "\n")
         (if agenda-items
             (dolist (item agenda-items)
               (insert
                (format "%s. %s\n"
                        (alist-get
                         'AGENDA_NUMBER
                         (perinf-object-properties item))
                        (perinf-object-title item))))
           (insert (perinf-i18n 'details.none) "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'meeting.decisions)
                             'face 'bold)
                 "\n")
         (if meeting-decisions
             (dolist (decision meeting-decisions)
               (insert "• ")
               (perinf-core--insert-button
                (perinf-object-title decision)
                (lambda (button)
                  (perinf-core-show-object
                   (button-get button 'perinf-object)))
                'perinf-object decision)
               (insert "\n"))
           (insert (perinf-i18n 'meeting.no-decisions) "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'meeting.tasks) 'face 'bold)
                 "\n")
         (if meeting-tasks
             (dolist (task meeting-tasks)
               (insert "• ")
               (perinf-core--insert-button
                (perinf-object-title task)
                (lambda (button)
                  (perinf-core-show-object
                   (button-get button 'perinf-object)))
                'perinf-object task)
               (insert "  —  "
                       (perinf-core--task-status-label task)
                       "\n"))
           (insert (perinf-i18n 'meeting.no-tasks) "\n"))))
      ('transcript
       (perinf-core--detail-value
        (perinf-i18n 'transcript.file)
        (alist-get 'ORIGINAL_FILE_NAME properties))
       (insert "\n"
               (propertize (perinf-i18n 'transcript.content)
                           'face 'bold)
               "\n\n"
               (perinf-storage-transcript-content object)
               "\n"))
      ('minutes
       (let ((events (perinf-storage-list-review-events object))
             (minutes-decisions
              (seq-filter
               (lambda (decision)
                 (equal
                  (alist-get
                   'MINUTES_ID (perinf-object-properties decision))
                  (perinf-object-id object)))
               (perinf-storage-list
                'decision perinf-current-project)))
             (minutes-tasks
              (seq-filter
               (lambda (task)
                 (equal
                  (alist-get
                   'MINUTES_ID (perinf-object-properties task))
                  (perinf-object-id object)))
               (perinf-storage-list 'task perinf-current-project))))
         (perinf-core--detail-value
          (perinf-i18n 'minutes.generation-method)
          (alist-get 'GENERATION_METHOD properties))
         (perinf-core--detail-value
          (perinf-i18n 'minutes.approved-by)
          (alist-get 'APPROVED_BY properties))
         (perinf-core--detail-value
          (perinf-i18n 'minutes.approved-at)
          (alist-get 'APPROVED_AT properties))
         (insert "\n"
                 (propertize (perinf-i18n 'minutes.review-history)
                             'face 'bold)
                 "\n")
         (if events
             (dolist (event events)
               (let* ((reviewed-at (alist-get 'reviewed-at event))
                      (review-date
                       (and reviewed-at (substring reviewed-at 0 10))))
                 (insert
                  "• "
                  (perinf-i18n
                   (intern
                    (format "minutes.review-event.%s"
                            (alist-get 'event event))))
                  " — "
                  (or (alist-get 'actor event) "")
                  " — "
                  (if reviewed-at
                      (concat
                       (perinf-date-format review-date date-format)
                       " "
                       (perinf-time-format reviewed-at time-format))
                    "")
                  "\n"))
               (unless (string-empty-p (or (alist-get 'reason event) ""))
                 (insert "  "
                         (perinf-i18n 'minutes.rejection-reason)
                         ": "
                         (alist-get 'reason event)
                         "\n")))
           (insert (perinf-i18n 'minutes.review-history-none) "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'minutes.content) 'face 'bold)
                 "\n\n"
                 (perinf-storage-minutes-content object)
                 "\n")
         (when (eq (perinf-object-status object) 'final-approved)
           (insert "\n")
           (perinf-core--insert-button
            (perinf-i18n 'decision.register-from-minutes)
            (lambda (button)
              (perinf-decision-create-from-minutes
               (button-get button 'perinf-minutes-id)))
            'perinf-minutes-id (perinf-object-id object))
           (insert "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'minutes.decisions)
                             'face 'bold)
                 "\n")
         (if minutes-decisions
             (dolist (decision minutes-decisions)
               (insert "• ")
               (perinf-core--insert-button
                (perinf-object-title decision)
                (lambda (button)
                  (perinf-core-show-object
                   (button-get button 'perinf-object)))
                'perinf-object decision)
               (insert "\n"))
           (insert (perinf-i18n 'minutes.no-decisions) "\n"))
         (insert "\n"
                 (propertize (perinf-i18n 'minutes.tasks) 'face 'bold)
                 "\n")
         (if minutes-tasks
             (dolist (task minutes-tasks)
               (insert "• ")
               (perinf-core--insert-button
                (perinf-object-title task)
                (lambda (button)
                  (perinf-core-show-object
                   (button-get button 'perinf-object)))
                'perinf-object task)
               (insert "  —  "
                       (perinf-core--task-status-label task)
                       "\n"))
           (insert (perinf-i18n 'minutes.no-tasks) "\n")))))))

(defun perinf-core-show-object (object)
  "Show detail view for OBJECT."
  (let ((buffer (get-buffer-create "*Personal Information System*")))
    (with-current-buffer buffer
      (setq perinf-selected-object object
            perinf-current-view 'detail)
      (perinf-core--render))
    (pop-to-buffer buffer)))

(defun perinf-core--render-administration ()
  "Insert the administration view."
  (insert (propertize (perinf-i18n 'administration.title)
                      'face '(:height 1.2 :weight bold))
          "\n\n")
  (if perinf-current-project
      (insert
       (format "%s: %s\n"
               (perinf-i18n 'project.title)
               (perinf-core--metadata-value 'PROJECT_TITLE))
       (format "%s: %s\n"
               (perinf-i18n 'project.location)
               (abbreviate-file-name perinf-current-project))
       (format "%s: %s\n"
               (perinf-i18n 'project.language)
               (perinf-core--metadata-value 'INTERFACE_LANGUAGE))
       (format "%s: %s\n"
               (perinf-i18n 'project.date-format)
               (perinf-core--display-setting 'DATE_FORMAT))
       (format "%s: %s\n"
               (perinf-i18n 'project.time-format)
               (perinf-core--display-setting 'TIME_FORMAT))
       (format "%s: %s\n"
               (perinf-i18n 'project.schema-version)
               (perinf-core--metadata-value 'SCHEMA_VERSION)))
    (insert (perinf-i18n 'home.no-project) "\n")))

(defun perinf-core--render ()
  "Render the current Personal Information System view."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (perinf-i18n 'app.name)
                        'face '(:height 1.5 :weight bold))
            "\n")
    (insert (format "%s %s\n\n"
                    (perinf-i18n 'home.version)
                    perinf-version))
    (perinf-core--insert-navigation)
    (condition-case error-data
        (pcase perinf-current-view
          ('home (perinf-core--render-home))
          ('work (perinf-core--render-work))
          ('meetings (perinf-core--render-meetings))
          ('records (perinf-core--render-records))
          ('search (perinf-core--render-search))
          ('detail (perinf-core--render-object-detail))
          ('administration (perinf-core--render-administration))
          (_ (perinf-core--render-home)))
      (error
       (insert (format "%s: %s\n"
                       (perinf-i18n 'common.error)
                       (error-message-string error-data)))))
    (insert "\n")
    (perinf-core--insert-button
     (perinf-i18n 'common.refresh)
     (lambda (_button) (perinf-core-refresh)))
    (insert "   ")
    (perinf-core--insert-button
     (perinf-i18n 'common.close)
     (lambda (_button) (quit-window)))
    (insert "\n\n"
            (propertize (perinf-i18n 'home.storage-note)
                        'face 'shadow)
            "\n")
    (goto-char (point-min))))

(defun perinf-core--show-view (view)
  "Show Personal Information System VIEW in the main buffer."
  (let ((buffer (get-buffer-create "*Personal Information System*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'perinf-mode)
        (perinf-mode))
      (setq perinf-current-view view)
      (perinf-core--render))
    (pop-to-buffer buffer)))

(defun perinf-core--unavailable (_feature)
  "Explain that a selected FEATURE belongs to the next milestone."
  (message "%s" (perinf-i18n 'common.not-implemented)))

;;;###autoload
(defun perinf-core-open (&optional project-directory)
  "Open the Personal Information System start page.
With PROJECT-DIRECTORY, display metadata from that Personal Information System project."
  (interactive)
  (perinf-i18n-load-locales)
  (perinf-core--load-state)
  (let ((candidate (or project-directory
                       perinf-current-project
                       perinf-last-project-directory)))
    (when candidate
      (if (perinf-project-p candidate)
          (perinf-core--activate-project candidate)
        (when project-directory
          (user-error "Not a Personal Information System project: %s" candidate)))))
  (let ((buffer (get-buffer-create "*Personal Information System*")))
    (with-current-buffer buffer
      (perinf-mode)
      (setq perinf-current-view 'home)
      (perinf-core--render))
    (pop-to-buffer buffer)))

(defun perinf-core-home ()
  "Show the Personal Information System home view."
  (interactive)
  (perinf-core--show-view 'home))

(defun perinf-core-work ()
  "Show the Personal Information System work view."
  (interactive)
  (perinf-core--show-view 'work))

(defun perinf-core-meetings ()
  "Show the Personal Information System meetings view."
  (interactive)
  (perinf-core--show-view 'meetings))

(defun perinf-core-records ()
  "Show the Personal Information System records view."
  (interactive)
  (perinf-core--show-view 'records))

(defun perinf-core-administration ()
  "Show the Personal Information System administration view."
  (interactive)
  (perinf-core--show-view 'administration))

(defun perinf-core--load-state ()
  "Load local Personal Information System convenience state without evaluating code."
  (when (file-readable-p perinf-state-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents perinf-state-file)
          (let ((state (read (current-buffer))))
            (when (and (listp state)
                       (stringp (plist-get state :last-project)))
              (setq perinf-last-project-directory
                    (plist-get state :last-project)))))
      (error nil))))

(defun perinf-core--save-state ()
  "Atomically save local Personal Information System convenience state."
  (let* ((directory (file-name-directory perinf-state-file))
         (temporary nil))
    (make-directory directory t)
    (setq temporary
          (make-temp-file (expand-file-name ".perinf-state-" directory)))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil)
                  (print-level nil))
              (prin1 (list :last-project perinf-last-project-directory)
                     (current-buffer))
              (insert "\n")))
          (rename-file temporary perinf-state-file t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun perinf-core--activate-project (directory)
  "Activate and validate the Personal Information System project in DIRECTORY."
  (let* ((normalized
          (file-name-as-directory (expand-file-name directory)))
         (metadata (perinf-project-read-metadata normalized))
         (language-name (alist-get 'INTERFACE_LANGUAGE metadata nil nil #'eq))
         (language (intern language-name)))
    (unless (perinf-project-p normalized)
      (user-error "Not a Personal Information System project: %s" normalized))
    (setq perinf-current-project normalized
          perinf-last-project-directory normalized
          perinf-interface-language language)
    (perinf-core--save-state)
    normalized))

;;;###autoload
(defun perinf-core-create-project
    (directory title language date-format time-format)
  "Interactively create and open a new Personal Information System project."
  (interactive
   (let* ((directory
           (read-directory-name
            (perinf-i18n 'project.create-directory-prompt)
            (expand-file-name "Personal Information System" (or (getenv "HOME") default-directory))
            nil nil))
          (title (read-string (perinf-i18n 'project.title-prompt)))
          (language
           (intern
            (completing-read
             (perinf-i18n 'project.language-prompt)
             '("da" "en" "fr" "de" "es") nil t
             (symbol-name perinf-interface-language))))
          (date-format
           (intern
            (completing-read
             (perinf-i18n 'project.date-format-prompt)
             '("day-month-year-dash" "day-month-year-slash"
               "month-day-year-slash" "iso" "localized-long")
             nil t "day-month-year-dash")))
          (time-format
           (intern
            (completing-read
             (perinf-i18n 'project.time-format-prompt)
             '("twenty-four-hour" "twelve-hour")
             nil t "twenty-four-hour"))))
     (list directory title language date-format time-format)))
  (perinf-i18n-load-locales)
  (let ((project
         (perinf-project-create
          directory title language date-format time-format)))
    (perinf-core--activate-project project)
    (perinf-core-open project)
    (message "%s" (perinf-i18n 'project.created))))

;;;###autoload
(defun perinf-core-select-project (directory)
  "Select and open the existing Personal Information System project in DIRECTORY."
  (interactive
   (list
    (read-directory-name
     (perinf-i18n 'project.open-directory-prompt)
     (or perinf-last-project-directory default-directory)
     nil t)))
  (perinf-i18n-load-locales)
  (perinf-core--activate-project directory)
  (perinf-core-open directory))

(defun perinf-core-refresh ()
  "Refresh the current Personal Information System view."
  (interactive)
  (unless (derived-mode-p 'perinf-mode)
    (user-error "This is not a Personal Information System buffer"))
  (perinf-core--render))

(provide 'perinf-core)

;;; perinf-core.el ends here
