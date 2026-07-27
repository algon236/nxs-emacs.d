;;; perinf-project.el --- Project metadata for Personal Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'org)
(require 'org-id)
(require 'perinf-project-schema)

(defconst perinf-project-metadata-file "perinf-project.org"
  "File containing authoritative Personal Information System project metadata.")

(defconst perinf-project-directories
  '("data" "data/meetings" "data/transcripts" "data/minutes"
    "media" "media/audio" "archive" "config")
  "Directories created for a new Personal Information System project.")

(defconst perinf-project-data-files
  '(("data/tasks.org" . "Tasks")
    ("data/people.org" . "People")
    ("data/contexts.org" . "Contexts")
    ("data/decisions.org" . "Decisions")
    ("media/audio/audio-index.org" . "Audio recordings"))
  "Initial shared Org data files and their canonical English titles.")

(defun perinf-project-p (directory)
  "Return non-nil when DIRECTORY contains Personal Information System project metadata."
  (file-regular-p
   (expand-file-name perinf-project-metadata-file directory)))

(defun perinf-project-read-metadata (directory)
  "Read and validate project metadata from DIRECTORY.
Return an alist with language-independent property names."
  (let ((file (expand-file-name perinf-project-metadata-file directory)))
    (unless (file-readable-p file)
      (user-error "Personal Information System project metadata is not readable: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (unless (re-search-forward org-heading-regexp nil t)
        (user-error "Personal Information System project metadata has no heading: %s" file))
      (let ((metadata
             (mapcar
              (lambda (property)
                (cons property
                      (org-entry-get nil (symbol-name property))))
              perinf-project-required-metadata)))
        (dolist (entry metadata)
          (unless (cdr entry)
            (user-error "Missing project metadata property: %s" (car entry))))
        metadata))))

(defun perinf-project--iso-now ()
  "Return the current time as an ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))

(defun perinf-project--safe-property-value (value)
  "Return VALUE as a safe, single-line Org property value."
  (string-trim (replace-regexp-in-string "[\n\r]+" " " value)))

(defun perinf-project--write-file (file content)
  "Write CONTENT to FILE, refusing to replace an existing file."
  (when (file-exists-p file)
    (user-error "Refusing to replace existing file: %s" file))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil file nil 'silent)))

(defun perinf-project-create (directory title language date-format time-format)
  "Create a Personal Information System project in DIRECTORY.
TITLE is user-written project content.  LANGUAGE, DATE-FORMAT, and
TIME-FORMAT are language-independent setting symbols.  DIRECTORY must not
already exist.  Return the normalized project directory."
  (let* ((target (directory-file-name (expand-file-name directory)))
         (parent (file-name-directory target))
         (project-id (concat "project-" (org-id-uuid)))
         (metadata-id (concat "project-metadata-" (org-id-uuid)))
         (created-at (perinf-project--iso-now))
         (safe-title (perinf-project--safe-property-value title)))
    (when (file-exists-p target)
      (user-error "Project directory already exists: %s" target))
    (unless (file-directory-p parent)
      (user-error "Parent directory does not exist: %s" parent))
    (unless (memq language '(en da fr de es))
      (user-error "Unsupported interface language: %S" language))
    (unless (memq date-format
                  '(iso day-month-year-dash day-month-year-slash
                    month-day-year-slash localized-long))
      (user-error "Unsupported date format: %S" date-format))
    (unless (memq time-format '(twenty-four-hour twelve-hour))
      (user-error "Unsupported time format: %S" time-format))
    (when (string-empty-p safe-title)
      (user-error "Project title must not be empty"))
    (condition-case error-data
        (progn
          (dolist (relative perinf-project-directories)
            (make-directory (expand-file-name relative target) t))
          (perinf-project--write-file
           (expand-file-name perinf-project-metadata-file target)
           (format
            (concat "#+title: %s\n"
                    "#+language: %s\n\n"
                    "* %s\n"
                    ":PROPERTIES:\n"
                    ":ID:                 %s\n"
                    ":PERINF_TYPE:        project-metadata\n"
                    ":PERINF_STATUS:      active\n"
                    ":PROJECT_ID:         %s\n"
                    ":PROJECT_TITLE:      %s\n"
                    ":SCHEMA_VERSION:     %d\n"
                    ":INTERFACE_LANGUAGE: %s\n"
                    ":DATE_FORMAT:        %s\n"
                    ":TIME_FORMAT:        %s\n"
                    ":CREATED_AT:         %s\n"
                    ":END:\n")
            safe-title language safe-title metadata-id project-id safe-title
            perinf-current-schema-version language date-format time-format
            created-at))
          (dolist (entry perinf-project-data-files)
            (perinf-project--write-file
             (expand-file-name (car entry) target)
             (format "#+title: %s\n#+startup: overview\n" (cdr entry))))
          (file-name-as-directory target))
      (error
       (when (file-directory-p target)
         (delete-directory target t))
       (signal (car error-data) (cdr error-data))))))

(provide 'perinf-project)

;;; perinf-project.el ends here
