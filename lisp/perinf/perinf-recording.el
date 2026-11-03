;;; perinf-recording.el --- Guided meeting recordings -*- lexical-binding: t; -*-

;; Copyright (C) 2026, Niels Søndergaard, Nivaa, Denmark.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Danish guided macOS recording workflow.  The user exports the recording
;; and explicitly imports it.  Never read or modify the recorder's database.

;;; Code:

(require 'button)
(require 'perinf-meeting)

(declare-function perinf-core--call-function-from-button "perinf-core")

(defcustom perinf-recording-application "Memoer"
  "Suggested macOS recording application name or .app path."
  :type 'string
  :group 'perinf)

(defun perinf-recording--directory (meeting-id project)
  "Return a stable export directory for MEETING-ID in PROJECT."
  (expand-file-name
   (concat "media/audio/incoming/" (secure-hash 'sha256 meeting-id) "/")
   project))

(defun perinf-recording--open (&rest arguments)
  "Ask macOS to open ARGUMENTS, reporting launch errors."
  (unless (eq system-type 'darwin)
    (user-error "Åbning af optageapp kræver macOS"))
  (with-temp-buffer
    (unless (eq 0 (apply #'call-process "/usr/bin/open" nil t nil arguments))
      (user-error "Kunne ikke åbne: %s" (string-trim (buffer-string))))))

(defun perinf-recording--launch (application)
  "Launch APPLICATION, resolving Apple's localized app name by bundle ID."
  (if (member (downcase (string-trim application))
              '("memoer" "memo" "voice memos" "com.apple.voicememos"))
      (perinf-recording--open "-b" "com.apple.VoiceMemos")
    (perinf-recording--open "-a" application)))

(defun perinf-recording--import (meeting-id project source)
  "Import SOURCE for MEETING-ID in PROJECT and verify the saved result."
  (let* ((meeting (perinf-storage--meeting-by-id meeting-id project))
         (index (expand-file-name "media/audio/audio-index.org" project)))
    (unless (and (file-regular-p source) (file-readable-p source)
                 (> (file-attribute-size (file-attributes source)) 0)
                 (member (downcase (or (file-name-extension source) ""))
                         '("m4a" "wav" "mp3" "aiff" "aif" "caf" "aac"
                           "flac" "ogg" "mp4")))
      (user-error "Vælg en færdig, ikke-tom lydfil, f.eks. en m4a-fil"))
    ;; Refuse before copying anything if either metadata file has unsaved edits.
    (dolist (file (list (perinf-object-file meeting) index))
      (let ((buffer (find-buffer-visiting file)))
        (when (and buffer (buffer-modified-p buffer))
          (user-error "Gem først dine ændringer i %s" file))))
    (let* ((audio (perinf-storage-attach-audio meeting-id source project))
           (saved (perinf-storage--meeting-by-id meeting-id project)))
      (unless (equal (perinf-object-id audio)
                     (alist-get 'AUDIO_ID (perinf-object-properties saved)))
        (error "Lydfilens tilknytning kunne ikke bekræftes"))
      audio)))

(defun perinf-recording--choose-file (meeting-id project)
  "Choose a finished recording for MEETING-ID in PROJECT."
  (let* ((directory (perinf-recording--directory meeting-id project))
         (default-directory directory)
         (source (read-file-name "Vælg den færdige lydfil: " directory nil t))
         (audio (perinf-recording--import meeting-id project source))
         (destination
          (expand-file-name
           (alist-get 'FILE_REFERENCE (perinf-object-properties audio)) project)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Lydfilen er gemt og knyttet til mødet:\n\n"
              (perinf-object-title
               (perinf-storage--meeting-by-id meeting-id project))
              "\n\n" destination
              "\n\nKopien er kontrolleret. Originalen er bevaret.\n"
              "Optagelsen i Memoer slettes kun, når du selv vælger det.\n"))
    (message "Lydfilen er gemt i PerInf og knyttet til mødet")))

(defun perinf-recording--button (label function &rest arguments)
  "Insert a LABEL button calling FUNCTION with captured ARGUMENTS."
  (let ((buffer (current-buffer)))
    (insert-text-button
     label 'follow-link t
     'help-echo label
     'action (lambda (_button)
               (perinf-core--call-function-from-button
                (lambda ()
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (apply function arguments))))))))
  (insert "\n\n"))

;;;###autoload
(defun perinf-recording-start (meeting-id &optional project)
  "Open a guided recorder for MEETING-ID in PROJECT.
The meeting and project stay fixed even when the user navigates elsewhere."
  (interactive (list (perinf-meeting--select-meeting) perinf-current-project))
  (unless (eq system-type 'darwin)
    (user-error "Åbning af optageapp kræver macOS"))
  (let* ((project (file-name-as-directory
                   (expand-file-name (or project perinf-current-project
                                         (user-error "Åbn først et projekt")))))
         (meeting (perinf-storage--meeting-by-id meeting-id project))
         (directory (perinf-recording--directory meeting-id project)))
    (when (alist-get 'AUDIO_ID (perinf-object-properties meeting))
      (user-error "Mødet har allerede en lydoptagelse"))
    (let ((application
           (string-trim
            (read-string "Optageapp (navn eller sti): "
                         nil nil perinf-recording-application))))
      (when (string-empty-p application)
        (user-error "Angiv en optageapp"))
      (make-directory directory t)
      (pop-to-buffer
       (get-buffer-create
        (format "*PerInf lyd: %s*" (secure-hash 'sha256 (concat project meeting-id)))))
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Lydoptagelse til mødet\n" 'face 'bold)
                (perinf-object-title meeting) "\n"
                (or (alist-get 'START_AT (perinf-object-properties meeting)) "")
                "\n\nOptageapp: " application
                "\n\n1. Start og afslut selv optagelsen i appen.\n"
                "2. I Memoer: træk den færdige optagelse til den åbne Finder-mappe.\n"
                "   I andre apps: gem eller eksporter lydfilen til mappen.\n"
                "3. Vend tilbage hertil, klik på Tilknyt lydfil, og vælg filen.\n\n"
                "Mappen til dette møde:\n" directory "\n\n")
        (perinf-recording--button "Åbn optageapp" #'perinf-recording--launch application)
        (perinf-recording--button "Åbn mappen til lydfilen" #'perinf-recording--open directory)
        (perinf-recording--button "Tilknyt lydfil til dette møde"
                                  #'perinf-recording--choose-file meeting-id project)
        (insert "PerInf bevarer originalen og kontrollerer den gemte kopi.\n"
                "Der slettes ikke automatisk optagelser.\n")
        (goto-char (point-min)))
      (perinf-recording--open directory)
      (perinf-recording--launch application))))

(provide 'perinf-recording)
;;; perinf-recording.el ends here
