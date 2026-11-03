;;; perinf-statistics.el --- Immutable local statistical reports -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Niels Søndergaard
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Danish Org reports and versioned JSON snapshots, isolated per project.
;; Reading an archived report never refreshes or overwrites it.  Historical
;; activity is reconstructed from surviving records, never historical stock.
;;; Code:
(require 'org)
(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'button)
(require 'perinf-project)
(require 'perinf-meeting)
(declare-function perinf-core--call-interactively-from-button "perinf-core" (command))

(defvar-local perinf-statistics--project nil)
(defvar-local perinf-statistics--year nil)
(defvar-local perinf-statistics--report-file nil)
(defcustom perinf-statistics-export-template
  (expand-file-name "var/templates/template.org" user-emacs-directory)
  "Org header template used for exported report copies."
  :type 'file :group 'perinf)
(defconst perinf-statistics--metrics
  '((tasks . "Opgaver i alt") (unfinished . "Ikke afsluttede opgaver")
    (completed . "Afsluttede opgaver") (meetings . "Møder i alt")
    (held . "Afholdte møder") (planned . "Planlagte møder")
    (cancelled . "Annullerede møder") (people . "Personer i alt")
    (active-people . "Aktive personer") (groups . "Persongrupper")
    (memos . "Husk i alt") (open-memos . "Ikke færdige Husk")
    (documents . "Dokumentposter") (audio . "Lydoptagelser")
    (minutes . "Referater") (transcripts . "Transskriptioner")
    (decisions . "Beslutninger") (contexts . "Kontekster")))

(defun perinf-statistics--date (value)
  "Extract an ISO calendar date from VALUE, or nil."
  (when (and value (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" value))
    (match-string 0 value)))

(defun perinf-statistics--records (project)
  "Read PROJECT records without modifying files or visiting buffers.
Refuse unsaved source buffers rather than silently measuring older data."
  (let (rows)
    (dolist (file '("data/tasks.org" "data/people.org"))
      (unless (file-readable-p (expand-file-name file project))
        (user-error "Datagrundlaget mangler: %s" file)))
    (dolist (sub '("data" "media" "archive"))
      (let ((dir (expand-file-name sub project)))
        (when (file-directory-p dir)
          (dolist (file (seq-remove
                        (lambda (f) (string-prefix-p ".#" (file-name-nondirectory f)))
                        (directory-files-recursively dir "\\.org\\'")))
            (let ((buffer (find-buffer-visiting file)))
              (when (and buffer (buffer-modified-p buffer))
                (user-error "Gem først ændringerne i %s" file)))
            (with-temp-buffer
              (insert-file-contents file)
              (delay-mode-hooks (org-mode))
              (org-map-entries
               (lambda ()
                 (let* ((props (org-entry-properties nil 'all))
                        (type (cdr (assoc "PERINF_TYPE" props))))
                   (when (or type (and (equal (file-name-nondirectory file) "memos.org")
                                       (= (org-outline-level) 1)
                                       (equal (cdr (assoc "CATEGORY" props)) "Husk")))
                     (push (cons (cons "_TYPE" (or type "memo")) props) rows))))))))))
    (nreverse rows)))

(defun perinf-statistics--value (row key)
  "Return KEY in ROW."
  (cdr (assoc key row)))

(defun perinf-statistics--collect (project year &optional now)
  "Collect PROJECT statistics for YEAR at NOW, without saving."
  (let* ((now (or now (current-time)))
         (today (format-time-string "%Y-%m-%d" now))
         (current-year (string-to-number (substring today 0 4)))
         (rows (perinf-statistics--records project))
         (metadata (perinf-project-read-metadata project))
         (counts (mapcar (lambda (m) (cons (car m) 0)) perinf-statistics--metrics))
         (months (make-vector 12 nil))
         (ids (make-hash-table :test #'equal))
         (duplicates 0) (missing-id 0) (broken 0) (missing-close 0)
         (missing-deadline 0) (zero-meetings 0) (seconds 0) (running 0)
         (timed 0) (missing-dates 0)
         (meeting-seconds 0) (meeting-extra 0) (meeting-running 0))
    (unless (and (integerp year) (<= 1900 year current-year))
      (user-error "Vælg et år mellem 1900 og %s" current-year))
    (dotimes (i 12)
      (aset months i (copy-tree `((month . ,(format "%04d-%02d" year (1+ i)))
                      (created . 0) (closed . 0) (held . 0) (cancelled . 0)
                      (planned . 0) (people . 0) (memos . 0)))))
    (cl-labels
        ((inc (key) (cl-incf (alist-get key counts)))
         (event (key value)
           (let ((date (perinf-statistics--date value)))
             (if (not date) (cl-incf missing-dates)
               (when (and (= (string-to-number date) year)
                          (or (memq key '(planned cancelled))
                              (not (string> date today))))
                 (let ((month (string-to-number (substring date 5 7))))
                   (when (<= 1 month 12)
                     (cl-incf (alist-get key (aref months (1- month)))))))))))
      (dolist (row rows)
        (let* ((type (perinf-statistics--value row "_TYPE"))
               (status (perinf-statistics--value row "PERINF_STATUS"))
               (created (perinf-statistics--value row "CREATED_AT"))
               (id (perinf-statistics--value row "ID")))
          (if (not id) (cl-incf missing-id)
            (when (gethash id ids) (cl-incf duplicates))
            (puthash id type ids))
          (pcase type
            ("task"
             (inc 'tasks) (event 'created created)
             (when (equal status "completed") (inc 'completed))
             (unless (member status '("completed" "cancelled"))
               (inc 'unfinished)
               (unless (perinf-statistics--value row "DEADLINE")
                 (cl-incf missing-deadline)))
             (let ((closed (perinf-statistics--value row "CLOSED")))
               (if closed (event 'closed closed)
                 (when (equal status "completed") (cl-incf missing-close))))
             (when (perinf-statistics--value row "TASK_TIMER_STARTED_AT")
               (cl-incf running))
             (let ((value (perinf-statistics--value row "TASK_WORK_SECONDS")))
               (when value
                 (unless (string-match-p "\\`[0-9]+\\'" value)
                   (user-error "Ugyldig timertid på opgave %s" id))
                 (cl-incf timed) (cl-incf seconds (string-to-number value)))))
            ("meeting"
             (inc 'meetings)
             (when (member status '("held" "planned" "cancelled"))
               (inc (intern status))
               (event (intern status) (perinf-statistics--value row "START_AT")))
             (when (and (equal status "held")
                        (equal (perinf-statistics--value row "ACTUAL_START_AT")
                               (perinf-statistics--value row "ACTUAL_FINISH_AT")))
               (cl-incf zero-meetings)))
            ("person" (inc 'people) (event 'people created)
             (when (equal status "active") (inc 'active-people)))
            ("person-group" (inc 'groups))
            ("memo" (inc 'memos)
             (event 'memos (perinf-statistics--value row "CREATED"))
             (unless (equal (perinf-statistics--value row "TODO") "DONE")
               (inc 'open-memos)))
            ("document" (inc 'documents))
            ("audio-recording" (inc 'audio))
            ("minutes" (inc 'minutes))
            ("transcript" (inc 'transcripts))
            ("decision" (inc 'decisions))
            ("context" (inc 'contexts)))))
      (dolist (row rows)
        (dolist (key '("PERSON_ID" "ASSIGNEE_ID"))
          (let ((id (perinf-statistics--value row key)))
            (when (and id (not (equal (gethash id ids) "person")))
              (cl-incf broken)))))
      (dolist (meeting (perinf-storage-list 'meeting project))
        (when (eq (perinf-object-status meeting) 'held)
          (cl-incf meeting-seconds (perinf-meeting-duration-seconds meeting)))
        (cl-incf meeting-extra
                 (string-to-number (or (alist-get 'MEETING_EXTRA_SECONDS
                                                  (perinf-object-properties meeting)) "0")))
        (when (alist-get 'MEETING_TIMER_STARTED_AT (perinf-object-properties meeting))
          (cl-incf meeting-running)))
      `((version . 1) (project . ,(alist-get 'PROJECT_ID metadata))
        (title . ,(alist-get 'PROJECT_TITLE metadata)) (year . ,year)
        (captured . ,(format-time-string "%Y-%m-%dT%H:%M:%S.%6N%z" now))
        (stock . ,(if (= year current-year) counts nil))
        (months . ,months) (seconds . ,seconds) (timed . ,timed)
        (running . ,running)
        (meeting-seconds . ,meeting-seconds)
        (meeting-extra . ,meeting-extra)
        (meeting-running . ,meeting-running)
        (quality . ((duplicates . ,duplicates) (missing-id . ,missing-id)
                    (broken . ,broken) (missing-close . ,missing-close)
                    (missing-deadline . ,missing-deadline)
                    (zero-meetings . ,zero-meetings)
                    (missing-dates . ,missing-dates)))))))

(defun perinf-statistics--history (project)
  "Return validated report snapshots for PROJECT, sorted oldest first."
  (let ((dir (expand-file-name "statistics" project))
        (id (alist-get 'PROJECT_ID (perinf-project-read-metadata project))) rows)
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively dir "snapshot\\.json\\'"))
        (let ((data (with-temp-buffer
                      (insert-file-contents file)
                      (json-parse-buffer :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil))))
          (unless (and (equal (alist-get 'version data) 1)
                       (equal (alist-get 'project data) id)
                       (integerp (alist-get 'year data))
                       (stringp (alist-get 'captured data))
                       (file-readable-p (expand-file-name "report.org" (file-name-directory file))))
            (user-error "Rapportarkivet indeholder en ugyldig rapport: %s" file))
          (push (cons (cons 'file file) data) rows))))
    (sort rows (lambda (a b)
                 (string< (alist-get 'captured a) (alist-get 'captured b))))))

(defun perinf-statistics--baselines (snapshot history)
  "Return last same-year and last pre-year stock snapshots from HISTORY."
  (let* ((year (alist-get 'year snapshot))
         (boundary (format "%04d-01-01" year))
         (eligible (seq-filter
                    (lambda (r) (not (string> (alist-get 'captured r)
                                              (alist-get 'captured snapshot)))) history)))
    (let ((start (car (last (seq-filter
                            (lambda (r) (and (alist-get 'stock r)
                                             (string< (alist-get 'captured r) boundary))) eligible)))))
      (list (or (car (last (seq-filter (lambda (r) (= (alist-get 'year r) year)) eligible)))
                start)
            start))))

(defun perinf-statistics--sum (snapshot key)
  "Sum monthly KEY in SNAPSHOT."
  (cl-loop for row across (vconcat (alist-get 'months snapshot))
           sum (or (alist-get key row) 0)))

(defun perinf-statistics--delta (value baseline key)
  "Format VALUE minus BASELINE stock KEY, or unavailable."
  (let ((old (alist-get key (alist-get 'stock baseline))))
    (if (numberp old) (format "%+d" (- value old)) "—")))

(defun perinf-statistics--render (snapshot history)
  "Return immutable Org report text for SNAPSHOT and HISTORY."
  (let* ((year (alist-get 'year snapshot))
         (baselines (perinf-statistics--baselines snapshot history))
         (previous (car baselines)) (start (cadr baselines))
         (quality (alist-get 'quality snapshot)))
    (with-temp-buffer
      (insert (format "#+title: PerInf statistik – %d\n#+startup: showall\n\n" year)
              (format "Projekt: %s\nRapport gemt: %s\n\n"
                      (alist-get 'title snapshot) (alist-get 'captured snapshot))
              "* Sammenligningsgrundlag\n"
              (format "Sidste sammenlignelige rapport (valgt år %d): %s\n" year
                      (or (alist-get 'captured previous) "Ingen tidligere rapport"))
              (format "Seneste måling før årets begyndelse: %s\n\n"
                      (or (alist-get 'captured start) "Mangler – ændring kan ikke beregnes"))
              "Datoerne ovenfor er de faktiske måledatoer. En ældre måling er ikke nødvendigvis status præcis 1. januar.\n\n")
      (when (alist-get 'stock snapshot)
        (insert "* Status og ændringer\n"
                "| Oplysning | Nu | Siden sidste rapport | Siden målingen før årsskiftet |\n|---+---+---+---|\n")
        (dolist (metric perinf-statistics--metrics)
          (let ((value (alist-get (car metric) (alist-get 'stock snapshot))))
            (insert (format "| %s | %d | %s | %s |\n" (cdr metric) value
                            (perinf-statistics--delta value previous (car metric))
                            (perinf-statistics--delta value start (car metric)))))))
      (unless (alist-get 'stock snapshot)
        (insert "* Historisk år\nAktiviteten nedenfor er genberegnet fra de poster, der findes ved denne kørsel. Nuværende antal vises ikke som historisk årsstatus.\n"))
      (insert "\n* Årets registrerede aktivitet\n"
              "Fra 1. januar i det valgte år. Opgaver efter oprettelse/afslutning; møder efter aftalt dato og registreret status. Planlagte møder kan ligge senere på året.\n\n"
              "| Aktivitet | I året | Ændring siden sidste rapport for året |\n|---+---+---|\n")
      (dolist (metric '((created . "Nye opgaver") (closed . "Afsluttede opgaver")
                        (held . "Afholdte møder") (cancelled . "Annullerede møder")
                        (planned . "Planlagte møder") (people . "Nye personer")
                        (memos . "Nye Husk")))
        (let ((value (perinf-statistics--sum snapshot (car metric))))
          (insert (format "| %s | %d | %s |\n" (cdr metric) value
                          (if previous
                              (format "%+d" (- value (if (= (alist-get 'year previous) year)
                                                          (perinf-statistics--sum previous (car metric)) 0)))
                            "—")))))
      (insert "\n* Måneder\nDen igangværende måned er ufuldstændig. Fremtidige måneder viser kun allerede registrerede aftaler; nul betyder ikke en færdig månedsopgørelse.\n\n| Måned | Nye opgaver | Afsluttede | Afholdte møder | Annullerede | Planlagte | Nye personer | Nye Husk |\n|---+---+---+---+---+---+---+---|\n")
      (mapc (lambda (row)
              (insert (format "| %s | %d | %d | %d | %d | %d | %d | %d |\n"
                              (alist-get 'month row) (alist-get 'created row)
                              (alist-get 'closed row) (alist-get 'held row)
                              (alist-get 'cancelled row) (alist-get 'planned row)
                              (alist-get 'people row) (alist-get 'memos row))))
            (alist-get 'months snapshot))
      (when (alist-get 'stock snapshot)
        (let ((seconds (alist-get 'seconds snapshot)))
          (insert (format "\n* Registreret tid\nSamlet gemt timertid: %d timer, %d minutter, %d sekunder på %d opgaver.\nIgangværende timere: %d (deres endnu ikke gemte tid er ikke med).\n"
                          (/ seconds 3600) (% (/ seconds 60) 60) (% seconds 60)
                          (alist-get 'timed snapshot) (alist-get 'running snapshot)))
          (when (assq 'meeting-seconds snapshot)
            (let ((duration (alist-get 'meeting-seconds snapshot))
                  (extra (alist-get 'meeting-extra snapshot)))
              (insert (format "\nAfholdte møders mødetid: %.2f timer.\nGemt ekstratid på alle møder: %.2f timer.\nMødetid og ekstratid i alt: %.2f timer.\nIgangværende ekstratimere: %d (ugemt tid er ikke med).\n"
                              (/ duration 3600.0) (/ extra 3600.0)
                              (/ (+ duration extra) 3600.0)
                              (alist-get 'meeting-running snapshot)))))
          (insert "Tiden kan ikke fordeles sikkert på måneder. Nulstillinger og slettede opgaver kan reducere summen. Der beregnes derfor ingen månedlig eller årlig arbejdstid ud fra tællerforskelle.\n")))
      (insert "\n* Datakontrol og fortolkning\n")
      (dolist (metric '((duplicates . "Gentagne id'er") (missing-id . "Manglende id'er")
                        (broken . "Personhenvisninger uden match")
                        (missing-close . "Afsluttede opgaver uden afslutningsdato")
                        (missing-deadline . "Ikke afsluttede opgaver uden deadline")
                        (zero-meetings . "Afholdte møder med ens/manglende start og slut")
                        (missing-dates . "Manglende datoer til aktivitetsfordeling")))
        (insert (format "- %s: %d\n" (cdr metric) (alist-get (car metric) quality))))
      (insert "\nHusk tælles særskilt; DONE uden afslutningsdato kan ikke henføres til en afslutningsmåned. Bilag tælles som registerposter.\n"
              "Ændringer viser nettoudviklingen i registreringerne, også rettelser, sletninger og efterregistrering. De er ikke i sig selv mål for arbejdsindsats. Tidligere rapporter ændres aldrig ved genberegning.\n")
      (buffer-string))))

(defun perinf-statistics--save (project year)
  "Atomically save a new report pair for PROJECT and YEAR; return its path."
  (let* ((history (perinf-statistics--history project))
         (snapshot (perinf-statistics--collect project year))
         (text (perinf-statistics--render snapshot history))
         (parent (expand-file-name (format "statistics/%d/" year) project))
         staging destination)
    (make-directory parent t)
    ;; Temporary directories live outside the archive so interrupted writes
    ;; can never appear as complete reports to the history reader.
    (setq staging (make-temp-file (expand-file-name ".perinf-statistics-" project) t))
    (setq destination (expand-file-name
                       (concat (format-time-string "%Y%m%dT%H%M%S-%6N-")
                               (file-name-nondirectory staging)) parent))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file (expand-file-name "snapshot.json" staging)
              (insert (json-encode snapshot) "\n"))
            (with-temp-file (expand-file-name "report.org" staging)
              (insert text)))
          (rename-file staging destination nil)
          (expand-file-name "report.org" destination))
      (when (file-directory-p staging) (delete-directory staging t)))))

(defun perinf-statistics-export (file)
  "Save the displayed report to FILE with the user's standard Org header."
  (interactive
   (list (read-file-name "Gem rapport som: " nil nil nil
                         (format "PerInf-statistik-%s.org" perinf-statistics--year))))
  (unless perinf-statistics--report-file
    (user-error "Åbn først en statistikrapport"))
  (unless (file-readable-p perinf-statistics-export-template)
    (user-error "Org-skabelonen kan ikke læses: %s" perinf-statistics-export-template))
  (setq file (expand-file-name file))
  (when (file-in-directory-p file (expand-file-name "statistics" perinf-statistics--project))
    (user-error "Vælg en placering uden for rapportarkivet"))
  (when (and (get-file-buffer file)
             (buffer-modified-p (get-file-buffer file)))
    (user-error "Filen har ugemte ændringer i Emacs: %s" file))
  (let* ((report (with-temp-buffer
                   (insert-file-contents perinf-statistics--report-file)
                   (buffer-string)))
         (title (if (string-match "^#\\+title: \\(.*\\)" report)
                    (match-string 1 report)
                  (format "PerInf statistik – %s" perinf-statistics--year)))
         (header (with-temp-buffer
                   (insert-file-contents perinf-statistics-export-template)
                   (buffer-string))))
    (dolist (replacement `(("{{title}}" . ,title)
                           ("{{description}}" . "Gemt statistikrapport fra PerInf")
                           ("{{keywords}}" . "PerInf, statistik")))
      (setq header (string-replace (car replacement) (cdr replacement) header)))
    ;; Only replace the original metadata; keep the archived report body intact.
    (setq report (replace-regexp-in-string
                  "\\`#\\+title:[^\n]*\n#\\+startup:[^\n]*\n\n" "" report))
    (with-temp-buffer
      (insert header "\n" report)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) file nil nil nil 'confirm))))
  (message "Rapport gemt i %s" file))

(defun perinf-statistics--show (project year file)
  "Show FILE with statistics controls for PROJECT and YEAR."
  (let ((buffer (get-buffer-create (format "*PerInf statistik: %s*" (file-truename project)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file)
        (org-mode)
        (setq-local perinf-statistics--project project
                    perinf-statistics--year year
                    perinf-statistics--report-file file)
        (goto-char (point-min))
        (dolist (entry '(("GEM RAPORT" . perinf-statistics-export)
                         ("Ny rapport" . perinf-statistics-new)
                         ("Skift år" . perinf-statistics-select-year)
                         ("Gamle rapporter" . perinf-statistics-history)))
          (insert-text-button (car entry) 'follow-link t
                              'help-echo "Klik med venstre museknap, eller tryk RET for at aktivere knappen"
                              'action (lambda (_button)
                                        (perinf-core--call-interactively-from-button (cdr entry))))
          (insert "    "))
        (insert "\n\n")
        (org-table-map-tables #'org-table-align)
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun perinf-statistics ()
  "Open latest current-year report, creating the first report if necessary."
  (interactive)
  (unless (and perinf-current-project (perinf-project-p perinf-current-project))
    (user-error "Åbn først et PerInf-projekt"))
  (perinf-statistics--open-year perinf-current-project
                               (string-to-number (format-time-string "%Y"))))

(defun perinf-statistics--open-year (project year)
  "Open newest saved report for PROJECT YEAR, or create its first report."
  (let ((latest (car (last (seq-filter
                           (lambda (r) (= (alist-get 'year r) year))
                           (perinf-statistics--history project))))))
    (perinf-statistics--show
     project year (if latest
                      (expand-file-name "report.org" (file-name-directory (alist-get 'file latest)))
                    (perinf-statistics--save project year)))))

(defun perinf-statistics-new ()
  "Save and display a new immutable report for the selected year."
  (interactive)
  (let ((project perinf-statistics--project) (year perinf-statistics--year))
    (perinf-statistics--show project year (perinf-statistics--save project year))
    (message "Ny statistik for %d beregnet og gemt kl. %s"
             year (format-time-string "%H:%M:%S"))))

(defun perinf-statistics-select-year ()
  "Select a year, including a year without saved reports."
  (interactive)
  (let* ((project perinf-statistics--project)
         (year (read-number "Statistik for år: " perinf-statistics--year)))
    (unless (and (integerp year) (<= 1900 year (string-to-number (format-time-string "%Y"))))
      (user-error "Ugyldigt år"))
    (perinf-statistics--open-year project year)))

(defun perinf-statistics-history ()
  "Select an old report from the selected year's immutable archive."
  (interactive)
  (let* ((project perinf-statistics--project) (year perinf-statistics--year)
         (reports (reverse (seq-filter (lambda (r) (= (alist-get 'year r) year))
                                        (perinf-statistics--history project))))
         (choices (cl-loop for r in reports for n from 1
                           collect (cons (format "%s (%d)" (alist-get 'captured r) n) r)))
         (chosen (cdr (assoc (completing-read "Rapport: " choices nil t) choices))))
    (when chosen
      (perinf-statistics--show project year
                               (expand-file-name "report.org" (file-name-directory (alist-get 'file chosen)))))))

(provide 'perinf-statistics)
;;; perinf-statistics.el ends here
