;;; sleep-report.el --- Søvnrapport fra Org-tabel -*- lexical-binding: t; coding: utf-8; -*-

(require 'org)
(require 'org-table)
(require 'cl-lib)
(require 'subr-x)

(defconst emacs-nxs-sleep-report-version 10
  "Projektets heltalsversion.")

(defgroup emacs-nxs-sleep-report nil
  "Generér en LaTeX-søvnrapport fra en navngivet Org-tabel."
  :group 'org)

(defcustom emacs-nxs-sleep-report-tex-file "sleep-report.tex"
  "LaTeX-hovedfil relativt til Org-filen."
  :type 'string
  :group 'emacs-nxs-sleep-report)

(defcustom emacs-nxs-sleep-report-output-directory "generated"
  "Mappe til automatisk genererede LaTeX-fragmenter."
  :type 'string
  :group 'emacs-nxs-sleep-report)

(defcustom emacs-nxs-sleep-report-open-pdf t
  "Åbn PDF-filen efter vellykket kompilering."
  :type 'boolean
  :group 'emacs-nxs-sleep-report)

(defconst emacs-nxs-sleep-report--months
  '(("januar" . 1) ("februar" . 2) ("marts" . 3) ("april" . 4)
    ("maj" . 5) ("juni" . 6) ("juli" . 7) ("august" . 8)
    ("september" . 9) ("oktober" . 10) ("november" . 11)
    ("december" . 12)))

(defun emacs-nxs-sleep-report--trim-row (row)
  (mapcar (lambda (cell) (string-trim (format "%s" cell))) row))

(defun emacs-nxs-sleep-report--find-table ()
  "Returnér (NAVN OVERSKRIFT RÆKKER) for den første passende Org-tabel."
  (save-excursion
    (goto-char (point-min))
    (let (resultat)
      (while (and (not resultat)
                  (re-search-forward "^[ \t]*#\\+NAME:[ \t]*\\(.+\\)[ \t]*$" nil t))
        (let ((name (string-trim (match-string-no-properties 1))))
          (forward-line 1)
          (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
            (forward-line 1))
          (when (looking-at-p "^[ \t]*|")
            (let* ((table (org-table-to-lisp))
                   (rows (cl-remove-if (lambda (row) (eq row 'hline)) table))
                   (header (emacs-nxs-sleep-report--trim-row (car rows)))
                   (normalized (mapcar #'downcase header)))
              (when (and (member "dato" normalized)
                         (cl-some (lambda (s) (string-match-p "puls" s)) normalized)
                         (cl-some (lambda (s) (string-match-p "soevn\\|søvn" s)) normalized))
                (setq resultat
                      (list name header
                            (mapcar #'emacs-nxs-sleep-report--trim-row
                                    (cdr rows)))))))))
      (or resultat
          (user-error "Ingen navngivet søvntabel fundet i %s" (buffer-name))))))

(defun emacs-nxs-sleep-report--period (name)
  "Udled perioden fra tabelnavnet NAME, for eksempel juli-2026."
  (let ((case-fold-search t) month year)
    (dolist (entry emacs-nxs-sleep-report--months)
      (when (string-match-p (regexp-quote (car entry)) name)
        (setq month entry)))
    (when (string-match "\\(20[0-9][0-9]\\)" name)
      (setq year (string-to-number (match-string 1 name))))
    (unless (and month year)
      (user-error "Tabelnavnet '%s' skal indeholde måned og år" name))
    (format "%s %d" (car month) year)))

(defun emacs-nxs-sleep-report--blank-p (value)
  (let ((s (downcase (string-trim (or value "")))))
    (or (string-empty-p s)
        (member s '("nan" "na" "n/a" "nil" "-")))))

(defun emacs-nxs-sleep-report--number (value &optional minimum maximum)
  "Returnér VALUE som tal, eller nil ved ugyldige data.
Komma accepteres som decimaltegn. MINIMUM og MAXIMUM er valgfrie grænser."
  (unless (emacs-nxs-sleep-report--blank-p value)
    (let ((s (replace-regexp-in-string "," "." (string-trim value))))
      (when (string-match-p "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" s)
        (let ((n (string-to-number s)))
          (when (and (or (null minimum) (>= n minimum))
                     (or (null maximum) (<= n maximum)))
            n))))))

(defun emacs-nxs-sleep-report--time-number (value)
  "Konvertér H:MM til decimale timer, eller returnér nil.
Timer skal være 0-24, og minutter 0-59."
  (unless (emacs-nxs-sleep-report--blank-p value)
    (when (string-match "\\`\\([0-9]+\\):\\([0-9][0-9]\\)\\'"
                        (string-trim value))
      (let ((hours (string-to-number (match-string 1 value)))
            (minutes (string-to-number (match-string 2 value))))
        (when (and (<= 0 hours 24) (<= 0 minutes 59)
                   (or (< hours 24) (= minutes 0)))
          (+ hours (/ minutes 60.0)))))))

(defun emacs-nxs-sleep-report--time-to-hours (value)
  "Konvertér H:MM til tekst med decimale timer, eller tom tekst."
  (let ((n (emacs-nxs-sleep-report--time-number value)))
    (if n (format "%.2f" n) "")))

(defun emacs-nxs-sleep-report--latex-escape (text)
  (let ((s (or text "")))
    (dolist (pair '(("\\" . "\\textbackslash{}")
                    ("&" . "\\&") ("%" . "\\%") ("$" . "\\$")
                    ("#" . "\\#") ("_" . "\\_") ("{" . "\\{")
                    ("}" . "\\}") ("~" . "\\textasciitilde{}")
                    ("^" . "\\textasciicircum{}")))
      (setq s (replace-regexp-in-string
               (regexp-quote (car pair)) (cdr pair) s t t)))
    s))

(defun emacs-nxs-sleep-report--write (file content)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert content)))

(defun emacs-nxs-sleep-report--day-rows (rows)
  (cl-remove-if-not
   (lambda (row)
     (and (string-match-p "\\`[0-9]+\\'" (or (nth 0 row) ""))
          (cl-some (lambda (cell)
                     (not (emacs-nxs-sleep-report--blank-p cell)))
                   (cdr row))))
   rows))

(defun emacs-nxs-sleep-report--line-plot
    (rows column header color minimum maximum &optional regression)
  "Lav et linjeplot og udelad ugyldige eller urimelige værdier."
  (let* ((valid
          (cl-loop for row in rows
                   for value = (emacs-nxs-sleep-report--number
                                (funcall column row) minimum maximum)
                   when value collect
                   (cons (string-to-number (nth 0 row)) value)))
         (data
          (mapconcat
           (lambda (entry) (format "  %d  %.4f" (car entry) (cdr entry)))
           valid "\n")))
    (unless valid
      (user-error "Ingen gyldige værdier til plottet %s" header))
    (concat
     (format "\\addplot+[color=%s, mark=*, mark options={fill=%s!40}] table[x=dato,y=%s] {\n"
             color color header)
     (format "  dato  %s\n%s\n};\n" header data)
     (when (and regression (> (length valid) 1))
       (concat
        (format "\\addplot+[color=%s, dashed, no marks] table[y={create col/linear regression={y=%s}}] {\n"
                color header)
        (format "  dato  %s\n%s\n};\n" header data))))))

(defun emacs-nxs-sleep-report--bar-plot (rows column header fill draw)
  "Lav én udfyldt søjleserie til et stablet plot."
  (let ((data
         (mapconcat
          (lambda (row)
            (format "  %d  %s"
                    (string-to-number (nth 0 row))
                    (funcall column row)))
          rows "\n")))
    (concat
     (format "\\addplot[fill=%s, draw=%s, no markers] table[x=dato,y=%s] {\n"
             fill draw header)
     (format "  dato  %s\n%s\n};\n" header data))))

(defun emacs-nxs-sleep-report--generate (directory table-name rows)
  (let* ((out (expand-file-name emacs-nxs-sleep-report-output-directory directory))
         (period (emacs-nxs-sleep-report--period table-name))
         (usable (emacs-nxs-sleep-report--day-rows rows)))
    (unless usable
      (user-error "Søvntabellen indeholder ingen målinger"))

    (emacs-nxs-sleep-report--write
     (expand-file-name "report-meta.tex" out)
     (format "%% Automatisk genereret.\n\\newcommand{\\SleepPeriod}{%s}\n\\newcommand{\\SleepReportVersion}{%d}\n"
             (emacs-nxs-sleep-report--latex-escape period)
             emacs-nxs-sleep-report-version))

    (emacs-nxs-sleep-report--write
     (expand-file-name "measurements-table.tex" out)
     (concat
      (mapconcat
       (lambda (row)
         (concat
          (mapconcat #'emacs-nxs-sleep-report--latex-escape
                     (cl-subseq (append row (make-list 9 "")) 0 9)
                     " & ")
          " \\\\"))
       usable "\n")
      "\n"))

    (emacs-nxs-sleep-report--write
     (expand-file-name "plot-pulse.tex" out)
     (concat
      (emacs-nxs-sleep-report--line-plot usable (lambda (r) (nth 1 r))
                                          "pulsmin" "blue" 20 250)
      "\n"
      (emacs-nxs-sleep-report--line-plot usable (lambda (r) (nth 2 r))
                                          "pulsavg" "red" 20 250 t)))

    (emacs-nxs-sleep-report--write
     (expand-file-name "plot-oxygen.tex" out)
     (concat
      (emacs-nxs-sleep-report--line-plot usable (lambda (r) (nth 3 r))
                                          "o2min" "blue" 50 100)
      "\n"
      (emacs-nxs-sleep-report--line-plot usable (lambda (r) (nth 4 r))
                                          "o2avg" "red" 50 100 t)))

    (emacs-nxs-sleep-report--write
     (expand-file-name "plot-temperature.tex" out)
     (emacs-nxs-sleep-report--line-plot usable (lambda (r) (nth 5 r))
                                         "temp" "blue" 25 45 t))

    ;; Kun rækker med gyldige tider medtages i søvnplottet.
    ;; Nederste del: faktisk søvn.
    ;; Øverste del: resterende tid i sengen = i-seng minus søvn.
    (let ((sleep-rows
           (cl-remove-if-not
            (lambda (r)
              (let ((bed (emacs-nxs-sleep-report--time-number (nth 6 r)))
                    (sleep (emacs-nxs-sleep-report--time-number (nth 7 r))))
                (and bed sleep (>= bed sleep))))
            usable)))
      (unless sleep-rows
        (user-error "Ingen gyldige rækker til søvnplottet"))
      (emacs-nxs-sleep-report--write
       (expand-file-name "plot-sleep.tex" out)
       (concat
        (emacs-nxs-sleep-report--bar-plot
         sleep-rows
         (lambda (r)
           (format "%.2f" (emacs-nxs-sleep-report--time-number (nth 7 r))))
         "soevn" "blue!65" "blue!80!black")
        "\n"
        (emacs-nxs-sleep-report--bar-plot
         sleep-rows
         (lambda (r)
           (let ((bed (emacs-nxs-sleep-report--time-number (nth 6 r)))
                 (sleep (emacs-nxs-sleep-report--time-number (nth 7 r))))
             (format "%.2f" (- bed sleep))))
         "rest" "red!60" "red!80!black"))))
    out))

(defun emacs-nxs-sleep-report--compile (directory)
  (let* ((tex (expand-file-name emacs-nxs-sleep-report-tex-file directory))
         (pdf (concat (file-name-sans-extension tex) ".pdf"))
         (default-directory directory)
         (buffer (get-buffer-create "*sleep-report-lualatex*")))
    (with-current-buffer buffer (erase-buffer))
    (dotimes (_ 2)
      (unless (zerop
               (call-process "lualatex" nil buffer t
                             "-interaction=nonstopmode"
                             "-halt-on-error"
                             "-file-line-error"
                             (file-name-nondirectory tex)))
        (display-buffer buffer)
        (user-error "LuaLaTeX gav en fejl; se *sleep-report-lualatex*")))
    (when (and emacs-nxs-sleep-report-open-pdf (file-exists-p pdf))
      (find-file-other-window pdf))
    (message "Søvnrapport version %d er opdateret"
             emacs-nxs-sleep-report-version)))

;;;###autoload
(defun emacs-nxs-sleep-report-update ()
  "Generér fragmenter og kompilér rapporten fra den aktuelle Org-fil."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Kommandoen skal køres fra Org-filen"))
  (unless buffer-file-name
    (user-error "Gem Org-filen først"))
  (save-buffer)
  (pcase-let* ((`(,name ,_header ,rows)
                (emacs-nxs-sleep-report--find-table))
               (directory (file-name-directory buffer-file-name)))
    (emacs-nxs-sleep-report--generate directory name rows)
    (emacs-nxs-sleep-report--compile directory)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<f8>")
              #'emacs-nxs-sleep-report-update))

(provide 'sleep-report)
;;; sleep-report.el ends here
