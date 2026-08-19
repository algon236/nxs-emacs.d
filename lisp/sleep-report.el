;;; sleep-report.el --- Søvnrapport fra Org-tabel -*- lexical-binding: t; coding: utf-8; -*-

(require 'org)
(require 'org-table)
(require 'cl-lib)
(require 'calendar)
(require 'subr-x)

(defconst emacs-nxs-sleep-report-version 16
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

(defun emacs-nxs-sleep-report--find-named-table (name)
  "Returnér rækkerne i Org-tabellen med navnet NAME, inklusive tabelhovedet."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward
             (format "^[ \t]*#\\+NAME:[ \t]*%s[ \t]*$"
                     (regexp-quote name))
             nil t)
      (user-error "Tabellen '%s' blev ikke fundet" name))
    (forward-line 1)
    (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
      (forward-line 1))
    (unless (looking-at-p "^[ \t]*|")
      (user-error "Der står ingen Org-tabel efter navnet '%s'" name))
    (mapcar #'emacs-nxs-sleep-report--trim-row
            (cl-remove-if (lambda (row) (eq row 'hline))
                          (org-table-to-lisp)))))

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

(defun emacs-nxs-sleep-report--danish-date (value)
  "Formatér Org-datoen VALUE som dag, dansk månedsnavn og år."
  (if (string-match
       "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
       (or value ""))
      (let* ((year (string-to-number (match-string 1 value)))
             (month (string-to-number (match-string 2 value)))
             (day (string-to-number (match-string 3 value)))
             (month-name
              (nth (1- month)
                   '("januar" "februar" "marts" "april" "maj" "juni"
                     "juli" "august" "september" "oktober" "november"
                     "december"))))
        (format "%d. %s %d" day month-name year))
    (if (emacs-nxs-sleep-report--blank-p value) "" value)))

(defun emacs-nxs-sleep-report--date-iso (value)
  "Returnér datoen i VALUE som YYYY-MM-DD, eller nil."
  (when (string-match
         "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
         (or value ""))
    (match-string 0 value)))

(defun emacs-nxs-sleep-report--daily-note-id (date)
  "Returnér ID for org-roam-dagsnoten på DATE.
Returnér symbolet `file' hvis noten findes uden ID, og ellers nil.
DATE skal have formatet YYYY-MM-DD."
  (when date
    (let* ((roam-directory
            (if (boundp 'org-roam-directory)
                org-roam-directory
              (expand-file-name "~/org/roam/")))
           (dailies-directory
            (if (boundp 'org-roam-dailies-directory)
                org-roam-dailies-directory
              "dagligt/"))
           (file (expand-file-name
                  (concat date ".org")
                  (expand-file-name dailies-directory roam-directory))))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (if (re-search-forward
               "^[ \t]*:ID:[ \t]+\\([^ \t\r\n]+\\)[ \t]*$" nil t)
              (match-string-no-properties 1)
            'file))))))

(defun emacs-nxs-sleep-report--medicine-report-rows (rows)
  "Formatér datokolonne 2 og 6 i medicintabellens ROWS til rapporten."
  (cons
   '("Præparat" "købt" "pr/dag" "mod" "antal" "slut")
   (mapcar
    (lambda (row)
      (list (nth 0 row)
            (emacs-nxs-sleep-report--danish-date (nth 1 row))
            (nth 2 row)
            (nth 3 row)
            (nth 4 row)
            (emacs-nxs-sleep-report--danish-date (nth 5 row))))
    (cl-remove-if
     (lambda (row) (emacs-nxs-sleep-report--blank-p (nth 0 row)))
     (cdr rows)))))

(defun emacs-nxs-sleep-report--blood-pressure-report-rows (rows)
  "Formatér blodtryksdata og journal-ID'er i ROWS til rapporten.
Kildens kolonner er dato, hjælpefelt, vægt og derefter sys/dia for
morgen, middag og aften."
  (mapcar
   (lambda (row)
     (let ((date (emacs-nxs-sleep-report--date-iso (car row))))
       (list (emacs-nxs-sleep-report--danish-date (car row))
             (nth 3 row) (nth 4 row)
             (nth 5 row) (nth 6 row)
             (nth 7 row) (nth 8 row)
             (emacs-nxs-sleep-report--daily-note-id date))))
   (cl-remove-if
    (lambda (row)
      (or (emacs-nxs-sleep-report--blank-p (car row))
          (cl-every #'emacs-nxs-sleep-report--blank-p (cdr row))))
    (cdr rows))))

(defun emacs-nxs-sleep-report--blood-pressure-table (rows)
  "Lav LaTeX-rækker med todelt hoved og eventuelle id:-links fra ROWS."
  (concat
   "\\SetCell[r=2]{c} Dato & \\SetCell[c=2]{c} Morgen & & "
   "\\SetCell[c=2]{c} Middag & & \\SetCell[c=2]{c} Aften & & "
   "\\SetCell[r=2]{c} Journal \\\\\n"
   " & sys & dia & sys & dia & sys & dia & \\\\\n"
   (mapconcat
    (lambda (row)
      (let ((values (butlast row))
            (journal-id (car (last row))))
        (concat
         (mapconcat #'emacs-nxs-sleep-report--latex-escape values " & ")
         " & "
         (if (stringp journal-id)
             (format "\\href{id:%s}{Journal}"
                     (emacs-nxs-sleep-report--latex-escape journal-id))
           (if journal-id "Journal" ""))
         " \\\\")))
    (emacs-nxs-sleep-report--blood-pressure-report-rows rows)
    "\n")
   "\n"))

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

(defun emacs-nxs-sleep-report--blood-pressure-plot (rows)
  "Lav sys- og dia-kurver for morgen, middag og aften fra ROWS.
Manglende eller ugyldige målinger udelades enkeltvis.  Sys og dia for
samme tidspunkt bruger samme farve og skelnes med linje og markør.
Hver serie med mindst to målinger får en lineær tendenslinje fra den
første til den sidste dag i målingernes måned.  Dia under 60 er en
operatørfejl; sys over 160 udelades fra plottet."
  (let (plots)
    (dolist (series '((3 sys "morgen-sys" "Morgen sys" "morgen" "blue" "solid" "*")
                      (4 dia "morgen-dia" "Morgen dia" "morgen" "blue" "dashed" "square*")
                      (5 sys "middag-sys" "Middag sys" "middag" "red" "solid" "*")
                      (6 dia "middag-dia" "Middag dia" "middag" "red" "dashed" "square*")
                      (7 sys "aften-sys" "Aften sys" "aften" "green!60!black" "solid" "*")
                      (8 dia "aften-dia" "Aften dia" "aften" "green!60!black" "dashed" "square*")))
      (let* ((column (nth 0 series))
             (kind (nth 1 series))
             (header (nth 2 series))
             (legend (nth 3 series))
             (time-of-day (nth 4 series))
             (color (nth 5 series))
             (line-style (nth 6 series))
             (mark (nth 7 series))
             (valid
              (cl-loop for row in (cdr rows)
                       for date = (emacs-nxs-sleep-report--date-iso (car row))
                       for value = (emacs-nxs-sleep-report--number (nth column row))
                       when (and date value (eq kind 'dia) (< value 60))
                       do (user-error
                           "Blodtryk %s: dia for %s er %.1f; værdien må ikke være under 60"
                           date time-of-day value)
                       when (and date value
                                 (or (eq kind 'dia) (<= value 160)))
                       collect (cons (string-to-number (substring date 8 10))
                                     value)))
             (first-date
              (cl-loop for row in (cdr rows)
                       for date = (emacs-nxs-sleep-report--date-iso (car row))
                       when date return date))
             (month-end
              (when first-date
                (calendar-last-day-of-month
                 (string-to-number (substring first-date 5 7))
                 (string-to-number (substring first-date 0 4)))))
             (regression
              (when (> (length valid) 1)
                (let* ((count (float (length valid)))
                       (sum-x (cl-loop for entry in valid sum (car entry)))
                       (sum-y (cl-loop for entry in valid sum (cdr entry)))
                       (sum-xx (cl-loop for entry in valid
                                        sum (* (car entry) (car entry))))
                       (sum-xy (cl-loop for entry in valid
                                        sum (* (car entry) (cdr entry))))
                       (denominator (- (* count sum-xx) (* sum-x sum-x))))
                  (unless (zerop denominator)
                    (let* ((slope (/ (- (* count sum-xy) (* sum-x sum-y))
                                     denominator))
                           (intercept (/ (- sum-y (* slope sum-x)) count)))
                      (list (+ intercept slope)
                            (+ intercept (* slope month-end)))))))))
        (when valid
          (push
           (concat
            (format "\\addplot+[color=%s, %s, mark=%s, mark options={fill=%s!40}] table[x=dato,y=%s] {\n"
                    color line-style mark color header)
            (format "  dato  %s\n%s\n};\n"
                    header
                    (mapconcat
                     (lambda (entry)
                       (format "  %d  %.4f" (car entry) (cdr entry)))
                     valid "\n"))
            (format "\\addlegendentry{%s}\n" legend)
            (when regression
              (concat
               (format "\\addplot+[color=%s, densely dotted, very thick, no marks, forget plot] coordinates {"
                       color)
               (format "(1,%.4f) (%d,%.4f)};\n"
                       (nth 0 regression) month-end (nth 1 regression)))))
           plots))))
    (unless plots
      (user-error "Ingen gyldige blodtryksmålinger til plottet"))
    (mapconcat #'identity (nreverse plots) "\n")))

(defun emacs-nxs-sleep-report--generate
    (directory table-name rows medicine-rows blood-pressure-rows)
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
     (expand-file-name "blood-pressure-table.tex" out)
     (emacs-nxs-sleep-report--blood-pressure-table blood-pressure-rows))

    (emacs-nxs-sleep-report--write
     (expand-file-name "medicine-table.tex" out)
     (concat
      (mapconcat
       (lambda (row)
         (concat
          (mapconcat #'emacs-nxs-sleep-report--latex-escape
                     row
                     " & ")
          " \\\\"))
       (emacs-nxs-sleep-report--medicine-report-rows medicine-rows) "\n")
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

    (emacs-nxs-sleep-report--write
     (expand-file-name "plot-blood-pressure.tex" out)
     (emacs-nxs-sleep-report--blood-pressure-plot blood-pressure-rows))

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
               (medicine-rows
                (emacs-nxs-sleep-report--find-named-table
                 "Status-Medicin"))
               (blood-pressure-rows
                (emacs-nxs-sleep-report--find-named-table
                 "Blodtryk"))
               (directory (file-name-directory buffer-file-name)))
    (emacs-nxs-sleep-report--generate
     directory name rows medicine-rows blood-pressure-rows)
    (emacs-nxs-sleep-report--compile directory)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<f8>")
              #'emacs-nxs-sleep-report-update))

(provide 'sleep-report)
;;; sleep-report.el ends here
