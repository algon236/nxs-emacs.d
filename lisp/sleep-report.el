;;; sleep-report.el --- Søvnrapport fra Org-tabel -*- lexical-binding: t; coding: utf-8; -*-

(require 'org)
(require 'org-table)
(require 'cl-lib)
(require 'calendar)
(require 'subr-x)

(defconst emacs-nxs-sleep-report-version 18
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

(defun emacs-nxs-sleep-report--table-header-and-data ()
  "Returnér tabelhoved og datarækker ved punkt.
Indledende skillelinjer ignoreres.  En skillelinje efter tabelhovedet
markerer begyndelsen på data, og den næste markerer slutningen.  Dermed
medtages en eventuel efterfølgende gennemsnitsrække ikke som data."
  (let ((table (org-table-to-lisp)))
    (while (eq (car table) 'hline)
      (setq table (cdr table)))
    (let ((header (car table)))
      (setq table (cdr table))
      (while (eq (car table) 'hline)
        (setq table (cdr table)))
      (list header
            (cl-loop for row in table
                     until (eq row 'hline)
                     collect row)))))

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
            (pcase-let* ((`(,raw-header ,raw-rows)
                          (emacs-nxs-sleep-report--table-header-and-data))
                         (header (emacs-nxs-sleep-report--trim-row raw-header))
                         (normalized (mapcar #'downcase header)))
              (when (and (member "dato" normalized)
                         (cl-some (lambda (s) (string-match-p "puls" s)) normalized)
                         (cl-some (lambda (s) (string-match-p "soevn\\|søvn" s)) normalized))
                (setq resultat
                      (list name header
                            (mapcar #'emacs-nxs-sleep-report--trim-row
                                    raw-rows))))))))
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
    (pcase-let ((`(,header ,rows)
                 (emacs-nxs-sleep-report--table-header-and-data)))
      (mapcar #'emacs-nxs-sleep-report--trim-row
              (cons header rows)))))

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
     (when (and regression
                (> (length valid) 1)
                (not (apply #'= (mapcar #'cdr valid))))
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

(defun emacs-nxs-sleep-report--bp-series (rows)
  "Returnér seks serier med alle, medtagne og udeladte målinger.
Grænserne er 7,5 procent for sys og 15 procent for dia, beregnet én
gang fra hver series oprindelige gennemsnit. Grænsepunkter medtages."
  (cl-loop for spec in '((3 sys "Morgen" "blue") (4 dia "Morgen" "blue")
                        (5 sys "Middag" "red") (6 dia "Middag" "red")
                        (7 sys "Aften" "green!60!black") (8 dia "Aften" "green!60!black"))
           collect
           (let* ((points (cl-loop for row in (cdr rows)
                                  for date = (emacs-nxs-sleep-report--date-iso (car row))
                                  for value = (emacs-nxs-sleep-report--number (nth (car spec) row))
                                  when (and date value)
                                  collect (cons (string-to-number (substring date 8 10)) value)))
                  (mean (when points (/ (apply #'+ (mapcar #'cdr points)) (float (length points)))))
                  (limit (if (eq (nth 1 spec) 'sys) 0.075 0.15))
                  kept omitted)
             (dolist (point points)
               (if (<= (abs (- (cdr point) mean)) (+ (* (abs mean) limit) 1e-9))
                   (push point kept)
                 (push point omitted)))
             (list :kind (nth 1 spec) :label (nth 2 spec) :color (nth 3 spec)
                   :all points :kept (nreverse kept) :omitted (nreverse omitted)))))

(defun emacs-nxs-sleep-report--bp-coordinates (points)
  (mapconcat (lambda (p) (format "(%d,%.6f)" (car p) (cdr p))) points " "))

(defun emacs-nxs-sleep-report--blood-pressure-plot (rows &optional kind)
  "Forbind alle numeriske målinger; beregn tendens uden markerede afvigelser.
KIND begrænser plottet til sys eller dia. Ingen faste blodtryksgrænser."
  (mapconcat
   (lambda (series)
     (let* ((points (plist-get series :all)) (kept (plist-get series :kept))
            (omitted (plist-get series :omitted)) (color (plist-get series :color))
            (n (length kept)) (sx (apply #'+ (mapcar #'car kept)))
            (sy (apply #'+ (mapcar #'cdr kept)))
            (sxx (cl-loop for (x . _y) in kept sum (* x x)))
            (sxy (cl-loop for (x . y) in kept sum (* x y)))
            (den (- (* n sxx) (* sx sx))))
       (when points
         (concat
          (format "\\addplot+[color=%s,solid,mark=*,mark size=1.5pt] coordinates {%s};\n\\addlegendentry{%s}\n"
                  color (emacs-nxs-sleep-report--bp-coordinates points) (plist-get series :label))
          (when omitted
            (format "\\addplot+[color=%s,only marks,mark=x,mark size=3pt,very thick,forget plot] coordinates {%s};\n"
                    color (emacs-nxs-sleep-report--bp-coordinates omitted)))
          (when (and (> n 1) (> den 0))
            (let* ((slope (/ (- (* n sxy) (* sx sy)) (float den)))
                   (intercept (/ (- sy (* slope sx)) n))
                   (first (apply #'min (mapcar #'car kept)))
                   (last (apply #'max (mapcar #'car kept))))
              (format "\\addplot+[color=%s,densely dotted,very thick,no marks,forget plot] coordinates {(%d,%.6f) (%d,%.6f)};\n"
                      color first (+ intercept (* slope first)) last (+ intercept (* slope last)))))))))
   (cl-remove-if-not (lambda (s) (or (null kind) (eq kind (plist-get s :kind))))
                     (emacs-nxs-sleep-report--bp-series rows)) "\n"))

(defun emacs-nxs-sleep-report--bp-summary (points)
  "Formatér gennemsnit og stikprøvens SD; SD kræver mindst to punkter."
  (if (null points) "---"
    (let* ((n (length points)) (mean (/ (apply #'+ (mapcar #'cdr points)) (float n)))
           (sd (when (> n 1)
                 (sqrt (/ (cl-loop for (_x . y) in points sum (expt (- y mean) 2)) (1- n))))))
      (replace-regexp-in-string
       "\\." "," (format "%0.1f $\\pm$ %s" mean (if sd (format "%.1f" sd) "---"))))))

(defun emacs-nxs-sleep-report--bp-page (rows)
  "Lav to grafer og spredningstabel på samme side."
  (let ((series (emacs-nxs-sleep-report--bp-series rows)))
    (concat
     "\\begin{center}\n{\\large\\bfseries Blodtryk -- \\SleepPeriod}\\par\n"
     "{\\small Sys: $\\pm7{,}5\\%$; dia: $\\pm15\\%$ af oprindeligt gennemsnit.}\\par\n"
     (mapconcat
      (lambda (kind)
        (concat
         (format "\\begin{tikzpicture}\n\\begin{axis}[reportaxis,height=0.24\\textheight,title={%s},xlabel={Dag},ylabel={mmHg},xmin=0,xmax=32,xtick={1,3,...,31},legend columns=3,legend style={at={(0.5,1.02)},anchor=south,font=\\scriptsize},title style={at={(0.5,1.20)},font=\\normalsize\\bfseries}]\n"
                 (if (eq kind 'sys) "Systolisk blodtryk" "Diastolisk blodtryk"))
         (emacs-nxs-sleep-report--blood-pressure-plot rows kind)
         "\\end{axis}\n\\end{tikzpicture}\\par\\vspace{5mm}\n")) '(sys dia) "\n")
     "{\\small\\bfseries Gennemsnit og spredning af medtagne målinger}\\par\n"
     "\\begin{tblr}{width=\\textwidth,colspec={Q[l,1] Q[r,1] Q[r,1.3] Q[r,1.3] Q[r,1.1]},row{even}={bg=GreenYellow!10},row{1}={bg=DarkGreen,fg=white,font=\\bfseries},hlines,vlines,cells={font=\\footnotesize},rowsep=2pt}\nTidspunkt & Antal sys/dia & Sys: gns. $\\pm$ SD & Dia: gns. $\\pm$ SD & Udeladt sys/dia \\\\\n"
     (mapconcat
      (lambda (i)
        (let* ((sys (nth i series)) (dia (nth (1+ i) series))
               (s (plist-get sys :kept)) (d (plist-get dia :kept)))
          (format "%s & %d / %d & %s & %s & %d / %d \\\\\n"
                  (plist-get sys :label) (length s) (length d)
                  (emacs-nxs-sleep-report--bp-summary s) (emacs-nxs-sleep-report--bp-summary d)
                  (length (plist-get sys :omitted)) (length (plist-get dia :omitted))))) '(0 2 4) "")
     "\\end{tblr}\n\\end{center}\n"
     "{\\footnotesize Gennemsnit og SD i mmHg. SD bruger $n-1$; --- betyder utilstrækkelige data.\\par\n"
     "Alle målepunkter forbindes. Kryds udelades kun fra beregninger og prikkede tendenslinjer.\\par\n"
     "Frasortering sker én gang pr. serie; punkter på grænsen medtages. Ingen fast 160-grænse.\\par\n"
     "Spredningen gælder de medtagne målinger og er ikke apparatets måleusikkerhed.\\par\n"
     "Grafernes y-skalaer tilpasses hver for sig.}\\par\n")))

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

    (emacs-nxs-sleep-report--write
     (expand-file-name "blood-pressure-page.tex" out)
     (emacs-nxs-sleep-report--bp-page blood-pressure-rows))

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
