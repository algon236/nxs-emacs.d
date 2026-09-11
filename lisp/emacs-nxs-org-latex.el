;;; emacs-nxs-org-latex.el --- NXS Org-LaTeX-tabeller -*- lexical-binding: t; -*-

;;; Commentary:
;; Den fælles preamble leverer tabularray. Sundhedstilpasninger gælder kun
;; navngivne tabeller i sundhedsmappen og kun eksportens midlertidige buffer.

;;; Code:
(require 'cl-lib)

(defcustom emacs-nxs-org-latex-health-directory
  (expand-file-name "~/Documents/niels/Sundhed/")
  "Mappe med sundhedsrapporter, inklusive månedlige undermapper."
  :type 'directory :group 'emacs-nxs)

(with-eval-after-load 'ox-latex
  (setq org-latex-default-table-environment "longtblr"
        org-latex-tables-centered nil))

(defun emacs-nxs-org-latex--set-longtblr-no-float (name-regexp)
  "Tilpas en rigtig tabel med NAME-REGEXP; returnér en marker før attributten.
Bevar øvrige LaTeX-attributter og undgå at ændre navngivne kodeblokke."
  (require 'org-element)
  (when-let* ((table
              (org-element-map (org-element-parse-buffer) 'table
                (lambda (element)
                  (when-let* ((name (org-element-property :name element)))
                    (when (string-match-p name-regexp name) element)))
                nil t)))
    (let* ((begin (org-element-property :begin table))
           (end (org-element-property :post-affiliated table))
           (text (buffer-substring-no-properties begin end))
           (case-fold-search t)
           (attribute "#+ATTR_LATEX: :environment longtblr :float nil\n"))
      ;; Remove only the two overridden options, wherever they occur.
      (setq text
            (replace-regexp-in-string
             "^[ \t]*#\\+ATTR_LATEX:.*$"
             (lambda (line)
               (replace-regexp-in-string
                "[ \t]+:\\(?:environment\\|float\\)[ \t]+[^ \t\n]+" "" line))
             text t t))
      ;; An empty attribute line makes Org read the preceding nil as "nil ".
      (setq text (replace-regexp-in-string
                  "^[ \t]*#\\+ATTR_LATEX:[ \t]*\n" "" text))
      (goto-char begin)
      (delete-region begin end)
      (insert attribute text)
      (copy-marker begin))))

(defun emacs-nxs-org-latex--prepare-medicine-table (backend)
  "Tilpas sundhedstabeller i eksportkopien til BACKEND."
  (when (and (org-export-derived-backend-p backend 'latex)
             buffer-file-name
             (file-in-directory-p buffer-file-name
                                  emacs-nxs-org-latex-health-directory))
    (save-excursion
      (when-let* ((marker
                   (emacs-nxs-org-latex--set-longtblr-no-float
                    (concat "\\`"
                            (regexp-opt '("januar" "februar" "marts" "april" "maj"
                                          "juni" "juli" "august" "september"
                                          "oktober" "november" "december"))
                            "-[0-9]\\{4\\}\\'"))))
        (set-marker marker nil))
      (when-let* ((marker
                   (emacs-nxs-org-latex--set-longtblr-no-float
                    "\\`Status-Medicin-uden-kolonne-2-og-8\\'")))
        (goto-char marker)
        (insert "#+LATEX: \\begin{center}\\textbf{Medicin Status}\\end{center}\n")
        (set-marker marker nil)))))

(add-hook 'org-export-before-parsing-functions
          #'emacs-nxs-org-latex--prepare-medicine-table)

(provide 'emacs-nxs-org-latex)
;;; emacs-nxs-org-latex.el ends here
