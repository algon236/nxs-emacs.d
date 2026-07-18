;;; early-init.el --- Emacs NXS configuration, early initialization  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;;

;;; Commentary:
;;  Early init configuration for Emacs NXS
;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable debugging for better error messages.
;;NS (setq debug-on-error t)
;;NS inserted
(setq enable-local-variables :safe
      package-enable-at-startup nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test if started with "--debug-init"
(defvar DEBUG nil
  "goin' to debug?")
;; (setopt DEBUG t)
(when init-file-debug
  (message "*★* '--debug-init' was set; DEBUG is on")
  (setq DEBUG t))
(unless DEBUG
  (setq native-comp-async-report-warnings-errors 'silent))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  You need a modern emacs
(when (version< emacs-version "30")
  (error "nec-emacs requires Emacs 30 or later"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make GUI-started Emacs on macOS see the same important command-line tools
;; as a normal shell.  This is deliberately done in early-init.el so init.el can
;; find git, rg, gls, latexmk, lualatex, etc. immediately.
(defvar emacs-nxs-extra-exec-path
  '("/opt/homebrew/bin"
    "/opt/homebrew/sbin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/Library/TeX/texbin")
  "Extra directories prepended to PATH and `exec-path' very early.")

(dolist (dir (reverse emacs-nxs-extra-exec-path))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (or (getenv "PATH") "")))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup emacs-nxs nil
  "Personal Emacs NXS configuration."
  :group 'emacs)

(defcustom emacs-nxs-avoid-flash-options
  '((enabled          . t)
    (background       . "#1e1e2e")        ;; Catppuccin "#1e1e2e" or Crafters "#292D3E"
    (foreground       . "#292D3E")
    (reset-background . "#292D3E")
    (reset-foreground . "#cdd6f4"))       ;; Catppuccin "#cdd6f4" or Crafters "#EEFFFF"
  "Options to avoid flash of light on Emacs startup.
- `enabled`: Whether to apply the workaround.
- `background`, `foreground`: Initial colors to use.
- `reset-background`, `reset-foreground`: Optional explicit colors to restore after startup.

NOTE: The default values here presented are set for the default
`emacs-nxs' custom theme.  If you'd like to turn this ON with another
theme, change the background/foreground variables.

If reset values are nil, nothing is reset."
  :type '(alist :key-type symbol :value-type (choice (const nil) string))
  :group 'emacs-nxs)



;;; -------------------- PERFORMANCE & HACKS
;; HACK: inscrease startup speed

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil) ; EMACS-31

;; HACK: avoid being flashbanged
(defun emacs-nxs/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, based on `emacs-nxs-avoid-flash-options`."
  (when (alist-get 'enabled emacs-nxs-avoid-flash-options)
    (setq mode-line-format nil)
    (set-face-attribute 'default nil
                        :background (alist-get 'background emacs-nxs-avoid-flash-options)
                        :foreground (alist-get 'foreground emacs-nxs-avoid-flash-options))))

(defun emacs-nxs/reset-default-colors ()
  "Reset any explicitly defined reset values in `emacs-nxs-avoid-flash-options`."
  (when (alist-get 'enabled emacs-nxs-avoid-flash-options)
    (let ((bg (alist-get 'reset-background emacs-nxs-avoid-flash-options))
          (fg (alist-get 'reset-foreground emacs-nxs-avoid-flash-options)))
      (when bg
        (set-face-attribute 'default nil :background bg))
      (when fg
        (set-face-attribute 'default nil :foreground fg)))))

(emacs-nxs/avoid-initial-flash-of-light)
(add-hook 'after-init-hook #'emacs-nxs/reset-default-colors)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; be able to measure time for setup emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom nec/measure-time t
  "Do we want to measure time for this startup?"
  :group 'nec
  :type 'boolean)
;; from http://ergoemacs.org/emacs/elisp_datetime.html
(defvar nec/total-time nil
  "Time at which measurement of the Emacs startup began.")
(defvar nec/nutid0 nil
  "Time of the latest overall startup measurement.")
(defvar nec/nutid1 nil
  "Time of the latest subordinate startup measurement.")
(defvar nec/nutid2 nil
  "Time of the latest detailed startup measurement.")
(setq nec/total-time (current-time)
      nec/nutid0 nec/total-time
      nec/nutid1 nec/nutid0
      nec/nutid2 nec/nutid1)
(defun nec/header (tekst)
  (message  (concat  "*★* " tekst )))
(defun nec/timer (tekst)
  (message  (concat  "*★*     " tekst "  %.5fs" )
    (float-time (time-subtract (current-time) nec/nutid0)))
  (setq nec/nutid0 (current-time)
    nec/nutid1 nec/nutid0
    nec/nutid2 nec/nutid1))
(defun nec/stimer (tekst)
  (message  (concat  "*★*         " tekst "  %.5fs" )
    (float-time (time-subtract (current-time) nec/nutid1)))
  (setq nec/nutid1 (current-time)
    nec/nutid2 nec/nutid1))
(defun nec/sstimer (tekst)
  (message  (concat  "*★*             " tekst "  %.5fs" )
    (float-time (time-subtract (current-time) nec/nutid2)))
  (setq nec/nutid2 (current-time)))
(if nec/measure-time (nec/header "start load time (in 'early-init.el')"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always start Emacs and new frames maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;;NS insert
(dolist (spec `((top . 50)
                (left . 200)
                (width . 280)
                (height . 70)
                (user-position . t)
                (user-size . t)
                (background-color . ,(alist-get 'background emacs-nxs-avoid-flash-options))
                (foreground-color . ,(alist-get 'foreground emacs-nxs-avoid-flash-options))))
  (add-to-list 'default-frame-alist spec)
  (add-to-list 'initial-frame-alist spec))

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat "Emacs - [p] " (project-name project))
              (concat "Emacs - " (buffer-name))))))

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))


;; Keep ordinary startup quiet, but show warnings while debugging.
(when DEBUG
  (setq warning-minimum-level :warning))
(setq warning-suppress-types '((lexical-binding)))
(if nec/measure-time (nec/timer "early-init"))
(provide 'early-init)
;;; early-init.el ends here
