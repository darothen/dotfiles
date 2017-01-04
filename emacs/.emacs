;; Customized .emacs configuration file
;;
;; Re-organized on December 27, 2016
;; Daniel Rothenberg <darothen@mit.edu>
;;
;; Based heavily off of Norang and writequit.org (http://writequit.org/org/#orgheadline39)
;; Other useful sources:
;; - https://github.com/larstvei/dot-emacs

;;;
;;; Main Emacs Changes
;;;

(setq user-full-name "Daniel Rothenberg"
      user-mail-address "daniel@danielrothenberg.com")

;;;
;;; Package management / setup
;;;
(require 'cl)
(require 'package)

;; Fetch the list of available packages
(package-initialize)

;; Name the package archives
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")))

;; List the packages we want to install
(let* ((packages
        '(anaconda-mode      ; Alternative Python major mode
          auto-complete      ; Auto-completions based on buffer contents
          better-defaults    ; Aesthetics and other tweaks
          bind-key           ; Alias for more efficiently binding custom keys
          color-theme        ; Color/theme chooser
          ; idle-require       ; Delayed package loading
          leuven-theme       ; Nice light white/blue theme
          magit              ; Tools for controlling git from Emacs
          markdown-mode      ; Major mode for editing Markdown files
          material-theme     ; Mixed theme from Google Material
          moe-theme          ; Dark theme
          monokai-theme      ; Classic slate/dark theme
          org                ; Outline-based notes and task manager
          org-ref            ; Integrated reference manager for Org mode
          use-package))      ; Alias for loading packages
      (packages (remove-if 'package-installed-p packages)))
(when packages
  (ignore-errors (unless package-archive-contents (package-refresh-contents))
                 (mapcar 'package-install packages))))

; Delayed loading - for packages which can be cumbersome and time-consuming
; to load, this forces them to be loaded later on only when emacs is idling.
; Helps to speed startup times
(require 'idle-require)
;(dolist (feature
;         '(anaconda-mode
;           auto-complete
;           org
;           ox-latex
;           ox-md
;           ox-pandoc))
; (idle-require feature))

(setq idle-require-delay 5)
(idle-require-mode 1)

;;; Sane defaults

;; Loads of better default options
(require 'better-defaults)

;; Function for determining if in terminal or not
(defun is-in-terminal()
  (not (display-graphic-p)))

;; Set default values
(setq auto-revert-interval 1         ; Refresh buffers fast
      default-buffer-file-coding-system 'utf-8 ; UTF-8 default file encoding
      default-tab-width 4            ; Spaces per tab
      echo-keystrokes 0.1            ; Show keystrokes ASAP
      inhibit-startup-screen t       ; Don't display the startup screen
      initial-scratch-message nil    ; Clean scratch buffer
      line-move-visual t             ; Wrap long-lines
      read-file-name-completion-ignore-case t ; Ignore case when reading file names
      ring-bell-function 'ignore     ; Quiet that damn bell
      sentence-end-double-space nil  ; Sentences should end with one space
)

;; Set defaults for buffer-local values
(setq-default fill-column 80         ; Set the fill column to 80
              indent-tabs-mode nil   ; Use spaces for indents, not tabs
              ;split-width-threshold 100 ; Split vertically by default
)

;; Setup UTF-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Turn on syntax highlighting for all buffers
(global-font-lock-mode t)

;; Shortcut yes/no answer
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable scroll bar, tool bar, menu bar
;(scroll-bar-mode -1)
;(tool-bar-mode -1)
;(menu-bar-mode -1)

;;; Visual Settings

;; Set default theme to "Leuven" because it's awesome
(load-theme 'leuven t)

;; Quick-cycle between themes
; https://github.com/larstvei/dot-emacs
(defun cycle-themes()
  "Returns a function that lets you cycle your themes."
  (lexical-let ((themes '#1=(leuven material . #1#)))
    (lambda ()
      (interactive)
      ;; Rotates the theme cycle and changes the current theme.
      (load-theme (car (setq themes (cdr themes))) t))))

;;; Misc

;; Alias to bury scratch, but never kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Require newline at end of files
(setq require-final-newline t)

;; Automatically revert file if changed on disk
(global-auto-revert-mode 1)
;; be quiet about reverting files
(setq auto-revert-verbose nil)

;; Column and line modes
(column-number-mode 1)
(setq linum-format "%d ")
(global-linum-mode 1)
(global-visual-line-mode)

;; Visualize empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Disable dialog boxes and beeping noise
(setq use-dialog-box nil
      visible-bell t)

;; Show parentheses mode
(show-paren-mode t)

;; Undo tree mode, for visualizing undos/redos
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Winner Mode - undo and redo window configuration
;; Use C-c <left> and C-c <right> to switch b/t window configurations
(use-package winner
  :defer t)

;;;
;;; Additional scripts to load
;;;
(setq custom-file "~/.emacs.d/darothen.el")
(load custom-file t)
