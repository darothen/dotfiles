;; Based heavily off of Norang and writequit.org (http://writequit.org/org/#orgheadline39)

;;;
;;; Main Emacs Changes
;;;

;; Function for determining if in terminal or not
(defun is-in-terminal()
  (not (display-graphic-p)))

;; Disable startup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Disable scroll bar, tool bar, menu bar
;(scroll-bar-mode -1)
;(tool-bar-mode -1)
;(menu-bar-mode -1)

(setq user-full-name "Daniel Rothenberg"
      user-mail-address "daniel@danielrothenberg.com")

(set-keyboard-coding-system nil)

;; Turn on syntax highlighting for all buffers
(global-font-lock-mode t)

;; Setup UTF-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Quicker echoing of unfishined commands
(setq echo-keystrokes 0.1)

;; Ignore case when using completion for file names
(setq read-file-name-completion-ignore-case t)

;; Shortcut yes/no answer
(defalias 'yes-or-no-p 'y-or-n-p)

;; Move around lines based on how they're displayed
(setq line-move-visual t)

;; Set fill column to 80 characters and 4-character tabs
(setq-default fill-column 80)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

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

;;;
;;; Package management / setup
;;;
;; List the packages we want to install
(setq package-list
  '(auto-complete
    better-defaults
    bind-key
    color-theme
    leuven-theme
    markdown-mode
    moe-theme
    monokai-theme
    org
    org-ref
    solarized-theme
    tangotango-theme
    use-package)
)
(require 'package)
;; Name the package archives
(setq package-archives
  '(("marmalade" . "http://marmalade-repo.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/"))
)
;; Fetch the list of available packages
(package-initialize)
;; Comment out this line to avoid re-polling the archive each startup
; (package-refresh-contents)
;; Install any missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;
;;; Aesthetic Tweaks
;;;
;; Loads of better default options
(require 'better-defaults)

;; Set default font
; (set-face-attribute 'default nil :font "Source Code Pro Bold-13")

;; Enable color theme
; Light background - leuven
(load-theme 'leuven t)
; Dark backgroud - monokai
; (load-theme 'monokai t)


;;;
;;; org-mode Configuration
;;;
(require 'org)

;; Associate all *.org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; Set some useful default keys:
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "<f9> <f9>" 'org-agenda-list)

(setq org-hide-leading-stars t)
(setq org-cycle-separator-lines 2)
; (setq org-odd-level-only nil)
(setq org-insert-heading-respect-content nil)
; (setq org-M-RET-may-split-line '((item) (default . t)))
(setq org-special-ctrl-a/e t)
(setq org-return-follows-link nil)
; (setq org-use-speed-commands t)
; (setq org-startup-align-all-tables nil)
(setq org-log-into-drawer nil)
(setq org-tags-column 1)
(setq org-ellipsis " \u25bc" )
; (setq org-speed-commands-user nil)
; (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-completion-use-ido t)
(setq org-indent-mode nil)
(setq org-enforce-todo-dependencies t) ;; Can't close projects w/ incomplete tasks
; (setq org-startup-truncated nil)
(setq auto-fill-mode -1)
; (setq-default fill-column 99999)
; (setq fill-column 99999)

;; Agenda
(setq org-agenda-files
      (append '("~/org/main.org")
              (file-expand-wildcards "~/org/cals/*.org")))

;; Tasks and States
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "SHELF(s@/!)")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "blue" :weight bold)
        ("NEXT" :foreground "orange" :weight bold)
        ("INPROGRESS" :foreground "lightgreen" :weight bold)
        ("DONE" :foreground "forestgreen" :weight bold)
        ("WAITING" :foreground "gold" :weight bold)
        ("HOLD" :foreground "red" :weight bold)
        ("SHELF" :foreground "purple" :weight bold)))

;; Capturing Tasks and Notes
(setq org-directory "~/org")
(setq org-default-notes-files "~/org/refile.org")
; Bind a keystroke to access org-capture
(bind-key "C-c r" 'org-capture)
; Capture templates - the
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t))))

;; Clocking
(eval-after-load 'org-agenda
 '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))
; Always leave the clock open
(setq org-clock-in-resume t)
; Clock to a LOGBOOK drawer
(setq org-clock-into-drawer t)
(setq org-drawers '("PROPERTIES" "LOGBOOK"))
; Automatically remove 0:00 duration clocks
(setq org-clock-out-remove-zero-time-clocks t)

;; org-reveal
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")

;; org-ref
(defun load-org-ref ()
  (require 'org-ref)
)
(add-hook 'org-mode-hook 'load-org-ref)
(setq reftex-default-bibliography '("~/Dropbox_MIT/Papers/library.bib"))
(setq org-ref-bibliography-notes "~/org/papers.org"
      org-ref-default-bibliography '("~/Dropbox_MIT/Papers/library.bib")
      org-ref-pdf-directory "~/Dropbox_MIT/Papers/")
(setq bibtex-completion-bibliography "~/Dropbox_MIT/Papers/library.bib"
      bibtex-completion-library-path "~/Dropbox_MIT/Papers/"
      bibtex-completion-notes-path "~/Dropbox_MIT/Papers/helm-bibtex-notes")
; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; Pandoc export
(with-eval-after-load 'ox
  (require 'ox-pandoc))

;; Babel
; Active langueges
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   ; (shell . t)
   ; (latex . t)
 ))

;; EasyPG Encryption
; http://orgmode.org/worg/org-tutorials/encrypting-files.html
(require 'epa-file)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Word count - org-wc
;(defun load-org-wc ()
;  (require 'org-ref)
;)
(add-hook 'org-mode (lambda () require 'org-wc))

;; Refiling into log
; For now, my log exists in main.org under the header "Log". This setting
; enables me to refile things into there.
;(setq org-refile-targets '(("main.org" :maxlevel . 2)))

;;;
;;; In-program Customization
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f" "70b9c3d480948a3d007978b29e31d6ab9d7e259105d558c41f8b9532c13219aa" default)))
 '(org-agenda-files
   (quote
    ("~/org/main.org" "~/org/cals/meetings.org" "~/org/cals/misc.org" "~/org/cals/music.org")))
 '(safe-local-variable-values (quote ((org-hide-emphasis-markers)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
