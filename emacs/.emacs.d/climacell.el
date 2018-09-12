(require 'package)
(require 'org)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;; Direct linking to ClimaCell JIRA tasks
(org-add-link-type "jira" 'org-jira-open)
(setq jiralib-url "https://climacell.atlassian.net")
(defun org-jira-open (ticket)
  "Visit a given ClimaCell ticket in the browser"
  (funcall 'browse-url (concat "https://climacell.atlassian.net/browse/" ticket)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-download ob-ipython use-package org-ref monokai-theme moe-theme material-theme markdown-mode magit leuven-theme idle-require counsel color-theme better-defaults auto-complete anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
