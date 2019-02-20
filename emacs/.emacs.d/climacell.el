;;; This pre-tangled ORG configuration file contains overrides to the more generic configuration I previously used in academia. In particular, we update things like agenda and refiling configurations, just to simplify things a bit.

(require 'package)
(require 'org)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-download ob-ipython use-package org-ref monokai-theme moe-theme material-theme markdown-mode magit leuven-theme idle-require counsel color-theme better-defaults auto-complete anaconda-mode))))

;;; Override agenda files
(setq org-agenda-files '("~/org/climacell.org"))

;;; Direct linking to ClimaCell JIRA tasks
(org-add-link-type "jira" 'org-jira-open)
(setq jiralib-url "https://climacell.atlassian.net")
(defun org-jira-open (ticket)
  "Visit a given ClimaCell ticket in the browser"
  (funcall 'browse-url (concat "https://climacell.atlassian.net/browse/" ticket)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)
