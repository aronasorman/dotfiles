(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))
(setq installed-packages '( ;; separated to make sorting the package list easier
			   evil
			   ace-jump-mode
                           auto-complete
			   deft
			   evil-nerd-commenter
			   evil-leader
			   evil-numbers
			   evil-paredit
			   elixir-mode
			   find-things-fast
			   js2-mode
			   god-mode
			   haskell-mode
			   helm
                           helm-cmd-t
			   ido-ubiquitous
			   ido-vertical-mode
			   linum-relative
			   magit
			   markdown-mode
			   multi-term
                           o-blog
			   ;; org-trello
			   paredit
			   projectile
			   rainbow-delimiters
			   smartparens
			   undo-tree
			   virtualenv
			   tabbar
                           use-package
			   web-mode
			   yasnippet
			   ))

;; load packages
(package-initialize)
(require 'use-package)

;;;;; custom functions and macros
(defun google-it ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(defun concat-dir (x y)
  (concat x "/" y))

(defvar src-dir "~/src")

;; load custom settings
(load "local_init" t)

(defun install-required-packages ()
  (interactive)
  (mapcar (lambda (package)
	    (or (package-installed-p package)
		(if (y-or-n-p (format "Package %s is missing. Install?" package))
		  (package-install package))))
	  installed-packages))
(add-hook 'after-init-hook 'install-required-packages)

(defmacro config-for (pkg &rest body)
  `(eval-after-load ,pkg
     '(progn ,@body)))

;;;;; vanilla emacs config
(defalias 'yes-or-no-p 'y-or-n-p)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(show-paren-mode 1)
;; remove backup from current directories
(defvar temporary-file-directory "/tmp")
(setq backup-directory-alist
            `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
            `((".*" ,temporary-file-directory t)))
(setq reb-re-syntax 'string)
(setq-default indent-tabs-mode nil) ;; fuck tabs
(setq-default truncate-lines t)

(display-time)
(column-number-mode t)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(bind-key "C-M-\\" 'fill-paragraph)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'global-hl-line-mode)

;; font
(set-face-attribute 'default nil :font "Inconsolata-14")

;;;;; vanilla keybindings
(global-set-key (kbd "<RET>") 'newline-and-indent)
(global-set-key (kbd "C-\\") 'delete-other-windows)
(global-set-key (kbd "M-\\") 'delete-window)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)

;;;;; package configs

;; auto-complete
(use-package auto-complete
  :init (progn
          (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict"))
  :config (progn
            (use-package auto-complete-config)
            (ac-config-default)))

(use-package evil
  :init (progn
          (add-to-list 'evil-emacs-state-modes 'grep-mode)
          (add-to-list 'evil-emacs-state-modes 'eshell-mode)
          (bind-key "<SPC>" 'ace-jump-mode evil-normal-state-map)
          (bind-key ";" 'evil-ex evil-normal-state-map)
          (bind-key "\\" 'evil-repeat-find-char evil-normal-state-map)
          (bind-key "C-o" 'imenu evil-normal-state-map)
          (bind-key "!" 'shell-command evil-normal-state-map))
  :config (progn
            (evil-mode t)))

(use-package helm-cmd-t
  :init (bind-key "C-p" 'helm-cmd-t evil-normal-state-map))

(use-package evil-numbers
  :init (progn
          (bind-key "C-a +" 'evil-numbers/inc-at-pt evil-normal-state-map)
          (bind-key "C-a -" 'evil-numbers/dec-at-pt evil-normal-state-map)))

(use-package ace-jump-mode
  :init (progn
          (setq ace-jump-mode-scope 'frame))
  :config (progn
            (require 'cl)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :init (progn
          (add-hook 'lisp-mode-hook 'paredit-mode))
  :config (progn
            (use-package evil-paredit
              :init (add-hook 'paredit-mode-hook 'evil-paredit-mode))))

(use-package projectile
  :init (progn
          (projectile-global-mode t)))

(use-package helm
  :init (progn
          (setq helm-ff-transformer-show-only-basename nil)
          (bind-key "M-x" 'helm-M-x)
          (helm-mode t)))

(use-package evil-leader
  :init (progn
          (setq evil-leader/leader ",")
          (global-evil-leader-mode t))
  :config (evil-leader/set-key
	      "gs" 'magit-status
	      "gb" 'magit-blame-mode
	      "go" 'google-it
	      "ps" 'projectile-switch-project
	      "cc" 'projectile-compile-project
	      "C" 'compile
	      "sl" 'sort-lines
	      "b" 'helm-buffers-list
	      "f" 'find-file
	      "t" 'deft
	      "d" 'dired
	      "m" 'mu4e
	      "|" (lambda () (interactive) (split-window-right) (windmove-right))
	      "-" (lambda () (interactive) (split-window-below) (windmove-down))
	      "." 'find-tag
	      "(" 'paredit-wrap-round
	      "u." 'pop-tag-mark
	      "oa" 'org-agenda
	      "oc" 'org-capture
	      "ot" 'org-todo-list
	      "oo" (lambda () (interactive) (org-tags-view t))
	      "ol" 'org-store-link
	      "w" 'virtualenv-workon
	      "ci" 'evilnc-comment-or-uncomment-lines))

(use-package magit
  :init (progn
          (bind-key "q" 'magit-quit-session magit-status-mode-map)
          (defadvice magit-status (around magit-fullscreen activate)
            (window-configuration-to-register :magit-fullscreen)
            ad-do-it
            (delete-other-windows))
          (defun magit-quit-session ()
            "Restores the previous window configuration and kills the magit buffer"
            (interactive)
            (kill-buffer)
            (jump-to-register :magit-fullscreen))))

(use-package smartparens
  :init (progn
          (add-hook 'emacs-lisp-mode-hook (lambda ()
                                            (turn-off-smartparens-mode)))
          (smartparens-global-mode t))
  :config (progn
	    (sp-local-pair 'html-mode "{" nil :actions nil)
	    (sp-local-pair 'html-mode "{%" "%}")))

(use-package yasnippet
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config (yas/reload-all))

(use-package deft
  :init (progn
          (setq deft-extension "org")
          (setq deft-text-mode 'org-mode)
          (setq deft-auto-save-interval 60.0)
          (setq deft-directory "~/notes")
          (setq deft-use-filename-as-title t)
          (setq org-return-follows-link t)
          (add-hook 'deft-mode-hook 'turn-off-evil-mode)
          (add-to-list 'evil-overriding-maps '(deft-mode-map))
          (bind-key "C-w" 'evil-delete-backward-word deft-mode-map)))

(use-package haskell-mode
  :init (progn
          (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
          (setq haskell-program-name "ghci")))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode))

(use-package elixir-mode)

(use-package linum-relative)

;; config for org mode
;;;; require custom org file
(use-package org
  :load-path ("~/.emacs.d/org/lisp"  "~/.emacs.d/org/contrib/lisp")
  :init (progn
          (setq org-startup-indented t)
          (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAITING(w@)" "|" "DONE(d!)" "CANCELED(c@)")))
          (setq org-log-done 'note)
          (setq org-log-into-drawer t)
          (setq org-default-notes-file "~/notes/capture.org")
          (setq org-html-doctype "html5")
          (setq org-html-html5-fancy t)
          (setq org-capture-templates `(("f" "For all things FLE related" entry
                                         (file "~/notes/todo/fle.org")
                                         "\n* TODO %? \nDEADLINE: %(org-time-stamp nil)\n")

                                        ("p" "Personal stuff" entry
                                         (file "~/notes/todo/personal.org")
                                         "\n* TODO %? \n")

                                        ("r" "Timesheet reports for time spent per task")
                                        ("rd" "Generate daily report" entry
                                         (file+headline "~/notes/reports.org" "daily")
                                         "* %<%Y-%m-%d> %?\n %(insert-clock-table-summary 'today)"
                                         :prepend t)
                                        ("rw" "Generate weekly report" entry
                                         (file+headline "~/notes/reports.org" "weekly")
                                         "* %<%Y-W%V> %?\n %(insert-clock-table-summary 'thisweek)"
                                         :prepend t)
                                        ))
          ))

;; set shortcuts for evil mode
(global-set-key (kbd "C-c C-l") 'org-store-link)
(global-set-key (kbd "C-c C-a") 'org-capture)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-l") 'org-clock-in-last)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(config-for "evil-autoloads"
	    (evil-define-key 'normal org-mode-map
	      "\\]" 'org-shiftmetaright
	      "\\[" 'org-shiftmetaleft
	      "\\t" 'org-todo
	      "\\r" 'org-priority
	      "\\l" 'org-insert-link
	      "\\bt" 'org-babel-tangle
	      "\\t" 'org-set-tags-command))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . t)
   (latex . t)
   (ledger . t)         ;this is the important one for this tutorial
   (ocaml . nil)
   (octave . t)
   (python . t)
   (ruby . t)
   (sh . t)
   (sql . t)))
(add-to-list 'org-babel-tangle-lang-exts '("ledger" . "ldgr"))

;; generate reports for the given date
(defun insert-clock-table-summary (date)
  (save-excursion
    (org-clock-report)
    ;; (search-backward "#+BEGIN: clocktable")
    (end-of-line)
    (insert (format " :scope agenda :block %s" date))
    (org-clock-report)
    ""))                                ; return empty string so we dont affect the result


;; org project settings for site and blog
(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/src/aronasorman.github.io"
         :recursive t
         :publishing-directory "build")))

;; ledger-mode
(use-package ledger
  :mode ("\\.ldgr$" . ledger-mode)
  :load-path  "~/.emacs.d/ledger-mode"
  :init (progn
          (setenv "LEDGER_FILE" "/home/aronasorman/notes/transactions.ldgr")
          (setq ledger-file (getenv "LEDGER_FILE"))
          (setq ledger-file-encrypted (concat ledger-file ".gpg"))))

(defun decrypt-ledger-file ()
  (if (not (executable-find "gpg"))
      (error "No gpg executable found.")
      (epa-decrypt-file ledger-file-encrypted)))

(defun delete-ledger-file ()
  (delete-file ledger-file)
  (message "unencrypted ledger file deleted"))

(defmacro decrypt-ledger-for-defun (fun-name)
  `(defadvice ,fun-name (around read-from-encrypted-ledger activate)
     (decrypt-ledger-file)
     ad-do-it
     (delete-ledger-file)))

(decrypt-ledger-for-defun ledger-report)
(decrypt-ledger-for-defun ledger-report-redo)
(config-for "evil-autoloads"
	    (add-to-list 'evil-emacs-state-modes 'ledger-report-mode))

;; web-mode
(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :init (progn
          (add-hook 'web-mode-hook 'turn-off-smartparens-mode)
          (setq web-mode-engines-alist
                '(("django" . "\\.html$")))))

;; mu and mu4e
(use-package mu4e
  :load-path "~/src/dotfiles/mu4e"
  :init (progn
          (add-to-list 'mu4e-bookmarks '("flag:flagged" "All flagged email" ?F))
          (add-to-list 'mu4e-bookmarks '("flag:flagged AND date:today..now" "Flagged email for today" ?f))
          (setq mu4e-maildir "~/mail")
          (setenv "GPGKEY" "0B78EF87")
          ;; startup GPG agent
          (let ((gpg-agent (executable-find "gpg-agent")))
            (if gpg-agent
                (let* ((gpg-agent-cmd (shell-command-to-string "gpg-agent --daemon"))
                       (gpg-agent-info-var (first (split-string gpg-agent-cmd ";")))
                       (gpg-agent-info (second (split-string gpg-agent-info-var "="))))
                  (message "gpg-agent found. Activating for Emacs...")
                  (setenv "GPG_AGENT_INFO" gpg-agent-info))
              (message "gpg-agent exectuable not found. Oh well.")))
          (setq mml2015-use 'epg))
  :config (progn
            (use-package org-mu4e)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-view-mode)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
	    (add-hook 'mu4e-view-mode-hook 'turn-on-visual-line-mode)
	    (add-hook 'mu4e-compose-mode-hook 'evil-local-mode)
	    (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
	    (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)))
(defvar local/mu4e-account-specific-settings)
(setq local/mu4e-account-specific-settings
      '(("fastmail"
	 (user-mail-address "aronasorman@fastmail.fm")
	 (smtpmail-smtp-server "mail.messagingengine.com")
	 (smtpmail-smtp-service 587))
	("learningequality"
	 (user-mail-address "aron@learningequality.org")
	 (smtpmail-smtp-server "smtp.gmail.com")
	 (smtpmail-smtp-service 587))))

(defun local/mu4e-set-account-settings ()
  "Set the account for composing a message."
  (let* ((account
	  (completing-read (format "Compose with account: (%s) "
				   (mapconcat #'(lambda (var) (car var)) local/mu4e-account-specific-settings "/"))
			   (mapcar #'(lambda (var) (car var)) local/mu4e-account-specific-settings)
			   nil t nil nil (caar local/mu4e-account-specific-settings)))
	 (account-vars (cdr (assoc account local/mu4e-account-specific-settings))))
    (if account-vars
	(mapc #'(lambda (var)
		  (set (car var) (cadr var)))
	      account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'local/mu4e-set-account-settings)

;; run backup script
(setq local/files-to-backup '("~/notes" "~/mail" "~/crypt"))
(setq local/files-to-backup (cl-concatenate 'list
					    local/files-to-backup
					    (split-string (shell-command-to-string "find ~/.emacs.d/ -name '*.el'") "\n")))

(defun local/run-backup ()
  (interactive)
  (let ((files (mapconcat 'identity local/files-to-backup " ")))
      (async-shell-command (format "python ~/src/dotfiles/scripts/backup.py %s" files))))
(add-hook 'midnight-hook 'local/run-backup)

;; midnight mode
;; (midnight-delay-set 'midnight-delay "6:00am")

(add-hook 'prog-mode-hook 'linum-mode) ;; avoid loading global-linum-mode now
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" default)))
 '(midnight-mode t nil (midnight))
 '(org-agenda-files (quote ("~/notes/todo/personal.org" "~/notes/todo/habits.org" "~/notes/todo/fle.org")))
 '(org-html-table-default-attributes (quote (:border "2" :cellspacing "5" :cellpadding "6" :rules "groups" :frame "hsides")))
 '(org-modules (quote (org-bbdb org-bibtex org-crypt org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-invoice)))
 '(safe-local-variable-values (quote ((ledger-master-file . "transactions.ldgr") (major-mode quote ledger-mode) (major-mode . ledger-mode))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
