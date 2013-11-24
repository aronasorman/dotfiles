(require 'package)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))
(setq installed-packages '( ;; separated to make sorting the package list easier
			   evil
			   ace-jump-mode
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
			   web-mode
			   yasnippet
			   ))

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
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(global-set-key (kbd "C-M-\\") 'fill-paragraph)

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

;; (add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'global-hl-line-mode)

(config-for "evil-autoloads"
	    (evil-mode 1)
	    (add-to-list 'evil-overriding-maps '(eshell-mode-map))
	    (add-hook 'eshell-mode-hook 'turn-off-evil-mode)
	    ;; terminate emacs with C-z
	    (define-key evil-normal-state-map (kbd "C-z") 'suspend-frame)
	    (define-key evil-insert-state-map (kbd "C-z") 'suspend-frame)
	    (define-key evil-normal-state-map (kbd "<SPC>") 'ace-jump-mode)
	    ;; terminal emacs somehow funges the escape key
	    (define-key evil-insert-state-map (kbd "C-[") 'evil-normal-state)
	    (define-key evil-insert-state-map (kbd "<ESC>") 'evil-normal-state)
	    ;; some conveniences from my vim stint
	    (define-key evil-normal-state-map (kbd ";") 'evil-ex)
	    (define-key evil-normal-state-map (kbd "\\") 'evil-repeat-find-char)
	    ;; miscellaneous keybindings
	    (define-key evil-normal-state-map (kbd "C-o") 'imenu)
	    (define-key evil-normal-state-map (kbd "C-p") 'helm-cmd-t)
	    (define-key evil-normal-state-map (kbd "!") 'shell-command)
	    (add-to-list 'evil-emacs-state-modes 'grep-mode)
	    (add-to-list 'evil-emacs-state-modes 'eshell-mode)
	    (define-key evil-normal-state-map (kbd "C-a +") 'evil-numbers/inc-at-pt)
	    (define-key evil-normal-state-map (kbd "C-a -") 'evil-numbers/dec-at-pt)
)

(config-for "god-mode-autoloads"
	    ;; (god-mode)
	    (global-set-key (kbd "<backtab>") 'god-local-mode))

(config-for "ace-jump-mode-autoloads"
	    (require 'cl)
	    (global-set-key (kbd "C-M-<SPC>") 'ace-jump-mode)
	    (setq ace-jump-mode-scope 'frame))

(config-for "rainbow-delimiters-autoloads"
	    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(config-for "paredit-autoloads"
	    (add-hook 'lisp-mode-hook 'paredit-mode))

(config-for "projectile-autoloads"
	    (projectile-global-mode 1))

;; (config-for "ido-ubiquitous-autoloads"
;; 	    (ido-ubiquitous-mode 1))

;; (config-for "ido-vertical-mode-autoloads"
;; 	    (ido-vertical-mode 1))

(config-for "helm-autoloads" ;; helm is now fast enough! hooray!
	    (setq helm-ff-transformer-show-only-basename nil)
	    (global-set-key (kbd "M-x") 'helm-M-x))

(defun local/find-file-in-project ()
  (interactive)
  (helm))

(config-for "evil-leader-autoloads"
	    (setq evil-leader/leader ",")
	    (evil-leader/set-key
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
	      "ci" 'evilnc-comment-or-uncomment-lines)
	    (global-evil-leader-mode 1))

; (config-for "workgroups-autoloads"
;; 	    (require 'workgroups)
;; 	    (setq wg-prefix-key (kbd "C-x w"))
;; 	    (workgroups-mode 1)
;; 	    (wg-load "~/.emacs.d/workgroups"))

(config-for "evil-paredit-autoloads"
	    (require 'evil-paredit)
	    (add-hook 'paredit-mode-hook 'evil-paredit-mode))

(config-for "find-things-fast-autoloads"
	    (require 'find-things-fast))

(config-for "magit-autoloads"
	    (require 'magit)
	    (defadvice magit-status (around magit-fullscreen activate)
	      (window-configuration-to-register :magit-fullscreen)
	      ad-do-it
	      (delete-other-windows))
	    (defun magit-quit-session ()
	      "Restores the previous window configuration and kills the magit buffer"
	      (interactive)
	      (kill-buffer)
	      (jump-to-register :magit-fullscreen))
	    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
	    (global-set-key (kbd "C-x C-g") 'magit-status)
	    (global-set-key (kbd "<f11>") 'magit-status))

(config-for "smartparens-autoloads"
	    (require 'smartparens)
	    (add-hook 'emacs-lisp-mode-hook (lambda ()
					      (turn-off-smartparens-mode)
					      (paredit-mode 1)))
	    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil) ; do not match the ' character
	    (sp-local-pair 'html-mode "{" nil :actions nil)
	    (sp-local-pair 'html-mode "{%" "%}")
	    (smartparens-global-mode 1))

(config-for "yasnippet-autoloads"
	    (require 'yasnippet)
	    (yas-global-mode 1))

(config-for "deft-autoloads"
	    (require 'deft)
	    (setq deft-extension "org")
	    (setq deft-text-mode 'org-mode)
	    (setq deft-auto-save-interval 60.0)
	    (setq deft-directory "~/notes")
	    (setq deft-use-filename-as-title 1)
	    (setq org-return-follows-link t)
	    (global-set-key (kbd "<kp-enter>") 'deft)
	    (add-hook 'deft-mode-hook 'turn-off-evil-mode)
	    (add-to-list 'evil-overriding-maps '(deft-mode-map))
	    ;; keybindings
	    (define-key deft-mode-map (kbd "C-w") 'evil-delete-backward-word))

(config-for "haskell-mode"
	    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
	    (setq haskell-program-name "ghci"))

(config-for "js2-mode"
	    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(config-for "elixir-mode"
	    (require 'elixir-mode))

(config-for "linum-relative-autoloads"
	    (add-hook 'linum-mode-hook (lambda ()
					 (require 'linum-relative))))

;; config for org mode
;;;; require custom org file
(add-to-list 'load-path "~/.emacs.d/org/lisp")
(add-to-list 'load-path "~/.emacs.d/org/contrib/lisp")
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



;; (config-for "org-trello-autoloads"
;; 	    (add-hook 'org-mode-hook 'org-trello-mode))

;; ledger-mode
(add-to-list 'load-path "~/.emacs.d/ledger-mode")
(require 'ledger)
(add-to-list 'auto-mode-alist '("\\.ldgr$" . ledger-mode))
(setenv "LEDGER_FILE" "/home/aronasorman/notes/transactions.ldgr")
(setq ledger-file (getenv "LEDGER_FILE"))
(setq ledger-file-encrypted (concat ledger-file ".gpg"))

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
(config-for "web-mode-autoloads"	; weird. it's a major mode, but we need the *-autoloads
	    (require 'web-mode)
	    (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
	    (add-hook 'web-mode-hook 'turn-off-smartparens-mode)
	    (setq web-mode-engines-alist
		  '(("django" . "\\.html$"))))

;; mu and mu4e
(add-to-list 'load-path (concat-dir src-dir "dotfiles/mu4e"))
(require 'mu4e)
(require 'org-mu4e)
(config-for "evil-autoloads"
	    (add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-view-mode)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
	    (add-hook 'mu4e-view-mode-hook 'turn-on-visual-line-mode)
	    (add-hook 'mu4e-compose-mode-hook 'evil-local-mode)
	    (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
	    (add-hook 'mu4e-view-mode-hook 'epa-mail-mode))
(add-to-list 'mu4e-bookmarks '("flag:flagged" "All flagged email" ?F))
(add-to-list 'mu4e-bookmarks '("flag:flagged AND date:today..now" "Flagged email for today" ?f))
(setq mu4e-maildir "~/mail")
(setenv "GPGKEY" "0B78EF87")
(let ((gpg-agent (executable-find "gpg-agent")))
  (if gpg-agent
      (let* ((gpg-agent-cmd (shell-command-to-string "gpg-agent --daemon"))
	     (gpg-agent-info-var (first (split-string gpg-agent-cmd ";")))
	     (gpg-agent-info (second (split-string gpg-agent-info-var "="))))
	(message "gpg-agent found. Activating for Emacs...")
	(setenv "GPG_AGENT_INFO" gpg-agent-info))
    (message "gpg-agent exectuable not found. Oh well.")))
(setq mml2015-use 'epg)

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

(defun local/insert-transaction-to-ledger ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\($[[:digit:]]+.[[:digit:]].\\).+transaction to \\([[:word:]\\|[:space:]]+\\) on [[:digit:]]")
    (let* ((default-price (match-string 1))
	   (default-payee (match-string 2))
	   (price (read-string (concat "Amount transferred (default " default-price "):") nil nil default-price))
	   (payee (read-string (concat "Paid to (default " default-payee "):") nil nil default-payee))
	   (paid-to-category (read-string "Payee category:" "expenses:food"))
	   (paid-from-category (read-string "Paid from category:" "assets:checking:chase")))
      (window-configuration-to-register :before-transaction-insert)  ; note: factor out into a with-configuration
      (with-current-buffer (find-file ledger-file-encrypted)
	(goto-char (point-max))
	(newline)
	(insert (format-time-string "%Y/%m/%d ") payee)
	(newline)
	(insert " " paid-to-category "  " price)
	(newline)
	(insert " " paid-from-category)
	(newline)
	(if (y-or-n-p "Save ledger?")
	    (save-buffer)
	  (revert-buffer t t)))
      (jump-to-register :before-transaction-insert))))
(define-key 'mu4e-view-mode-map (kbd "l") 'local/insert-transaction-to-ledger) ;; change once we can process more email types

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
(midnight-delay-set 'midnight-delay "6:00am")

(add-hook 'prog-mode-hook 'linum-mode) ;; avoid loading global-linum-mode now
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(package-initialize)
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
