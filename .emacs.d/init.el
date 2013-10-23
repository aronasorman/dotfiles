(require 'package)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
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
			   ido-ubiquitous
			   ido-vertical-mode
			   linum-relative
			   magit
			   markdown-mode
			   multi-term
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

(add-hook 'after-init-hook 'ido-mode)
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
	    (define-key evil-normal-state-map (kbd "C-p") 'ftf-find-file)
	    (define-key evil-normal-state-map (kbd "!") 'shell-command)
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

(config-for "ido-ubiquitous-autoloads"
	    (ido-ubiquitous-mode 1))

(config-for "ido-vertical-mode-autoloads"
	    (ido-vertical-mode 1))

(config-for "evil-leader-autoloads"
	    (setq evil-leader/leader ",")
	    (evil-leader/set-key
	      "gs" 'magit-status
	      "gb" 'magit-blame-mode
	      "go" 'google-it
	      "ps" 'projectile-switch-project
	      "cc" 'projectile-compile-project
	      "C" 'compile
	      "b" 'ido-switch-buffer
	      "f" 'find-file
	      "t" 'deft
	      "m" 'mu4e
	      "|" (lambda () (interactive) (split-window-right) (windmove-right))
	      "-" (lambda () (interactive) (split-window-below) (windmove-down))
	      "." 'find-tag
	      "(" 'paredit-wrap-round
	      "u." 'pop-tag-mark
	      "oa" 'org-agenda
	      "oc" 'org-capture
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
(setq org-startup-indented t)
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAITING(w@)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-log-done 'note)
(setq org-log-into-drawer t)
(setq org-default-notes-file "~/notes/capture.org")
(setq org-capture-templates `(("f" "For all things FLE related")

			      ("fp" "partnership" entry
			       (file+olp "~/notes/todo.org" "FLE" "partnerships")
			       "\n* TODO  %?\n"
			       :prepend t)
			      ("fd" "development overall tasks" entry
			       (file+olp "~/notes/todo.org" "FLE" "dev")
			       "\n* TODO  %?\n %^{DEADLINE}p %^{SCHEDULED}p")
			      ("fb" "bugs found or FIXMEs in code" entry
			       (file+olp "~/notes/todo.org" "FLE" "dev")
			       "\n* TODO found in %f: %?\n FILE: %a")

			      ("p" "Personal stuff not really related to any project")

			      ("pw" "TODOs related to workflow" entry
			       (file+olp "~/notes/todo.org" "Personal" "workflow")
			       "\n* TODO %?\n %^G")
			      ("pt" "Small tasks not really related to anything" entry
			       (file+olp "~/notes/todo.org" "Personal" "random")
			       "\n* TODO %?\n")))

;; set shortcuts for evil mode
(config-for "evil-autoloads"
	    (evil-define-key 'normal org-mode-map
	      "\\]" 'org-shiftmetaright
	      "\\[" 'org-shiftmetaleft
	      "\\t" 'org-todo
	      "\\r" 'org-priority
	      "\\l" 'org-insert-link
	      "\\t" 'org-set-tags-command))

;; mu and mu4e
(add-to-list 'load-path (concat-dir src-dir "dotfiles/mu4e"))
(require 'mu4e)
(require 'org-mu4e)
(config-for "evil-autoloads"
	    (add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-view-mode)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-compose-mode)
	    (add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
	    (add-hook 'mu4e-message-mode 'turn-on-visual-line-mode))
(setq mu4e-maildir "~/mail")

(add-hook 'prog-mode-hook 'global-linum-mode) ;; avoid loading global-linum-mode now
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" default)))
 '(org-agenda-files (quote ("~/notes/todo.org")))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
