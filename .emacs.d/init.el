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
			   workgroups
			   yasnippet
			   ))

;;;;; custom functions and macros
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
	      "cc" 'projectile-compile-project
	      "C" 'compile
	      "t" 'deft
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

(config-for "magit"
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

(config-for "elixir-mode"
	    (require 'elixir-mode))

;; disabled because it slowed down emacs
;; (config-for "linum-relative-autoloads"
;; 	    (add-hook 'linum-mode-hook (lambda ()
;; 					 (require 'linum-relative))))

(add-hook 'prog-mode-hook 'global-linum-mode) ;; avoid loading global-linum-mode now
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(package-initialize)
