(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ;; ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))
;; load packages
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/use-package")
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

;;;;; vanilla emacs config
(defalias 'yes-or-no-p 'y-or-n-p)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(global-hl-line-mode 1)
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

(add-hook 'after-init-hook 'global-auto-revert-mode)
;; (add-hook 'after-init-hook 'global-hl-line-mode)

;; font
(set-face-attribute 'default nil :font "Inconsolata-11")

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

;; debug line shortcuts
(setq debug-line-alist
      '((python-mode . "import pdb; pdb.set_trace()")))

(defun insert-debug-line ()
  (interactive)
  (let ((debug-line (cdr (assoc major-mode debug-line-alist))))
    (insert debug-line)))
(bind-key "C-x d" 'insert-debug-line)

(defun align-to-equals (begin end)
  "Align region to equal signs"
   (interactive "r")
   (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))
(bind-key "C-x a a" 'align-to-equals)
(bind-key "C-x a e" 'align-regexp)

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(defvar frame-maximized-p nil)
(make-variable-frame-local 'frame-maximized-p)
(defun toggle-window-maximization ()
  (interactive)
  (if frame-maximized-p
      (progn
        (balance-windows)
        (setq frame-maximized-p nil))
    (progn
      (maximize-window)
      (setq frame-maximized-p t))))
(bind-key "M-RET" 'toggle-window-maximization)

;;;;; path configuration

;; add /usr/local/bin to path
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))

;;;;; package configs

;; auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :diminish auto-complete-mode
;;   :init (progn
;;           (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict"))
;;   :config (progn
;;             (use-package auto-complete-config
;;               )
;;             (ac-config-default)))
(use-package company
  :ensure t
  :init (progn
          (add-hook 'after-init-hook 'global-company-mode)))

(use-package diminish
  :ensure t)

;; ;;; required by crosshairs-mode
;; (use-package hl-line+)

;; (use-package col-highlight
;;   :ensure t)

;; (use-package crosshairs
;;   :ensure t
;;   :config (progn
;;             (add-hook 'prog-mode-hook 'crosshairs-mode)))

(use-package handlebars-mode
  :ensure t)

;; shell paths
(use-package exec-path-from-shell
  :ensure t)

;; dependencies of other packages.
(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package pkg-info
  :ensure t)

;; (use-package powerline
;;   :ensure t
;;   :config (progn
;;             (powerline-default-theme)))

(use-package evil
  :ensure t
  :config (progn
            (add-to-list 'evil-emacs-state-modes 'grep-mode)
            (add-to-list 'evil-emacs-state-modes 'eshell-mode)
            (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
            (bind-key "<SPC>" 'ace-jump-mode evil-normal-state-map)
            (bind-key ";" 'evil-ex evil-normal-state-map)
            (bind-key "\\" 'evil-repeat-find-char evil-normal-state-map)
            (bind-key "C-o" 'imenu evil-normal-state-map)
            (bind-key "!" 'shell-command evil-normal-state-map)
            (bind-key "C-r" 'isearch-backward evil-normal-state-map)
            (bind-key "(" 'beginning-of-defun evil-normal-state-map)
            (bind-key ")" 'end-of-defun evil-normal-state-map)
            (bind-key "i i" 'evil-insert evil-visual-state-map)
            (bind-key "|" 'split-window-right evil-normal-state-map)
            (bind-key "-" 'split-window-below evil-normal-state-map)
            (bind-key "M-." nil evil-normal-state-map)
            (bind-key "M-," nil evil-normal-state-map)
            (bind-key "C-j" 'evil-esc evil-insert-state-map))
  :init (progn
          (add-hook 'eshell-mode-hook 'turn-off-evil-mode) ; evil-emacs-state-modes ain't working for eshell!
          (evil-mode t)))

(use-package multiple-cursors
  :ensure t
  :init (progn
          (bind-key "C-d" 'mc/mark-more-like-this-extended evil-normal-state-map)
          (bind-key "C-d" 'mc/mark-more-like-this-extended evil-visual-state-map)))

(use-package elpy
  :ensure t
  :config (progn
            (elpy-enable)
            (elpy-use-ipython))
  :init (progn
          ;; disable flymake mode for python
          (setq elpy-modules (remove 'elpy-module-flymake elpy-modules))
          (bind-key "C-c d" 'elpy-doc elpy-mode-map))
  )

(defun elpy-show-defun (copy-to-clipboard)
  "Show the current class and method, in case they are not on
screen."
  (interactive "P")
  (let ((function (python-info-current-defun))
        (copy-or-message (if copy-to-clipboard
                             (lambda (format-string &rest objects)
                               (kill-new (format format-string objects))
                               (message "copied to clipboard"))
                           'message)))
    (if function
        (funcall copy-or-message function)
      (funcall copy-or-message "Not in a function"))))



(use-package color-theme
  :ensure t)

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config (load-theme 'sanityinc-tomorrow-night t))

(use-package color-theme-monokai
  :ensure t
  :config (progn
            (color-theme-monokai)))

;; (use-package sunburn
;;   :load-path "~/.emacs.d/"
;;   :config (color-theme-sunburn))

(use-package haskell-mode
  :ensure t
  :init (progn
          (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
          (setq haskell-program-name "ghci")
          (setq haskell-tags-on-save t)
          (use-package ghc
            :ensure t
            :init (progn
                    (add-hook 'haskell-mode-hook 'ghc-init)))))

(use-package shm
  :load-path "~/.emacs.d/structured-haskell-mode/elisp"
  :config (progn
            (add-hook 'haskell-mode-hook 'structured-haskell-mode))
  :init (progn
          (bind-key "," 'self-insert-command evil-insert-state-map)
          (bind-key "C-M-k" 'windmove-up shm-map)))

(use-package tuareg
  :ensure t
  :init (progn
          (add-to-list 'evil-emacs-state-modes 'tuareg-interactive-mode)
          (bind-key "C-M-h" nil tuareg-mode-map)))

(use-package evil-numbers
  :ensure t
  :init (progn
          (bind-key "C-x C-a" 'evil-numbers/inc-at-pt evil-normal-state-map)
          (bind-key "C-x C-x" 'evil-numbers/dec-at-pt evil-normal-state-map)))

(use-package ace-jump-mode
  :ensure t
  :init (progn
          (setq ace-jump-mode-scope 'frame))
  :config (progn
            (require 'cl)
            (setq ace-jump-mode-move-keys
                  (loop for i from ?a to ?z collect i))))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init (progn
          (add-hook 'lisp-mode-hook 'paredit-mode)
          (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  :config (progn))

(use-package grizzl
  :ensure t)

;; note: rewrite to something specifically for switching virtualenvs
(defvar projectile-after-switch-project-actions
  `((,(expand-file-name "~/src/ka-lite/") . ,(lambda ()
                                               (pyvenv-workon "ka-lite")
                                               (message "switched virtualenv to kalite")))
    (,(expand-file-name "~/src/sipper/") . ,(lambda ()
                                              (pyvenv-workon "sipper")
                                              (message "switched virtualenv to sipper")))
    (,(expand-file-name "~/src/snippets/") . ,(lambda ()
                                                (pyvenv-workon "snippets")
                                                (message "switched virtualenv to snippets"))))
  "Functions to run when we switch after we switch to a different project")

(defun projectile-run-action-for-current-project ()
  (let* ((current-project (projectile-project-root))
         (current-project-action (cdr (assoc-string current-project projectile-after-switch-project-actions))))
    (if current-project-action
        (funcall current-project-action)
      (message "No action found for %s" current-project))))

(use-package perspective
  :ensure t
  :init (progn
          (persp-mode t)))

(use-package projectile
  :load-path "~/.emacs.d/projectile"
  :diminish projectile-mode
  :config (progn
            (setq projectile-remember-window-configs t)
            (setq projectile-completion-system 'grizzl)
            (setq projectile-switch-project-action 'projectile-dired)
            (add-hook 'projectile-switch-project-hook 'projectile-run-action-for-current-project)
            (projectile-global-mode t))
  :init (progn
          (bind-key "C-M-r" 'projectile-find-tag evil-normal-state-map)
          (require 'persp-projectile)
          (projectile-global-mode t)))

(use-package ido-vertical-mode
  :ensure t
  :init (progn
          (ido-vertical-mode)))

(use-package flx-ido
  :ensure t
  :config (progn
            (setq gc-cons-threshold 20000000)
            (setq flx-ido-threshold 1000)))

(setq exclude-aggressive-indent-from-these-major-modes '(python-mode))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
                                        (when (not (member major-mode
                                                           exclude-aggressive-indent-from-these-major-modes))
                                          (aggressive-indent-mode t))))))

(use-package helm
  :load-path "~/.emacs.d/helm"
  :diminish helm-mode
  :init (progn
          (setq helm-ff-transformer-show-only-basename nil)
          (bind-key "M-x" 'helm-M-x)
          (require 'helm-mode)
          (helm-mode t)))

(use-package helm-cmd-t
  :load-path "~/.emacs.d/helm-cmd-t"
  :diminish projectile-mode
  :init (progn
          (bind-key "C-p" 'projectile-find-file evil-normal-state-map)
          (bind-key "M-P" (lambda ()
                            (interactive)
                            (with-helm-default-directory (projectile-project-root)
                                (helm-cmd-t-git-grep (current-buffer) ""))))))

(use-package github-browse-file
  :ensure t
  :init (progn
          (bind-key "C-M-g" 'github-browse-file)))

(use-package proced
  :config (progn
            (bind-key "C-x p" 'proced)))

(use-package multi-term
  :ensure t
  :config (progn
            (use-package visor
             :load-path "~/.emacs.d/"
             :config (progn
                       (bind-key "C-x C-x" 'toggle-project-shell-workspace)))

            (defun spawn-new-term (term-num)
              (let ((shell-name (projectile-prepend-project-name (number-to-string term-num))))
                (with-helm-default-directory (projectile-project-root)
                    (delete-other-windows)
                  (if (get-buffer shell-name)
                      (switch-to-buffer shell-name)
                    (progn
                      (multi-term)
                      (rename-buffer shell-name))))))
            (setq term-spawn-or-switch-function 'spawn-new-term)

            (add-to-list 'evil-emacs-state-modes 'term-mode)
            (evil-define-key 'insert term-mode-map "C-r" 'term-send-reverse-search-history)
            ;; (add-hook 'evil-local-mode-hook (lambda ()
            ;;                                   (if (eq major-mode 'term-mode)
            ;;                                       (turn-off-evil-mode))))

            (add-to-list 'term-bind-key-alist '("M-1" . (lambda () (interactive) (spawn-new-term 1))))
            (add-to-list 'term-bind-key-alist '("M-2" . (lambda () (interactive) (spawn-new-term 2))))
            (add-to-list 'term-bind-key-alist '("M-3" . (lambda () (interactive) (spawn-new-term 3))))
            (add-to-list 'term-bind-key-alist '("M-4" . (lambda () (interactive) (spawn-new-term 4))))

            ;; make C-r search backward
            ;; delete the original keybinding first so there will be no conflicts
            (setq term-bind-key-alist (delete-if (lambda (elem)
                                                   (equal "C-r" (first elem)))
                                                 term-bind-key-alist))
            (add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))))

;; (use-package eshell
;;   :init (progn
;;           (require 'em-smart)
;;           (setq eshell-where-to-jump 'begin)
;;           (setq eshell-review-quick-commands nil)
;;           (setq eshell-smart-space-goes-to-end t))
;;   :config (progn

;;            (use-package visor
;;              :load-path "~/.emacs.d/")



;;             (bind-key "C-x C-x" 'toggle-project-shell-workspace)

;;             ;; apparently we have to do this inside eshell-mode-hook since
;;             ;; eshell-mode-map is only initialized once we enter eshell mode,
;;             ;; and it's a local variable at that.
;;             (add-hook 'eshell-mode-hook (lambda ()
;;                                           ;; don't make eshell clobber our window movement keybindings
;;                                           (bind-key "C-M-l" 'windmove-right eshell-mode-map)
;;                                           (bind-key "C-M-h" 'windmove-left eshell-mode-map)
;;                                           (bind-key "C-M-k" 'windmove-up eshell-mode-map)
;;                                           (bind-key "C-M-j" 'windmove-down eshell-mode-map)

;;                                           ;; delete word by pressing C-w
;;                                           (bind-key "C-w" 'backward-kill-word eshell-mode-map)

;;                                           ;; bindings for switching to different eshells ala tabs in real terminal emulators
;;                                           (bind-key "M-1" (lambda () (interactive) (spawn-or-switch-to-old-eshell 1)) eshell-mode-map)
;;                                           (bind-key "M-2" (lambda () (interactive) (spawn-or-switch-to-old-eshell 2)) eshell-mode-map)
;;                                           (bind-key "M-3" (lambda () (interactive) (spawn-or-switch-to-old-eshell 3)) eshell-mode-map)
;;                                           (bind-key "M-4" (lambda () (interactive) (spawn-or-switch-to-old-eshell 4)) eshell-mode-map)

;;                                           ;; add it in here so it actually gets done by the time we load eshell
;;                                           (eshell-smart-initialize)))))

(use-package evil-leader
  :ensure t
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
            "|" 'split-window-right
            "-" 'split-window-below
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
  :ensure t
  :diminish magit-auto-revert-mode
  :config (progn
            (add-to-list 'evil-emacs-state-modes 'magit-process-mode))
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
            (jump-to-register :magit-fullscreen)))
  :config (progn
            (bind-key "C-p" 'magit-pr/open-pull-request magit-status-mode-map)
            (bind-key "<f1>" 'magit-status)))

(defvar github-ids-contributed-to '("aronasorman" "learningequality"))
(defun magit-pr/url (compare-repo)
  (let ((current-branch (first (vc-git-branches)))
        (repo-name  (helm-basename (magit-get-top-dir))))
    (format "https://github.com/aronasorman/%s/compare/%s:master...%s" repo-name compare-repo current-branch)))

(defun magit-pr/ask-id-to-compare-against ()
  (interactive)
  (completing-read "ID to contribute to: "
                   github-ids-contributed-to
                   nil
                   'confirm))

(defun magit-pr/open-pull-request ()
  (interactive)
  (let ((user-id (magit-pr/ask-id-to-compare-against)))
    (browse-url (magit-pr/url user-id))))

(add-hook 'magit-checkout-command-hook
          (lambda (ignore)
            (projectile-maybe-invalidate-cache t)
            nil ; we have to return nil for the checkout to continue on
            ))

(use-package dired
  :config (progn
            (add-to-list 'evil-emacs-state-modes 'dired-mode)
            (bind-key "," 'magit-status dired-mode-map)
            (bind-key ";" 'projectile-find-file dired-mode-map)))

(use-package slime
  :load-path "~/.emacs.d/slime"
  :init (progn
          (setq inferior-lisp-program "sbcl"))
  :config (progn
            (use-package slime-autoloads)))

(use-package smartparens
  :load-path "~/.emacs.d/smartparens"
  :init (progn
          (use-package smartparens-config)
          (smartparens-global-strict-mode 1)
          (show-smartparens-global-mode 1))
  :config (progn

            (defun wrap-with-pair (rewrap)
              "Wrap the current sexp inside a pair. If rewrap is
               true, change the current pair's rewrapping instead"
              (interactive "P")
              (if rewrap
                  (call-interactively 'sp-rewrap-sexp)
                (let ((pair (string (read-char "wrap with: "))))
                  (sp-wrap-with-pair pair))))

            (defun transpose-depending-on-mode (force-normal-sexp)
              (interactive "P")
              (let ((lisp-modes '(emacs-lisp-mode lisp-mode clojure-mode)))
                (if (or force-normal-sexp (member major-mode lisp-modes))
                    (sp-transpose-sexp)
                  (sp-transpose-hybrid-sexp))))

            ;; make the bindings I will use most often explicit
            (bind-key "C-M-w" 'wrap-with-pair evil-normal-state-map)
            (bind-key "C-M-w" 'wrap-with-pair evil-insert-state-map)
            (bind-key "C-M-y" 'transpose-depending-on-mode evil-normal-state-map)
            (bind-key "C-<right>" 'sp-forward-slurp-sexp evil-normal-state-map)
            (bind-key "C-<right>" 'sp-forward-slurp-sexp evil-insert-state-map)
            (bind-key "C-<left>" 'sp-backward-slurp-sexp evil-normal-state-map)
            (bind-key "C-<left>" 'sp-backward-slurp-sexp evil-insert-state-map)
            (bind-key "]]" 'sp-forward-barf-sexp evil-normal-state-map)
            (bind-key "[[" 'sp-backward-barf-sexp evil-normal-state-map)
            (bind-key "C-S-k" 'sp-kill-hybrid-sexp evil-normal-state-map)
            (bind-key "C-M-\\" 'sp-splice-sexp evil-normal-state-map)
            (bind-key "C-M-=" 'sp-indent-defun evil-normal-state-map)
            (bind-key "M-(" 'sp-backward-sexp evil-normal-state-map)
            (bind-key "M-)" 'sp-forward-sexp evil-normal-state-map)
            (bind-key "C-)" 'sp-splice-sexp-killing-forward evil-normal-state-map)
            (bind-key "C-(" 'sp-splice-sexp-killing-backward evil-normal-state-map)

            ;; turn off some bindings that conflict with how we switch windows
            (bind-key "C-M-k" nil sp-keymap)

            ;; some pairs for django templates
            (sp-local-pair 'web-mode "{" nil :actions nil)
	    (sp-local-pair 'web-mode "{%" "%}")))

(use-package elixir-mode
  :load-path "~/.emacs.d/elixir-mode"
  :init (progn
          (use-package alchemist
            :ensure t)))

(use-package lua-mode
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :config (progn
            (bind-key "M-y" 'browse-kill-ring)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config (yas/reload-all))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (add-to-list 'evil-emacs-state-modes 'undo-tree-mode))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode)

(use-package deft
  :ensure t
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


(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode))

(use-package linum-relative
  :load-path "~/.emacs.d/linum-relative")

;; config for org mode
;;;; require custom org file
(use-package org
  :ensure t
  :load-path ("~/.emacs.d/org/lisp"  "~/.emacs.d/org/contrib/lisp")
  :init (progn
          (setq org-startup-indented t)
          (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAITING(w@)" "|" "DONE(d!)" "CANCELED(c@)")))
          (setq org-log-done 'note)
          (setq org-log-into-drawer t)
          (setq org-default-notes-file "~/notes/capture.org")
          (add-to-list 'org-export-backends 'md)
          (setq org-html-doctype "html5")
          (setq org-html-html5-fancy t)
          (setq org-capture-templates `(("f" "For all things FLE related" entry
                                         (file "~/notes/todo/fle.org")
                                         ;; "\n* TODO %? \nDEADLINE: %(org-time-stamp nil)\n"
                                         "\n* TODO %? \n"
                                         )

                                        ("p" "Personal stuff" entry
                                         (file "~/notes/todo/personal.org")
                                         "\n* TODO %? \n")

                                        ;; activate again if we ever need timesheet reports
                                        ;; ("r" "Timesheet reports for time spent per task")
                                        ;; ("rd" "Generate daily report" entry
                                        ;;  (file+headline "~/notes/reports.org" "daily")
                                        ;;  "* %<%Y-%m-%d> %?\n %(insert-clock-table-summary 'today)"
                                        ;;  :prepend t)
                                        ;; ("rw" "Generate weekly report" entry
                                        ;;  (file+headline "~/notes/reports.org" "weekly")
                                        ;;  "* %<%Y-W%V> %?\n %(insert-clock-table-summary 'thisweek)"
                                        ;;  :prepend t)
                                        ))
          ))

;; set shortcuts for evil mode
(global-set-key (kbd "C-c C-l") 'org-store-link)
(global-set-key (kbd "C-c C-a") 'org-capture)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-l") 'org-clock-in-last)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(evil-define-key 'normal org-mode-map
  "\\]" 'org-shiftmetaright
  "\\[" 'org-shiftmetaleft
  "\\t" 'org-todo
  "\\r" 'org-priority
  "\\l" 'org-insert-link
  "\\bt" 'org-babel-tangle
  "\\t" 'org-set-tags-command)

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
(add-to-list 'evil-emacs-state-modes 'ledger-report-mode)

(use-package flycheck
  :ensure t
  :init (progn
          (use-package flycheck-haskell
            :ensure t)
          (setq flycheck-idle-change-delay 1.5)
          (add-hook 'prog-mode-hook 'flycheck-mode)
          (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)))

(use-package go-mode
  :ensure t
  :config (progn
            (require 'go-mode-autoloads)))

;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :init (progn
          (setq web-mode-code-indent-offset 4)
          (setq web-mode-markup-indent-offset 4)
          (add-hook 'web-mode-hook 'turn-off-smartparens-mode)
          (setq web-mode-engines-alist
                '(("django" . "\\.html$")))))

;; yaml mode
(use-package yaml-mode
  :ensure t)

;; rust mode
(use-package rust-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config (progn
            (use-package clojure-test-mode
              :ensure t)
            (use-package cider
              :ensure t
              :init (progn
                      (add-to-list 'evil-emacs-state-modes 'cider-test-report-mode)
                      (add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
                      (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
                      (cider-repl-toggle-pretty-printing)))))

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

(add-hook 'prog-mode-hook 'linum-mode) ;; avoid loading global-linum-mode now
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" default)))
 '(evil-show-paren-range 3)
 '(fci-rule-color "#efefef")
 '(midnight-mode t nil (midnight))
 '(org-agenda-files (quote ("~/notes/todo/fle.org")))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-html-table-default-attributes (quote (:border "2" :cellspacing "5" :cellpadding "6" :rules "groups" :frame "hsides")))
 '(org-modules (quote (org-bbdb org-bibtex org-crypt org-docview org-gnus org-habit org-id org-info org-inlinetask org-invoice org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-invoice)))
 '(safe-local-variable-values (quote ((ledger-master-file . "transactions.ldgr") (major-mode quote ledger-mode) (major-mode . ledger-mode))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(sp-navigate-close-if-unbalanced t)
 '(sp-show-pair-from-inside t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#c82829") (40 . "#f5871f") (60 . "#eab700") (80 . "#718c00") (100 . "#3e999f") (120 . "#4271ae") (140 . "#8959a8") (160 . "#c82829") (180 . "#f5871f") (200 . "#eab700") (220 . "#718c00") (240 . "#3e999f") (260 . "#4271ae") (280 . "#8959a8") (300 . "#c82829") (320 . "#f5871f") (340 . "#eab700") (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 4 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
