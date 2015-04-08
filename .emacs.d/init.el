;;; init.el -- my emacs initialization file
;;; Commentary:
;;; This is it bro.

;;; Code:
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
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

(defvar src-dir "~/src")

;; calendar format
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

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

(use-package company
  :ensure t
  :init (progn
          (add-hook 'after-init-hook 'global-company-mode)))

(use-package diminish
  :ensure t)

(use-package dtrace-script-mode
  :mode ("\\.d$" . dtrace-script-mode)
  :ensure t)

(use-package restclient
  :load-path "~/.emacs.d/"
  :mode ("\\.rest$" . restclient-mode)
  :init (progn
          (use-package json-reformat
            :ensure t)) )

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

(use-package evil
  :ensure t
  :config (progn
            (add-to-list 'evil-emacs-state-modes 'grep-mode)
            (add-to-list 'evil-emacs-state-modes 'eshell-mode)
            (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
            (add-to-list 'evil-emacs-state-modes 'sql-interactive-mode)
            (add-to-list 'evil-emacs-state-modes 'compilation-mode)
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
            (bind-key "C-j" 'evil-force-normal-state evil-insert-state-map))
  :init (progn
          (add-hook 'eshell-mode-hook 'turn-off-evil-mode) ; evil-emacs-state-modes ain't working for eshell!
          (evil-mode t)))

(use-package multiple-cursors
  :ensure t
  :init (progn
          (bind-key "C-d" 'mc/mark-more-like-this-extended evil-normal-state-map)
          (bind-key "C-d" 'mc/mark-more-like-this-extended evil-visual-state-map)))


(use-package color-theme
  :ensure t)

(use-package color-theme-monokai
  :ensure t
  :config (progn
            (color-theme-monokai)))

(use-package haskell-mode
  :ensure t
  :init (progn
          (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
          (setq haskell-program-name "ghci")
          (setq haskell-tags-on-save t)
          (use-package ghc
            :ensure t
            :init (progn
                    (add-hook 'haskell-mode-hook 'eldoc-mode)
                    (add-hook 'haskell-mode-hook (lambda ()
                                                   (aggressive-indent-mode -1)))
                    (add-hook 'haskell-mode-hook 'ghc-init)))))

(use-package tidal
  :load-path "~/.emacs.d"
  :config (progn
            (setq tidal-interpreter "/usr/local/bin/ghci")))

(use-package shm
  :load-path "~/.emacs.d/structured-haskell-mode/elisp"
  ;; :config (progn
  ;;           (add-hook 'haskell-mode-hook 'structured-haskell-mode))
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
          (bind-key "C-, C-a" 'evil-numbers/inc-at-pt evil-normal-state-map)
          (bind-key "C-, C-x" 'evil-numbers/dec-at-pt evil-normal-state-map)))

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
            (projectile-global-mode t))
  :init (progn
          (bind-key "C-M-r" 'projectile-find-tag evil-normal-state-map)
          (require 'persp-projectile)
          (projectile-global-mode t)))
(bind-key "C-/" 'projectile-switch-project-from-marks evil-normal-state-map)
(defun mark-dir ()
  (interactive)
  (let* ((name (or (buffer-file-name) dired-directory))
         (cwd (file-name-directory name))
         (basename (file-name-nondirectory (directory-file-name name))))
    (make-symbolic-link cwd (format "~/marks/%s" basename))))

(defun projectile-switch-project-from-marks ()
  "Switch projects based on the symbolic links in ~/marks."
  (interactive)
  (projectile-clear-known-projects)
  (let* ((markdir (expand-file-name "~/marks/"))
         (marks (directory-files markdir)))
    (mapcar (lambda (filename)
              (let ((long-filename (file-truename (concat markdir filename))))
                (projectile-add-known-project long-filename)))
            marks)
    (projectile-persp-switch-project)))

(use-package eshell
  :config (progn
            (bind-key "C-x C-x" 'toggle-project-eshell)
            (bind-key "C-'" 'magit-status)
            (bind-key "C-/" 'projectile-switch-project-from-marks eshell-mode-map)
            (add-hook 'eshell-mode-hook 'compilation-shell-minor-mode)))

(setq project-shell-mappings (make-hash-table :test 'equal))
(defun proj-name ()
  "custom function for deriving a unique name for a given project.
 Simply (projectile-project-root) for now."
  (projectile-project-root))

(defun initialize-eshell-for-project ()
  (interactive)
  (let* ((eshell-buffer-name (format "%s-eshell" (proj-name))))
    (puthash (proj-name) (current-buffer) project-shell-mappings)
    (eshell)
    ;; to make eshell work with virtualenvs, make sure we set the
    ;; eshell's path as buffer local, so it gets permanently set once
    ;; we create it. This assumes we've set the virtualenv once we start eshell.
    (make-local-variable 'eshell-path-env)
    (setq eshell-path-env (getenv "PATH"))))

(defun toggle-project-eshell ()
  (interactive)
  (let ((buf (gethash (proj-name) project-shell-mappings)))
    (if (not buf)
        (initialize-eshell-for-project)
      (progn
        (puthash (proj-name) (current-buffer) project-shell-mappings)
        (switch-to-buffer buf)))))
(bind-key "C-x C-x" 'toggle-project-eshell)

(use-package elpy
  :ensure t
  :config (progn
            (elpy-enable)
            (elpy-use-ipython))
  :init (progn
          ;; disable flymake mode for python
          (setq elpy-modules (remove 'elpy-module-flymake elpy-modules))
          (setq elpy-rpc-backend "jedi")
          (bind-key "M-," 'pop-tag-mark)
          (add-hook 'persp-switch-hook 'activate-virtualenv-for-project)
          (bind-key "C-c d" 'elpy-doc elpy-mode-map)))
(defalias 'workon 'pyvenv-workon)
(defun activate-virtualenv-for-project ()
  (interactive)
  (let ((target-virtualenv (file-name-nondirectory (directory-file-name (projectile-project-root)))))
    (if (member target-virtualenv (pyvenv-virtualenv-list))
        (progn (pyvenv-workon target-virtualenv)
               (message "autoswitched virtualenv to %s." target-virtualenv))
      (progn
        (pyvenv-deactivate)
        (message "turned off virtualenv.")))))

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
  :init (progn
          (bind-key "C-p" 'magit-pr/open-pull-request magit-status-mode-map)))

(bind-key "C-'" 'magit-status evil-normal-state-map) ;; somehow i need to put this outside so it will bite

(use-package dired
  :config (progn
            (add-to-list 'evil-emacs-state-modes 'dired-mode)
            (setq dired-dwim-target t)
            (bind-key "C-'" 'magit-status dired-mode-map)
            (bind-key "C-p" 'projectile-find-file dired-mode-map)
            (bind-key "|" 'split-window-right dired-mode-map)
            (bind-key "-" 'split-window-below dired-mode-map)
            (bind-key "C-/" 'projectile-switch-project-from-marks dired-mode-map)))
(bind-key "C-/" 'projectile-switch-project-from-marks dired-mode-map)

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
  :config (progn
            (add-to-list 'evil-emacs-state-modes 'undo-tree-visualizer-mode)
            (bind-key "C-x u" 'undo-tree-visualize)
            (add-hook 'dired-mode-hook (lambda ()
                                         (undo-tree-mode -1)))
            (bind-key "C-/" nil undo-tree-map)))

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
          (bind-key "C-+" (lambda () (interactive) (find-file-other-window "~/notes/todo/planning.org")))
          (setq org-capture-templates `(("t" "Day planning" entry
                                         (file+datetree+prompt "~/notes/todo/planning.org")
                                         "\n* TODO %? %(org-set-tags-command)\n %(org-time-stamp t)\n"
                                         )

                                        ;; activate again if we ever need timesheet reports
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

(use-package markdown-mode
  :ensure t)

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
            (require 'go-mode-autoloads)
            (bind-key "C-c d" 'godoc go-mode-map)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (bind-key "M-." 'godef-jump go-mode-map)
            (bind-key "M-," 'pop-tag-mark go-mode-map)
            (load-file "~/.emacs.d/go-rename.el")))

(use-package company-go
  :ensure t
  :config (add-hook 'go-mode-hook (lambda ()
                                    (set (make-local-variable 'company-backends) '(company-go))
                                    (company-mode-on))))

;; go's oracle integration
(use-package go-oracle
  :pre-load (load-file "~/.emacs.d/oracle.el")
  :config (progn
            (setq go-oracle-command "~/bin/oracle")
            (add-hook 'go-mode-hook (lambda () (go-oracle-mode t)))))

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

(use-package markdown-mode
  :ensure t)

;; yaml mode
(use-package yaml-mode
  :ensure t)

;; rust mode
(use-package rust-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config (progn
            ;; (use-package clojure-test-mode
            ;;   :ensure t)
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" default)))
 '(evil-show-paren-range 3)
 '(fci-rule-color "#efefef")
 '(midnight-mode t nil (midnight))
 '(org-agenda-files
   (quote
    ("~/notes/todo/planning.org" "~/notes/todo/fle.org")))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-html-table-default-attributes
   (quote
    (:border "2" :cellspacing "5" :cellpadding "6" :rules "groups" :frame "hsides")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-docview org-gnus org-habit org-id org-info org-inlinetask org-invoice org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-invoice)))
 '(safe-local-variable-values
   (quote
    ((ledger-master-file . "transactions.ldgr")
     (major-mode quote ledger-mode)
     (major-mode . ledger-mode))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(sp-navigate-close-if-unbalanced t)
 '(sp-show-pair-from-inside t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 4 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)

(provide 'init)
;;; init.el ends here
