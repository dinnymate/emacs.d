;;; -*- lexical-binding: t; -*-

;;(setq straight-recipes-emacsmirror-use-mirror t)
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq use-package-verbose t)

(setq inhibit-startup-message t)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq-default eldoc-echo-area-use-multiline-p t)


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(line-number-mode -1)
(tooltip-mode -1)
(mouse-wheel-mode t)
(global-display-line-numbers-mode t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(delete-selection-mode 1)

(setq-default cursor-type 'bar)
(auto-save-mode -1)
(setq make-backup-files nil)
(setq-default lisp-indent-function 'common-lisp-indent-function)
(setq help-window-select t)
(setq compilation-read-command nil)

(put 'dired-find-alternate-file 'disabled nil)
(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "^")
     (lambda () (interactive) (find-alternate-file "..")))
   (define-key dired-mode-map (kbd "RET")
     'dired-find-alternate-file)))

(setq show-paren-delay 0)
(add-hook 'prog-mode-hook 'show-paren-mode)

(put 'projectile-project-run-cmd 'safe-local-variable (lambda (_) t))
(put 'projectile-project-configure-cmd 'safe-local-variable (lambda (_) t))
(put 'projectile-project-compile-cmd 'safe-local-variable (lambda (_) t))
(put 'cider-clojure-cli-aliases 'safe-local-variable (lambda (_) t))

(advice-add 'yes-or-no-p :override
	    (lambda (prompt)
	      (let* ((yes-or-no-prompt (concat prompt " "))
		     (choices '("yes" "no"))
		     (answer (completing-read yes-or-no-prompt choices nil t)))
		(while (string= answer "")
		  (setq answer (completing-read
				yes-or-no-prompt
				choices nil 'require-match)))
		(string= answer "yes"))))

(use-package all-the-icons)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (with-eval-after-load 'treemacs
    (require 'all-the-icons nil t)
    (when (featurep 'all-the-icons)
      (setq doom-themes-treemacs-theme "doom-colors")
      (doom-themes-treemacs-config))))

(use-package doom-modeline
  :init
  (setq doom-modeline-project-detection 'projectile)
  (doom-modeline-mode 1))

(use-package projectile
    :after treemacs
    :commands projectile-global-mode
    :bind-keymap
    ("C-x p" . projectile-command-map)
    :init
    (require 'seq)
    (let ((treemacs-root-p
	   (lambda (file)
	     (seq-first
	      (seq-filter
	       (lambda (project)
		 (file-in-directory-p file project))
	       (mapcar 'treemacs-project->path
		       (treemacs-workspace->projects
			(treemacs-current-workspace))))))))
      (setq-default projectile-project-root-files-functions
		    (list treemacs-root-p)))

    (setq projectile-auto-discover nil)
    (setq projectile-track-known-projects-automatically nil)
  
    (let ((projectile-refresh-treemacs-projects
	   (lambda (&rest _args)
	     (projectile-global-mode -1)
	     (projectile-global-mode t)
	     (setq projectile-known-projects
		   (mapcar (lambda (p)
			     (file-name-as-directory
			      (abbreviate-file-name
			       (treemacs-project->path p))))
      			   (treemacs-workspace->projects
			    (treemacs-current-workspace)))))))
    
      (dolist (hook '(treemacs-create-project-functions
		      treemacs-delete-project-functions
		      treemacs-rename-project-functions
		      treemacs-switch-workspace-hook
		      treemacs-workspace-edit-hook))
	(add-hook hook projectile-refresh-treemacs-projects))

      (funcall projectile-refresh-treemacs-projects)))

(use-package magit
    :demand t
    :bind (("C-x g" . magit-status)))
 
(use-package treemacs
    :bind ("C-c b" . treemacs-select-window)
    :demand t
    :config
    (message "foo")
    (setq-default treemacs-is-never-other-window t)
    (add-hook 'treemacs-mode-hook
	      (lambda ()
		(display-line-numbers-mode -1)
		(add-hook 'post-command-hook
			  (lambda ()
			    (setq-local file-local-variables-alist nil)
			    (setq-local dir-local-variables-alist nil)
			    (hack-dir-local-variables)) nil t)))
    (set-face-attribute 'treemacs-root-face nil :height 110)
    (set-face-attribute 'treemacs-root-unreadable-face nil :height 110)
    (set-face-attribute 'treemacs-root-remote-face nil :height 110)
    (set-face-attribute 'treemacs-root-remote-unreadable-face nil :height 110)
    (set-face-attribute 'treemacs-root-remote-disconnected-face nil :height 110)
    (treemacs-resize-icons 22))

(use-package paredit
    :commands paredit-mode
    :hook ((emacs-lisp-mode
	    eval-expression-minibuffer-setup
	    ielm-mode
	    lisp-mode
	    lisp-interaction-mode
	    scheme-mode) . paredit-mode)
    :config
    (put 'paredit-forward-delete 'delete-selection 'supersede)
    (put 'paredit-backward-delete 'delete-selection 'supersede))

(use-package rainbow-delimiters
    :hook ((prog-mode
	    eval-expression-minibuffer-setup) . rainbow-delimiters-mode))

(use-package company
    :commands global-company-mode company-mode-hook
    :init
    (setq company-idle-delay 0.1)
    (setq company-begin-commands '(self-insert-command))
    (setq company-minimum-prefix-length 1)
    (global-company-mode t)
    :bind (:map company-active-map
		("C-n" . company-select-next)
		("C-p" . company-select-previous)
		:map company-mode-map
		("<C-tab>" . company-complete)))

(use-package selectrum
    :commands selectrum-mode
    :init (selectrum-mode t))

(use-package prescient)

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode))

(use-package company-prescient
  :init
  (company-prescient-mode))

(use-package marginalia
    :bind (:map minibuffer-local-map
		("M-A" . marginalia-cycle))
    :init
    (marginalia-mode)

    (advice-add #'marginalia-cycle :after
		(lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
    (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package ace-window
    :commands ace-window-display-mode
    :init
    (ace-window-display-mode t)
    :bind ("M-o" . ace-window))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-auto-guess-root t)
  :hook ((rust-mode . lsp-deferred))
  :commands lsp)

(use-package flycheck
  :commands flycheck)

(use-package clojure-mode
    :after paredit
    :commands clojure-mode clojure-mode-hook
    :config
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(use-package cider
    :after paredit rainbow-delimiters
    :commands cider-repl-mode-hook cider-mode-hook
    :config
    (setq cider-repl-display-help-banner nil)
    (setq clojure-toplevel-inside-comment-form t)
    (setq cider-font-lock-dynamically '(macro core function var))
    (setq cider-overlays-use-font-lock t)
    (setq cider-result-overlay-position 'at-point)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package rust-mode
  :commands rust-mode-hook
  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))


(use-package geiser
    :straight (:protocol https))

(use-package geiser-guile)
