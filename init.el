(setq inhibit-startup-message t)

(setq custom-file (concat user-emacs-directory "/custom.el"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(line-number-mode -1)
(tooltip-mode -1)
(mouse-wheel-mode t)
(global-display-line-numbers-mode t)
(setq-default cursor-type 'bar)
(auto-save-mode -1)
(setq make-backup-files nil)

(put 'dired-find-alternate-file 'disabled nil)

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "^")
     (lambda () (interactive) (find-alternate-file "..")))
   (define-key dired-mode-map (kbd "RET")
     'dired-find-alternate-file)))

(setq help-window-select t)

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

;; (straight-use-package 'dracula-theme)
;; (load-theme 'dracula t)

(straight-use-package 'company)
(straight-use-package 'company-box)

(global-company-mode t)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-mode-map (kbd "<C-tab>") 'company-complete)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
(setq-default lisp-indent-function 'common-lisp-indent-function)

(straight-use-package 'ido)
(straight-use-package 'ido-vertical-mode)
(straight-use-package 'ido-completing-read+)
(straight-use-package 'ido-yes-or-no)

(ido-mode t)
(ido-vertical-mode t)
(ido-everywhere t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)
(setq ido-enable-flex-matching nil)

(straight-use-package 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'magit-ido-completing-read)

(straight-use-package 'projectile)
(require 'projectile)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)


(straight-use-package 'hydra)

(defhydra hydra-windmove (:foreign-keys run
			  :hint nil)
  "Move to window:"
  ("<up>" windmove-up "Up")
  ("<down>" windmove-down "Down")
  ("<left>" windmove-left "Left")
  ("<right>" windmove-right "Right")
  ("j" windmove-left "Left")
  ("é" windmove-right "Right")
  ("k" windmove-up "Up")
  ("l" windmove-down  "Down")
  ("q" nil "Quit"))

(define-key global-map (kbd "C-c m") 'hydra-windmove/body)

(straight-use-package 'all-the-icons)

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'treemacs-magit)
(straight-use-package 'doom-themes)

(require 'treemacs)
(require 'treemacs-magit)

(define-key global-map (kbd "C-c b") 'treemacs-select-window)
(define-key global-map (kbd "C-c B") 'treemacs-projectile)
(treemacs-git-mode 'deferred)

(add-hook 'treemacs-mode-hook
	  (lambda ()
	    ;(setq doom-variable-pitch-font (font-spec :family "Noto Sans" :size 11))
	    ;(setq text-scale-mode-step 1.01)
	    ;(text-scale-decrease 1)
	    (display-line-numbers-mode -1)))

(treemacs-resize-icons 22)
(setq-default treemacs-is-never-other-window t)
(load-theme 'doom-one t)

(setq doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)

(require 'seq)

(defun projectile-root-treemacs (file)
  (seq-first (seq-filter (lambda (project) (file-in-directory-p file project))
	      (mapcar 'treemacs-project->path
		      (treemacs-workspace->projects
		       (treemacs-current-workspace))))))

(setq-default projectile-project-root-files-functions (list 'projectile-root-treemacs))


(defun treemacs-update-projectile (&rest _args)
  (projectile-global-mode -1)
  (projectile-global-mode t)
  (setq projectile-known-projects
	(mapcar (lambda (p)
		  (file-name-as-directory (abbreviate-file-name (treemacs-project->path p))))
      		(treemacs-workspace->projects
		 (treemacs-current-workspace)))))


(treemacs-update-projectile)
(setq projectile-auto-discover nil)
(setq projectile-track-known-projects-automatically nil)

(dolist (hook '(treemacs-create-project-functions
		treemacs-delete-project-functions
		treemacs-rename-project-functions
		treemacs-switch-workspace-hook
		treemacs-workspace-edit-hook))
  (add-hook hook #'treemacs-update-projectile))



(set-face-attribute 'treemacs-root-face nil :height 110)
(set-face-attribute 'treemacs-root-unreadable-face nil :height 110)
(set-face-attribute 'treemacs-root-remote-face nil :height 110)
(set-face-attribute 'treemacs-root-remote-unreadable-face nil :height 110)
(set-face-attribute 'treemacs-root-remote-disconnected-face nil :height 110)



(straight-use-package 'paredit)

(add-hook 'prog-mode-hook #'paredit-mode)

(straight-use-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; Language Modes


(straight-use-package 'clojure-mode)
(straight-use-package 'cider)

(setq clojure-toplevel-inside-comment-form t)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook (lambda ()
				  (display-line-numbers-mode -1)))

(put 'projectile-project-run-cmd 'safe-local-variable (lambda (_) t))
(put 'cider-clojure-cli-aliases 'safe-local-variable (lambda (_) t))
