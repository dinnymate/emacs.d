(setq inhibit-startup-message t)
(setq custom-file (concat user-emacs-directory "/custom.el"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(line-number-mode t)
(tooltip-mode -1)
(mouse-wheel-mode -1)
(global-display-line-numbers-mode t)
(setq-default cursor-type 'bar)

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
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'dracula-theme)
(load-theme 'dracula t)

(straight-use-package 'company)


(global-company-mode t)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-mode-map (kbd "<C-tab>") 'company-complete)
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 0)

(straight-use-package 'ido)
(straight-use-package 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode t)
(ido-everywhere t)

(straight-use-package 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)

(straight-use-package 'projectile)
(projectile-mode t)

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
