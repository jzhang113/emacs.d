;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary: Updated to use consult-projectile + consult-ag for navigation instead
;;; Code:

(when (maybe-require-package 'projectile)
  ;; (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  ;; (with-eval-after-load 'projectile
  ;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))


(when (maybe-require-package 'consult-projectile)
  (defvar consult-projectile-mode-map nil "Keymap for projectile-esque commands")

  (progn
    (setq consult-projectile-mode-map (make-sparse-keymap))
    (define-key consult-projectile-mode-map (kbd "C-c p f") 'consult-projectile)
    (define-key consult-projectile-mode-map (kbd "C-c p s") 'consult-ag))

  (define-minor-mode consult-projectile-mode
    "Projectile-like navigation with Consult"
    t
    " P"
    consult-projectile-mode-map)

  (add-hook 'emacs-startup-hook 'consult-projectile-mode))


(provide 'init-projectile)
;;; init-projectile.el ends here
