;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary: Updated to use consult-projectile + consult-ag for navigation instead
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))

(use-package consult-projectile
  :after projectile
  :bind (:map projectile-command-map
              ("f" . consult-projectile)))

(use-package consult-ag
  :after projectile
  :bind (:map projectile-command-map
              ("s a" . consult-ag)))

(provide 'init-projectile)
;;; init-projectile.el ends here
