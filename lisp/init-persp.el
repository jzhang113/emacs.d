;;; init-persp.el --- Configurations for perspective.el -*- lexical-binding: t -*-
;;; Commentary:

;; Load after init-minibuffer.el

;;; Code:

(elpaca-use-package perspective
  :bind (:map persp-mode-map
              ("C-x C-b" . persp-ibuffer))
  :custom
  (persp-mode-prefix-key (kbd "C-z"))
  :config
  ;; Ibuffer integration
  (require 'ibuffer)
  (add-hook 'ibuffer-hook
            (lambda ()
              (persp-ibuffer-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  ;; Consult integration
  (require 'consult)
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)

  (persp-mode))

(provide 'init-persp)
;;; init-persp.el ends here
