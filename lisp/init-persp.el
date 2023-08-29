;;; init-persp.el --- Configurations for perspective.el -*- lexical-binding: t -*-
;;; Commentary:

;; Load after init-minibuffer.el

;;; Code:

(when (maybe-require-package 'perspective)
  (customize-set-variable 'persp-mode-prefix-key (kbd "C-z"))

  ;; Ibuffer integration
  (bind-key (kbd "C-x C-b") 'persp-ibuffer)
  (add-hook 'ibuffer-hook
            (lambda ()
              (persp-ibuffer-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  ;; Consult integration
  (with-eval-after-load 'perspective
    (require 'consult)
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  (persp-mode))

(provide 'init-persp)
;;; init-persp.el ends here
