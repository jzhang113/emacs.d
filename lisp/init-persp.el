;;; init-persp.el --- Configurations for perspective.el -*- lexical-binding: t -*-
;;; Commentary:

;; Load after init-minibuffer.el

;;; Code:

(defvar jhz/persp-default-file "~/.emacs.d/.persp-state")

(defun emacsclient-startup ()
  "List of commands to run when starting a new emacsclient session."
  (persp-state-load jhz/persp-default-file)
  (delete-frame)
  (toggle-frame-fullscreen))

(defun jhz/persp-state-save (&args)
  "Save current perspective state to the default file without prompting."
  (persp-state-save))

(use-package perspective
  :bind (:map persp-mode-map
              ("C-x C-b" . persp-ibuffer))
  :custom
  (persp-mode-prefix-key (kbd "C-z"))
  (persp-state-default-file jhz/persp-default-file)
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

  (advice-add 'save-buffers-kill-terminal :before #'jhz/persp-state-save)

  (persp-mode))

(provide 'init-persp)
;;; init-persp.el ends here
