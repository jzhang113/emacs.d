;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(global-reveal-mode)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  ;; (completion-cycle-threshold 4)
  :config
  ;; Behave like prefix completion for single short strings
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1)  (length< word 4)
	 `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp))))

(use-package corfu
  :elpaca (:host github :repo "minad/corfu" :files (:defaults "extensions/*.el"))
  :after orderless
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  ;; aggressive orderless autocompletion
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 3)
  ;; (completion-styles '(orderless-fast))
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("C-j" . corfu-recomplete))
  :hook ((eshell-mode . (lambda () (setq-local corfu-auto nil))))
  :init  (global-corfu-mode)
  :config
  (cond
   ((display-graphic-p)
    (progn
      (setq corfu-popupinfo-delay t)
      (corfu-popupinfo-mode)))
   (t
    (progn
      (setq corfu-echo-delay t)
      (corfu-echo-mode)))))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config (corfu-terminal-mode))

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(defun corfu-recomplete ()
  "Quit the current completion and retry with only the last term."
  (interactive)
  (corfu-quit)
  (completion-at-point))

(provide 'init-corfu)
;;; init-corfu.el ends here
