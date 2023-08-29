;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(global-reveal-mode)

(elpaca-use-package orderless
  :ensure t
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

(elpaca-use-package corfu
  :after orderless
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  ;; aggressive orderless autocompletion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (completion-styles '(orderless-fast))
  :bind (:map corfu-map
              ("\\" . corfu-insert-separator))
  :hook ((eshell-mode . (lambda () (setq-local corfu-auto nil))))
  :init  (global-corfu-mode)
  :config
  (xcond
   ((display-graphic-p)
    (corfu-popupinfo-mode))
   (t
    ((corfu-echo-mode)
     (setq corfu-echo-delay t)))))

(elpaca-use-package (corfu-terminal :host "codeberg.org" :repo "akib/emacs-corfu-terminal")
  :unless (display-graphic-p)
  :after corfu
  :config (corfu-terminal-mode))

(elpaca-use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'init-corfu)
;;; init-corfu.el ends here
