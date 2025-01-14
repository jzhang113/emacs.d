;;; init-ruby.el --- Support for the Ruby language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Basic ruby setup
(require-package 'ruby-hash-syntax)

(add-auto-mode 'ruby-base-mode
               "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Kirkfile\\'")
(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")

(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-base-mode-hook 'subword-mode)

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'ruby-base-mode))

(require-package 'rspec-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((ruby-base-mode) "ruby-lsp")))

;; Ruby version manager
(use-package rvm
  :hook (ruby-base-mode . rvm-activate-corresponding-ruby))


(define-derived-mode brewfile-mode ruby-mode "Brewfile"
  "A major mode for Brewfiles, used by homebrew-bundle on MacOS.")

(add-auto-mode 'brewfile-mode "Brewfile\\'")


;;; Inferior ruby
(use-package inf-ruby
  :config
  (defun sanityinc/ruby-load-file (&optional choose-file)
    (interactive "P")
    (if (or choose-file (not buffer-file-name))
        (call-interactively 'ruby-load-file)
      (save-some-buffers)
      (ruby-load-file buffer-file-name)))
  (define-key inf-ruby-minor-mode-map [remap ruby-load-file] 'sanityinc/ruby-load-file))


;;; Ruby compilation
(require-package 'ruby-compilation)

(with-eval-after-load 'ruby-base-mode
  (define-key ruby-mode-map [S-f7] 'ruby-compilation-this-buffer)
  (define-key ruby-mode-map [f7] 'ruby-compilation-this-test))

(with-eval-after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))



;;; Robe
(when (maybe-require-package 'robe)
  (with-eval-after-load 'ruby-mode
    (add-hook 'ruby-base-mode-hook 'robe-mode)))



;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)



(require-package 'bundler)


(when (maybe-require-package 'yard-mode)
  (add-hook 'ruby-base-mode-hook 'yard-mode)
  (with-eval-after-load 'yard-mode
    (diminish 'yard-mode)))


;;; ERB
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))


;; Ruby - my convention for heredocs containing SQL

;; (require-package 'mmm-mode)
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;; (add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)



(provide 'init-ruby)
;;; init-ruby.el ends here
