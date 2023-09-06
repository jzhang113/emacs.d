;;; init-puni.el --- Structured editing of parentheses and other tags -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package puni
  :elpaca (:host github :repo "AmaiKinono/puni")
  :hook ((term-mode calc-mode dired-mode ibuffer-mode vterm-mode html-mode) . puni-disable-puni-mode)
  :bind (:map puni-mode-map
              ("C-w" . puni-kill-region-or-backward-word)
              ("M-f" . puni-forward-sexp-or-syntactic-forward)
              ("M-b" . puni-backward-sexp-or-syntactic-backward)
              ("C-+" . puni-expand-region)
              ;; slurping & barfing
              ("C-S-o" . puni-slurp-backward)
              ("C-S-p" . puni-barf-backward)
              ("C-{" . puni-barf-forward)
              ("C-}" . puni-slurp-forward)
              ;; depth chaining
              ("C-(" . puni-wrap-round)
              ("C-)" . puni-splice)
              ("M-r" . puni-raise)
              ("M-<up>" . puni-splice-killing-backward)
              ("ESC <up>" . puni-splice-killing-backward)
              ("M-<down>" . puni-splice-killing-forward)
              ("ESC <down>" . puni-splice-killing-forward)
              ("M-s c" . puni-convolute)
              ("M-s s" . puni-split))
  :init
  (puni-global-mode))

(defun puni-kill-region-or-backward-word ()
  "Like jhz/kill-region-or-backward-word.
Kill region if there is one active, backward-kill word otherwise."
  (interactive)
  (if (region-active-p)
      (puni-kill-active-region)
    (puni-backward-kill-word 1)))

(defun puni-splice-killing-backward ()
  "Splice the list the point is on by removing its delimiters, and
also kill all S-expressions before the point in the current list."
  (interactive)
  (puni-soft-delete-by-move
   #'puni-beginning-of-list-around-point)
  (puni-splice))

(defun puni-splice-killing-forward ()
  "Splice the list the point is on by removing its delimiters, and
also kill all S-expressions after the point in the current list."
  (interactive)
  (puni-soft-delete-by-move
   #'puni-end-of-list-around-point)
  (puni-splice))

(defun puni-forward-sexp-or-syntactic-forward (&optional n)
  "Go forward a sexp.
This is the same as `puni-strict-forward-sexp', except that it
jumps forward consecutive single-line comments, and will go over
syntactic boundaries.
With prefix argument N, go forward that many sexps.  Negative
argument means go backward."
  (interactive "^p")
  (setq n (or n 1))
  (if (< n 0) (puni-backward-sexp-or-syntactic-backward (- n))
    (dotimes (_ n)
      (or (puni-strict-forward-sexp 'skip-single-line-comments)
          (puni-syntactic-forward-punct)))))

(defun puni-backward-sexp-or-syntactic-backward (&optional n)
  "Go backward a sexp.
This is the same as `puni-strict-backward-sexp', except that it
jumps backward consecutive single-line comments, and will go over
syntactic boundaries.
With prefix argument N, go backward that many sexps.  Negative
argument means go forward."
  (interactive "^p")
  (setq n (or n 1))
  (if (< n 0) (puni-forward-sexp (- n))
    (dotimes (_ n)
      (or (puni-strict-backward-sexp 'skip-single-line-comments)
          (puni-syntactic-backward-punct)))))

(autoload #'insert-parentheses "lisp")
(defun puni-wrap-round (&optional n)
  (interactive "P")
  (insert-parentheses (if n n 1)))

(provide 'init-puni)
;;; init-puni.el ends here
