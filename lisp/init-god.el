;;; init-god.el --- God mode setup -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(when (maybe-require-package 'god-mode)
  (bind-key* "M-[ 3 2 ~" 'god-local-mode)
  (bind-key* "<f18>" 'god-local-mode)

  (add-hook 'post-command-hook 'jhz/god-mode-update-mode-line)

  (require 'god-mode)
  ;; god-mode rebindings
  ;; leave this buffer via 'q'
  (define-key god-local-mode-map (kbd "q") 'bury-buffer)

  ;; temporarily leave god-mode via 'i'nsert
  (define-key god-local-mode-map (kbd "i") 'incarnate)

  (god-mode))

(defun jhz/god-mode-update-mode-line ()
  (defvar jhz/fg (face-attribute 'mode-line :foreground))
  (defvar jhz/bg (face-attribute 'mode-line :background))
  (defvar jhz/fgi (face-attribute 'mode-line-inactive :foreground))
  (defvar jhz/bgi (face-attribute 'mode-line-inactive :background))
  (cond
   (god-local-mode
    (set-face-attribute 'mode-line nil
                        :foreground jhz/fg
                        :background "#a8680d")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground jhz/fgi
                        :background "#52110c"))
   (t
    (set-face-attribute 'mode-line nil
                        :foreground jhz/fg
                        :background jhz/bg)
    (set-face-attribute 'mode-line-inactive nil
                        :foreground jhz/fgi
                        :background jhz/bgi))))

(defun unincarnate ()
  (interactive)
  (incarnate-mode -1)
  (setq cursor-type 'box)
  (god-local-mode 1)
  (set-cursor-color "#b0381e"))

(defun incarnate ()
  (interactive)
  (when (bound-and-true-p god-local-mode)
    (god-local-mode 0)
    (unless (display-graphic-p)
      (set-cursor-color "black")
      (setq cursor-type 'bar)
      (incarnate-mode))))

(define-minor-mode incarnate-mode
  "As normal but toggle to God mode on RET"
  :lighter " God-Inc"
  :keymap '(("\r" . unincarnate)))

(provide 'init-god)
;;; init-god.el ends here
