;;; init-god.el --- God mode setup -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun jhz/store-prev-faces ()
  "Define jhz/prev-* variables for cursor and modeline."
  (defvar jhz/prev-cursor-color (face-attribute 'cursor :background))
  (defvar jhz/prev-cursor-type (symbol-value 'cursor-type))
  (defvar jhz/prev-modeline-fg (face-attribute 'mode-line :foreground))
  (defvar jhz/prev-modeline-bg (face-attribute 'mode-line :background))
  (defvar jhz/prev-modeline-fgi (face-attribute 'mode-line-inactive :foreground))
  (defvar jhz/prev-modeline-bgi (face-attribute 'mode-line-inactive :background)))

(when (maybe-require-package 'god-mode)
  (bind-key* "M-[ 3 2 ~" 'god-local-mode)
  (bind-key* "<f18>" 'god-local-mode)

  (jhz/store-prev-faces)
  (add-hook 'post-command-hook 'jhz/god-mode-update-mode-line)

  (require 'god-mode)
  ;; god-mode rebindings
  ;; leave this buffer via 'q'uit
  (define-key god-local-mode-map (kbd "q") 'bury-buffer)

  ;; temporarily leave god-mode via 'i'nsert
  (define-key god-local-mode-map (kbd "i") 'incarnate)

  (require 'god-mode-isearch)

  (god-mode))

(defun jhz/god-mode-update-mode-line ()
  "Set the color of the modeline depending on if we are in God-mode, Incarnate-mode, or neither."
  (cond
   (god-local-mode
    (set-face-attribute 'mode-line nil
                        :foreground jhz/prev-modeline-fg
                        :background "#a8680d")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground jhz/prev-modeline-fgi
                        :background "#52110c"))
   (incarnate-mode
    (set-face-attribute 'mode-line nil
                        :foreground jhz/prev-modeline-fg
                        :background "#0000ff")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground jhz/prev-modeline-fgi
                        :background "#00ffff"))
   (t
    (set-face-attribute 'mode-line nil
                        :foreground jhz/prev-modeline-fg
                        :background jhz/prev-modeline-bg)
    (set-face-attribute 'mode-line-inactive nil
                        :foreground jhz/prev-modeline-fgi
                        :background jhz/prev-modeline-bgi))))

(defun unincarnate ()
  "Return to God."
  (interactive)
  (incarnate-mode -1)
  (god-local-mode 1)
  (when (display-graphic-p)
    (setq cursor-type jhz/prev-cursor-type)
    (set-cursor-color jhz/prev-cursor-color)))

(defun incarnate ()
  "Leave God-mode temporarily."
  (interactive)
  (when (bound-and-true-p god-local-mode)
    (god-local-mode 0)
    (incarnate-mode)
    (when (display-graphic-p)
      (set-cursor-color "#ffff00")
      (setq cursor-type 'hbar))))

(define-minor-mode incarnate-mode
  "As normal but toggle to God mode on RET."
  :lighter " God-Inc"
  :keymap '(("\r" . unincarnate)))

(provide 'init-god)
;;; init-god.el ends here
