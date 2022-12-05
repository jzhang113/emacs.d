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

(elpaca-use-package god-mode
  :bind  (("M-[ 3 2 ~" . god-local-mode)
          ("<f18>" . god-local-mode)
          :map god-local-mode-map
          ("q" . bury-buffer)        ;; leave this buffer via 'q'uit
          ("Q" . kill-this-buffer)   ;; the other 'Q'uit
          ("i" . incarnate)          ;; temporarily leave god-mode via 'i'nsert
          :map isearch-mode-map
          ("<escape>" . god-mode-isearch-activate)
          :map god-mode-isearch-map
          ("<escape>" . god-mode-isearch-disable))
  :init
  (jhz/store-prev-faces)
  (setq god-mode-enable-function-key-translation nil)
  (god-mode-all)
  :hook (post-command . jhz/god-mode-update-mode-line)
  :config
  (require 'god-mode-isearch)

  ;; Overwrite the lookup method to recursively check for keybinds
  ;; For example, typing 'h b' with first check C-h C-b, followed by C-h b
  ;; https://www.reddit.com/r/emacs/comments/dnrpp3/comment/f5gq047/
  (defun god-mode-lookup-command (key-string)
    "Execute extended keymaps such as C-c, or if it is a command, call it."
    (let* ((key-vector (read-kbd-macro key-string t))
           (binding (key-binding key-vector)))
      (cond ((commandp binding)
             (setq last-command-event (aref key-vector (- (length key-vector) 1)))
             binding)
            ((keymapp binding)
             (god-mode-lookup-key-sequence nil key-string))
            (:else
             (let* ((key-string-list (s-split " " key-string))
                    (key-last (nth 0 (last key-string-list))))
               (if (string-match "^C\\-" key-last)
                   (god-mode-lookup-command (s-join " " (append (nbutlast key-string-list) (list (substring key-last 2)))))
                 (error "God: Unknown key binding for `%s`" key-string))))))))

(defun jhz/god-mode-update-mode-line ()
  "Set the color of the modeline depending on if we are in God-mode, Incarnate-mode, or neither."
  (cond ((bound-and-true-p god-local-mode)
         (set-face-attribute 'mode-line nil
                             :foreground jhz/prev-modeline-fg
                             :background "#a8680d")
         (set-face-attribute 'mode-line-inactive nil
                             :foreground jhz/prev-modeline-fgi
                             :background "#52110c"))
        ((bound-and-true-p incarnate-mode)
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
  "As normal but toggle God mode on RET."
  :lighter " God-Inc"
  :keymap `((,(kbd "<return>") . unincarnate)
            (,(kbd "RET") . unincarnate)))

(provide 'init-god)
;;; init-god.el ends here
