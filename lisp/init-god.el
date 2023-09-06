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

(use-package god-mode
  :bind  (("M-[ 3 2 ~" . god-mode-all)
          ("<f18>" . god-mode-all)
          ("C-x C-0" . delete-window) ;; Match the behavior of "x 1", "x 2", "x 3"
          :map god-local-mode-map
          ("q" . bury-buffer)         ;; leave this buffer via 'q'uit
          ("Q" . kill-this-buffer)    ;; the other 'Q'uit
          ("o" . god-single-insert)   ;; insert a few characters
          ("i" . incarnate)           ;; temporarily leave god-mode via 'i'nsert
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
  (add-to-list 'god-exempt-major-modes 'vterm-mode)

  ;; Overwrite the lookup method to also check for keybinds without a C- prefix on the last term
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
            ((not (cl-search " " key-string))
             (error "God: unknown keybinding for `%s`" key-string))
            (:else
             (let* ((key-string-list (s-split " " key-string))
                    (key-last (car (last key-string-list))))
               (if (string-match "^C\\-" key-last)
                   (god-mode-lookup-command (s-join " " (append (nbutlast key-string-list) (list (substring key-last 2)))))
                 (error "God: Unknown key binding for `%s`" key-string))))))))

(defcustom jhz/god-local-modeline-color
  "#8c2512"
  "Color of the modeline of the active pane in god mode."
  :group 'god
  :type 'color)

(defcustom jhz/god-paused-modeline-color
  "#12628c"
  "Color of the modeline of the active pane when god mode is temporarily paused."
  :group 'god
  :type 'color)

(defcustom jhz/god-other-modeline-color
  "#5d128c"
  "Color of the modeline of the inactive panes in god mode."
  :group 'god
  :type 'color)

(defcustom jhz/god-paused-timeout
  1.5
  "How many seconds before god mode automatically unpauses.
If set to nil, use <RET> to manually return to god mode."
  :group 'god
  :type '(choice (integer :tag "seconds")
                 (const :tag "disabled" nil)))

(defun jhz/god-mode-update-mode-line ()
  "Set the color of the modeline depending on if we are in God-mode, Incarnate-mode, or neither."
  (cond ((bound-and-true-p god-local-mode)
         (set-face-attribute 'mode-line nil
                             :foreground jhz/prev-modeline-fg
                             :background jhz/god-local-modeline-color)
         (set-face-attribute 'mode-line-inactive nil
                             :foreground jhz/prev-modeline-fgi
                             :background jhz/god-other-modeline-color))
        ((bound-and-true-p god-local-mode-paused)
         (set-face-attribute 'mode-line nil
                             :foreground jhz/prev-modeline-fg
                             :background jhz/god-paused-modeline-color)
         (set-face-attribute 'mode-line-inactive nil
                             :foreground jhz/prev-modeline-fgi
                             :background jhz/god-other-modeline-color))
        (t
         (set-face-attribute 'mode-line nil
                             :foreground jhz/prev-modeline-fg
                             :background jhz/prev-modeline-bg)
         (set-face-attribute 'mode-line-inactive nil
                             :foreground jhz/prev-modeline-fgi
                             :background jhz/prev-modeline-bgi))))

(defvar *god-timeout-flag* nil)
(defun jhz/start-god-resume-timeout ()
  (when (bound-and-true-p god-local-mode-paused)
    (progn
      (setq *god-timeout-flag* t)
      (run-with-idle-timer jhz/god-paused-timeout nil
                           #'(lambda ()
                               (progn
                                 (setq *god-timeout-flag* nil)
                                 (god-local-mode-resume)))))))

(defun incarnate ()
  "Leave God-mode temporarily."
  (interactive)
  (when (bound-and-true-p god-local-mode)
    (if jhz/god-paused-timeout
        (add-hook 'post-self-insert-hook
                  #'(lambda ()
                      (unless (and (bound-and-true-p god-local-mode-paused) *god-timeout-flag*)
                        (jhz/start-god-resume-timeout))))
      (incarnate-mode))
    (god-local-mode-pause)
    (when (display-graphic-p)
      (set-cursor-color "#ffff00")
      (setq cursor-type 'hbar))))

(define-minor-mode incarnate-mode
  "As normal but toggle God mode on RET."
  :lighter " God-Inc"
  :keymap `((,(kbd "<return>") . unincarnate)
            (,(kbd "RET") . unincarnate)))

(defun unincarnate ()
  "Return to God."
  (interactive)
  (incarnate-mode -1)
  (god-local-mode-resume))

(defun resume-cleanup ()
  "Restore any visuals set for paused god mode."
  (jhz/god-mode-update-mode-line)
  (when (display-graphic-p)
    (setq cursor-type jhz/prev-cursor-type)
    (set-cursor-color jhz/prev-cursor-color)))

(advice-add 'god-local-mode-resume :after #'resume-cleanup)

(defun god-single-insert (&optional num)
  "Like `quoted-insert', but read the next NUM inputs, and also run post-self-insert-hooks."
  (interactive "*p")
  (dotimes (count num)
    (let ((last-command-event (read-char)))
      (self-insert-command 1))))

(provide 'init-god)
;;; init-god.el ends here
