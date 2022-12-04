;;; init-local.el --- Personal settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Additional packages
(require 'init-persp)
(require 'init-god)
(require 'init-webkit)
(require 'init-puni)

;; Disable transient-mark mode
(transient-mark-mode -1)

;; Key rebindings
(defun jhz/kill-region-or-backward-word ()
  "Kill region if there is one active, backward-kill word otherwise."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(elpaca nil
  (bind-key (kbd "C-x k") 'kill-this-buffer)
  (bind-key (kbd "C-x K") 'kill-buffer)
  (bind-key (kbd "C-w") 'jhz/kill-region-or-backward-word)

  ;; Faster movement
  (bind-key (kbd "C-S-B") 'backward-to-word)
  (bind-key (kbd "C-b") 'backward-word)
  (bind-key (kbd "C-S-F") 'forward-word)
  (bind-key (kbd "C-f") 'forward-to-word)
  (bind-key (kbd "M-b") 'backward-sexp)
  (bind-key (kbd "M-f") 'forward-sexp))

;; Extend windmove across tmux panes
;; See: https://gist.github.com/sebmaynard/7689568
;; (require 'windmove)

(defun sebwindmove (windfunc tmuxparam)
  "Wrap a windmove command WINDFUNC, and if there's no window above, try switching tmux pane instead via select-pane TMUXPARAM."
  (condition-case nil
      (funcall windfunc)
    (error (shell-command (concat "tmux select-pane " tmuxparam)))))

(defun sebwindmove-up ()
  "Select the window above the current one, or try switching tmux panes if there is none."
  (interactive)
  (sebwindmove 'windmove-up "-U"))
(defun sebwindmove-down ()
  "Select the window below the current one, or try switching tmux panes if there is none."
  (interactive)
  (sebwindmove 'windmove-down "-D"))
(defun sebwindmove-left ()
  "Select the window to the left of the current one, or try switching tmux panes if there is none."
  (interactive)
  (sebwindmove 'windmove-left "-L"))
(defun sebwindmove-right ()
  "Select the window to the right of the current one, or try switching tmux panes if there is none."
  (interactive)
  (sebwindmove 'windmove-right "-R"))

(elpaca nil
  (bind-key* "C-<left>" 'sebwindmove-left)
  (bind-key* "C-<right>" 'sebwindmove-right)
  (bind-key* "C-<up>" 'sebwindmove-up)
  (bind-key* "C-<down>" 'sebwindmove-down))

(provide 'init-local)
;;; init-local.el ends here
