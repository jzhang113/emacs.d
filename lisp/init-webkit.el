;;; init-webkit.el --- Webkit browsing
;;; Commentary:

;;; Code:

(unless *is-a-mac*
  (elpaca-use-package (webkit :host github :repo "akirakyle/emacs-webkit" :files (:defaults "*.js" "*.css" "*.so") :pre-build ("make"))
    :bind ("C-c C-v" . webkit)
    :init
    (setq browse-url-browser-function 'webkit-browse-url)
    ;; Set this only if we are launching from terminal
    ;; (setq webkit-own-window t)
    :config
    (elpaca-use-package 'webkit-ace)
    (elpaca-use-package 'webkit-dark)))

(provide 'init-webkit)
;;; init-webkit.el ends here
