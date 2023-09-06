;;; init-webkit.el --- Webkit browsing
;;; Commentary:

;;; Code:

(use-package webkit
  :elpaca (:host github :repo "akirakyle/emacs-webkit" :files (:defaults "*.js" "*.css" "*.so") :pre-build ("make"))
  :unless *is-a-mac*
  :bind ("C-c C-v" . webkit)
  :init
  (setq browse-url-browser-function 'webkit-browse-url)
  ;; Set this only if we are launching from terminal
  ;; (setq webkit-own-window t)
  :config
  (require 'webkit-ace)
  (require 'webkit-dark))

(provide 'init-webkit)
;;; init-webkit.el ends here
