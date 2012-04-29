(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.2/emacs" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "grey70"))))
 '(font-lock-builtin-face ((t (:foreground "grey50"))))
 '(font-lock-comment-face ((t (:foreground "darkgreen"))))
 '(font-lock-constant-face ((t (:foreground "grey50"))))
 '(font-lock-doc-face ((t (:foreground "grey50"))))
 '(font-lock-doc-string-face ((t (:foreground "yellow2"))))
 '(font-lock-function-name-face ((t (:foreground "SteelBlue"))))
 '(font-lock-keyword-face ((t (:foreground "red"))))
 '(font-lock-preprocessor-face ((t (:foreground "SteelBlue"))))
 '(font-lock-reference-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-string-face ((t (:foreground "yellow2"))))
 '(font-lock-type-face ((t (:foreground "violet"))))
 '(font-lock-variable-name-face ((t (:foreground "orange"))))
 '(font-lock-warning-face ((t (:foreground "grey50"))))
 '(isearch ((t (:background "yellow4" :foreground "black"))))
 '(mode-line ((t (:background "grey90" :foreground "black"))))
 '(zmacs-region ((t (:background "grey70" :foreground "black"))) t))
(setq font-lock-mode t)
(transient-mark-mode t)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode (quote right))
 '(transient-mark-mode t))
(set-cursor-color "pink")

(put 'erase-buffer 'disabled nil)

(put 'upcase-region 'disabled nil)
