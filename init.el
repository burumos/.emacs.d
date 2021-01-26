;;; 初期設定ファイルの読込み
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; 設定ファイルを変更した後にバイトコンパイルしないと次回移行反映されない!!!
;; emacs --batch -f batch-byte-compile init.el

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'load-path
   (append
    (mapcar
     (lambda (dir) (concat user-emacs-directory dir))
     '(
       "elisp"
       ))
    load-path)))

(require 'my-util)
(require 'my-command)
(if (file-exists-p (concat user-emacs-directory "elisp/local.el"))
    (require 'local))
(require 'first-setting)
(require 'packages-1)
(require 'packages-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-migemo-mode t)
 '(avy-migemo-use-isearch-search-fun t)
 '(helm-mini-default-sources
   '(helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands))
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(jumplist-ex-mode t)
 '(jumplist-hook-commands
   '(isearch-forward swiper isearch end-of-buffer beginning-of-buffer mark-whole-buffer))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(open-junk-file-directory "~/junk/%Y/%m-%d-%H%M%S." t)
 '(package-selected-packages
   '(blackout el-get hydra leaf-keywords leaf yasnippet-snippets yaml-mode which-key web-mode vue-mode use-package undo-tree toml-mode tabbar switch-buffer-functions smart-jump rust-mode rjsx-mode popwin php-mode origami open-junk-file ng2-mode mwim migemo merlin markdown-mode magit jumplist json-mode ivy-yasnippet ivy-rich hungry-delete hiwin highlight-symbol go-mode git-gutter-fringe+ flyspell-correct-ivy flycheck eglot dockerfile-mode docker-tramp ddskk counsel-tramp company avy ace-jump-mode))
 '(tab-line-close-button-show nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-line-tab-current ((t (:inherit tab-line-tab :background "dark cyan"))))
 '(web-mode-comment-face ((t (:foreground "#587F35"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-pseudo-class ((t (:foreground "#DFCF44"))))
 '(web-mode-css-selector-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-string-face ((t (:foreground "#D78181"))))
 '(web-mode-doctype-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#D78181"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-tag-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-server-comment-face ((t (:foreground "#587F35")))))
