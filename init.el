;;; 初期設定ファイルの読込み
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq load-path
      (append
       (mapcar
        (lambda (dir) (concat user-emacs-directory dir))
        '(
          "elisp"
          ))
       load-path))

;;mac用 optionキーをcommnadに読み替え。metaキーとして使用
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

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
 '(css-indent-offset 2)
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands)))
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(jumplist-ex-mode t)
 '(jumplist-hook-commands
   (quote
    (isearch-forward swiper isearch end-of-buffer beginning-of-buffer mark-whole-buffer)))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(package-selected-packages
   (quote
    (yaml-mode dockerfile-mode toml-mode counsel-tramp yafolding origami go-eldoc go-mode git-gutter-fringe+ open-junk-file json-mode lsp-php rjsx-mode use-package eglot lsp-javascript-typescript lsp-vue lsp-ui lsp-mode ivy-yasnippet yasnippet jumplist which-key flyspell-correct-ivy flycheck undo-tree smart-jump ivy-posframe ivy magit mwim highlight-symbol bind-key web-mode ace-jump-mode php-mode popwin company vue-mode ivy-rich counsel ng2-mode js2-mode typescript-mode merlin tuareg company-php company-web hiwin migemo hungry-delete markdown-mode ripgrep goto-last-change redo+ helm fuzzy ddskk auto-complete)))
 '(typescript-indent-level 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
