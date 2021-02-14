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
 '(load-path
   '("/Users/kawajiri_n/.emacs.d/elisp" "/Users/kawajiri_n/.emacs.d/elpa/ace-jump-mode-20140616.815" "/Users/kawajiri_n/.emacs.d/elpa/avy-20190925.1054" "/Users/kawajiri_n/.emacs.d/elpa/blackout-20200404.1550" "/Users/kawajiri_n/.emacs.d/elpa/company-20190907.1149" "/Users/kawajiri_n/.emacs.d/elpa/counsel-tramp-20190616.122" "/Users/kawajiri_n/.emacs.d/elpa/counsel-20191017.1812" "/Users/kawajiri_n/.emacs.d/elpa/ddskk-20190423.1234" "/Users/kawajiri_n/.emacs.d/elpa/cdb-20151205.1343" "/Users/kawajiri_n/.emacs.d/elpa/ccc-20151205.1343" "/Users/kawajiri_n/.emacs.d/elpa/docker-tramp-20170207.325" "/Users/kawajiri_n/.emacs.d/elpa/dockerfile-mode-20190505.1807" "/Users/kawajiri_n/.emacs.d/elpa/eglot-20191016.2213" "/Users/kawajiri_n/.emacs.d/elpa/el-get-20200912.1653" "/Users/kawajiri_n/.emacs.d/elpa/flycheck-20190930.1425" "/Users/kawajiri_n/.emacs.d/elpa/flyspell-correct-ivy-20181205.1932" "/Users/kawajiri_n/.emacs.d/elpa/flyspell-correct-20190408.1010" "/Users/kawajiri_n/.emacs.d/elpa/git-gutter-fringe+-20140729.1103" "/Users/kawajiri_n/.emacs.d/elpa/fringe-helper-20140620.2109" "/Users/kawajiri_n/.emacs.d/elpa/git-gutter+-20151204.1723" "/Users/kawajiri_n/.emacs.d/elpa/go-mode-20191018.2048" "/Users/kawajiri_n/.emacs.d/elpa/highlight-symbol-20160102.2009" "/Users/kawajiri_n/.emacs.d/elpa/hiwin-20150825.827" "/Users/kawajiri_n/.emacs.d/elpa/hungry-delete-20170412.102" "/Users/kawajiri_n/.emacs.d/elpa/hydra-20201115.1055" "/Users/kawajiri_n/.emacs.d/elpa/ivy-rich-20191011.226" "/Users/kawajiri_n/.emacs.d/elpa/ivy-yasnippet-20181002.1655" "/Users/kawajiri_n/.emacs.d/elpa/json-mode-20190123.422" "/Users/kawajiri_n/.emacs.d/elpa/json-reformat-20160212.853" "/Users/kawajiri_n/.emacs.d/elpa/json-snatcher-20150512.347" "/Users/kawajiri_n/.emacs.d/elpa/jumplist-20151120.345" "/Users/kawajiri_n/.emacs.d/elpa/leaf-keywords-20201225.1406" "/Users/kawajiri_n/.emacs.d/elpa/leaf-20201211.412" "/Users/kawajiri_n/.emacs.d/elpa/lv-20200507.1518" "/Users/kawajiri_n/.emacs.d/elpa/magit-20191017.1800" "/Users/kawajiri_n/.emacs.d/elpa/git-commit-20190928.1746" "/Users/kawajiri_n/.emacs.d/elpa/markdown-mode-20190802.2215" "/Users/kawajiri_n/.emacs.d/elpa/merlin-20190926.1346" "/Users/kawajiri_n/.emacs.d/elpa/migemo-20190112.516" "/Users/kawajiri_n/.emacs.d/elpa/mwim-20181110.1900" "/Users/kawajiri_n/.emacs.d/elpa/ng2-mode-20190524.1912" "/Users/kawajiri_n/.emacs.d/elpa/open-junk-file-20161210.1114" "/Users/kawajiri_n/.emacs.d/elpa/origami-20180101.1553" "/Users/kawajiri_n/.emacs.d/elpa/php-mode-20190930.111" "/Users/kawajiri_n/.emacs.d/elpa/pkg-info-20150517.1143" "/Users/kawajiri_n/.emacs.d/elpa/epl-20180205.2049" "/Users/kawajiri_n/.emacs.d/elpa/popwin-20150315.1300" "/Users/kawajiri_n/.emacs.d/elpa/rjsx-mode-20190614.2215" "/Users/kawajiri_n/.emacs.d/elpa/js2-mode-20190815.1327" "/Users/kawajiri_n/.emacs.d/elpa/rust-mode-20200709.723" "/Users/kawajiri_n/.emacs.d/elpa/smart-jump-20190925.1518" "/Users/kawajiri_n/.emacs.d/elpa/dumb-jump-20190928.1758" "/Users/kawajiri_n/.emacs.d/elpa/popup-20160709.1429" "/Users/kawajiri_n/.emacs.d/elpa/f-20190109.906" "/Users/kawajiri_n/.emacs.d/elpa/s-20180406.808" "/Users/kawajiri_n/.emacs.d/elpa/swiper-20191016.1657" "/Users/kawajiri_n/.emacs.d/elpa/ivy-20191018.1251" "/Users/kawajiri_n/.emacs.d/elpa/switch-buffer-functions-20171011.1704" "/Users/kawajiri_n/.emacs.d/elpa/tabbar-20180726.1735" "/Users/kawajiri_n/.emacs.d/elpa/toml-mode-20161107.1800" "/Users/kawajiri_n/.emacs.d/elpa/transient-20191017.1115" "/Users/kawajiri_n/.emacs.d/elpa/dash-20190920.1035" "/Users/kawajiri_n/.emacs.d/elpa/typescript-mode-20191006.2134" "/Users/kawajiri_n/.emacs.d/elpa/undo-tree-0.6.5" "/Users/kawajiri_n/.emacs.d/elpa/use-package-20190716.1829" "/Users/kawajiri_n/.emacs.d/elpa/bind-key-20180513.430" "/Users/kawajiri_n/.emacs.d/elpa/vue-mode-20190415.231" "/Users/kawajiri_n/.emacs.d/elpa/edit-indirect-20180422.1807" "/Users/kawajiri_n/.emacs.d/elpa/ssass-mode-20190521.249" "/Users/kawajiri_n/.emacs.d/elpa/vue-html-mode-20180428.2035" "/Users/kawajiri_n/.emacs.d/elpa/mmm-mode-0.5.7" "/Users/kawajiri_n/.emacs.d/elpa/web-mode-20190916.1858" "/Users/kawajiri_n/.emacs.d/elpa/which-key-20190802.240" "/Users/kawajiri_n/.emacs.d/elpa/with-editor-20191008.2002" "/Users/kawajiri_n/.emacs.d/elpa/async-20191009.1018" "/Users/kawajiri_n/.emacs.d/elpa/yaml-mode-20190625.1740" "/Users/kawajiri_n/.emacs.d/elpa/yasnippet-snippets-20191104.900" "/Users/kawajiri_n/.emacs.d/elpa/yasnippet-20191009.216" "/Library/Application Support/Emacs/27.1/site-lisp" "/Library/Application Support/Emacs/site-lisp" "/Applications/Emacs.app/Contents/Resources/site-lisp" "/Applications/Emacs.app/Contents/Resources/lisp" "/Applications/Emacs.app/Contents/Resources/lisp/vc" "/Applications/Emacs.app/Contents/Resources/lisp/url" "/Applications/Emacs.app/Contents/Resources/lisp/textmodes" "/Applications/Emacs.app/Contents/Resources/lisp/progmodes" "/Applications/Emacs.app/Contents/Resources/lisp/play" "/Applications/Emacs.app/Contents/Resources/lisp/org" "/Applications/Emacs.app/Contents/Resources/lisp/nxml" "/Applications/Emacs.app/Contents/Resources/lisp/net" "/Applications/Emacs.app/Contents/Resources/lisp/mh-e" "/Applications/Emacs.app/Contents/Resources/lisp/mail" "/Applications/Emacs.app/Contents/Resources/lisp/leim" "/Applications/Emacs.app/Contents/Resources/lisp/language" "/Applications/Emacs.app/Contents/Resources/lisp/international" "/Applications/Emacs.app/Contents/Resources/lisp/image" "/Applications/Emacs.app/Contents/Resources/lisp/gnus" "/Applications/Emacs.app/Contents/Resources/lisp/eshell" "/Applications/Emacs.app/Contents/Resources/lisp/erc" "/Applications/Emacs.app/Contents/Resources/lisp/emulation" "/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp" "/Applications/Emacs.app/Contents/Resources/lisp/cedet" "/Applications/Emacs.app/Contents/Resources/lisp/calendar" "/Applications/Emacs.app/Contents/Resources/lisp/calc" "/Applications/Emacs.app/Contents/Resources/lisp/obsolete") t)
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(open-junk-file-directory "~/junk/%Y/%m-%d-%H%M%S." t)
 '(package-selected-packages
   '(docker-tramp-compat exist-docker-command "which dokcer" 0 "which dokcer" shell-command = blackout el-get hydra leaf-keywords leaf yasnippet-snippets yaml-mode which-key web-mode vue-mode use-package undo-tree toml-mode tabbar switch-buffer-functions smart-jump rust-mode rjsx-mode popwin php-mode origami open-junk-file ng2-mode mwim migemo merlin markdown-mode magit jumplist json-mode ivy-yasnippet ivy-rich hungry-delete hiwin highlight-symbol go-mode git-gutter-fringe+ flyspell-correct-ivy flycheck eglot dockerfile-mode docker-tramp ddskk counsel-tramp company avy ace-jump-mode))
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
