;;skk 日本語入力メソッド
;;辞書は下記URLでSKK-JISSYO..Lをダウロードしてelisp/ddsk-*/に配置
;;http://openlab.ring.gr.jp/skk/skk/dic/
(if (package-installed-p 'ddskk)
    (progn
      ;;(require 'skk-autoloads)
      (define-key global-map (kbd "C-x j") 'skk-mode)
      ;;(define-key global-map (kbd "C-x C-t") 'skk-tutorial)
      (setq skk-large-jisyo "~/.emacs.d/SKK-JISYO.L")
      (setq skk-sticky-key ";");; ";"で変換始め。;;で';'を入力
      (setq skk-egg-like-newline t) ;;C-mで改行しないで変換決定
      ))

;; 構文チェック
(if (package-installed-p 'flycheck)
    (progn
      (require 'flycheck)
      (global-flycheck-mode)
      ))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;括弧を自動的に挿入する
(if (package-installed-p 'smartparens)
    (progn
      (require 'smartparens-config)
      (smartparens-global-mode t)))


(if (package-installed-p 'markdown-mode)
    (progn
      (require 'markdown-mode)
      (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
      (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
      (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
      (add-hook 'markdown-mode-hook
                '(lambda ()
                   (markdown-toggle-fontify-code-blocks-natively)
                   (auto-fill-mode 0) ;; 自動改行機能をOFF
                   ))
      t))


(if (package-installed-p 'undo-tree)
    (progn
      (global-undo-tree-mode t)
      ;; C-?(C-Shift-/) redo
      ;; (global-set-key (kbd "M-z") 'undo-tree-redo)
      (global-set-key (kbd "C-M-_") 'undo-tree-redo)
      ))


;;helmの設定
;; M-y でキルリングを検索
;; helm-miniにemacs-commandを追加
(if (and (package-installed-p 'helm) nil)
    (progn
      (require 'helm-config)
      (helm-mode 1)
      (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

      (define-key helm-map (kbd "C-h") 'delete-backward-char)
      (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
      ;;(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
      ;;(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

      (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
            (let ((cmds))
              (mapatoms
               (lambda (elt) (when (commandp elt) (push elt cmds))))
              cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")

      (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
            (let ((cmds))
              (dolist (elem extended-command-history)
                (push (intern elem) cmds))
              cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history")

      (custom-set-variables
       '(helm-mini-default-sources '(helm-source-buffers-list
                     helm-source-recentf
                     helm-source-files-in-current-dir
                     helm-source-emacs-commands-history
                     helm-source-emacs-commands
                     )))
      ;; For find-file etc.
      (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
      ;; For helm-find-files etc.
      (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

      (define-key global-map (kbd "C-x C-r") 'helm-recentf) ;; 最近開いたファイルの一覧
      (define-key global-map (kbd "C-x C-b") 'helm-buffers-list) ;;バッファーリスト
      (define-key global-map (kbd "C-x C-f") 'helm-find-files)     ;;helmでファイルを探索
      ;; (define-key global-map (kbd "C-x C-t") 'helm-mark-ring)
      (define-key global-map (kbd "M-x") 'helm-M-x)
      ;;(define-key global-map (kbd "C-;") 'helm-mini)
      (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
      ))
;;ブックマーク 設定しだいでmigemoにも対応するらしい
(if (and (package-installed-p 'helm-bm) nil)
    (progn
      (require 'helm-bm)
      (define-key global-map (kbd "C-c C-l") 'helm-bm)
      (define-key global-map (kbd "C-c C-t") 'bm-toggle)
      ))
;; helmを使ったファイル検索
(if (package-installed-p 'helm-ls-git)
    (progn
      (require 'helm-ls-git)
      (define-key global-map (kbd "C-c C-f") 'helm-ls-git-ls)
      ))
;; helmを使ったgit grep
(if (package-installed-p 'helm-git-grep)
    (progn
      (require 'helm-git-grep)
      (define-key global-map (kbd "C-c C-s") 'helm-ls-git-ls)
      ))

;;;;;;;;;;;;;; ivy/counsel
(if (package-installed-p 'counsel)
    (progn
      (require 'counsel)
      (ivy-mode 1)
      (setq ivy-use-virtual-buffers t)
      (setq enable-recursive-minibuffers t)
      (global-set-key (kbd "C-s") 'swiper)
      (global-set-key (kbd "M-x") 'counsel-M-x)
      (global-set-key (kbd "C-x C-f") 'counsel-find-file)
      (global-set-key (kbd "C-x C-x C-f") 'counsel-git)
      (global-set-key (kbd "C-x C-x C-g") 'counsel-git-grep)
      (global-set-key (kbd "C-x C-r") 'counsel-recentf)
      (global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
      (global-set-key (kbd "M-y") 'counsel-yank-pop)
      ;; 検索文字列
      (setq ivy-re-builders-alist
            '((t . ivy--regex-ignore-order)))
      ;; 情報量を増やす
      (if (package-installed-p 'ivy-rich)
          (progn
            (require 'ivy-rich)
            (ivy-rich-mode 1)
            (setq ivy-format-function #'ivy-format-function-line)
            ))
      ;; ivy版migemo(うまくうごいていない)
      (if (package-installed-p 'avy-migemo)
          (progn
            (require 'avy-migemo)
            ;; (avy-migemo-mode 1)
            ))
      ;; ivyを使ったflyspell
      (if (package-installed-p 'flyspell-correct-ivy)
          (progn
            (require 'flyspell-correct-ivy)
            (define-key flyspell-mode-map (kbd "C-c f") 'flyspell-correct-wrapper)
            (setq flyspell-correct-interface #'flyspell-correct-ivy)
            ))
      ))

;;;;;;;;;;;;;;


;;最後の変更箇所に移動
;; (if (package-installed-p 'goto-last-change)
;;     (progn
;;       (define-key global-map (kbd "C-{") 'goto-last-change)
;;       ;; (define-key global-map (kbd "C-}") 'goto-last-change-reverse)
;;       ))

;;高速grep ripgrep
;;C:/Users/naoki/AppData/Roaming/ripgrep-0.3.2-gnu/rg.exe -S --no-heading --vimgrep "aaa" .
(if (package-installed-p 'ripgrep)
    (progn
      ;; rgバイナリの位置の設定
      ;;(setq ripgrep-executable "C:/Users/naoki/AppData/Roaming/ripgrep-0.3.2-gnu/rg.exe")
      ;; rgに渡すオプション
      (setq ripgrep-arguments '("-S"))))

;;カーソルをjump
(if (package-installed-p 'ace-jump-mode)
    (progn
      (require 'ace-jump-mode)
      (global-set-key (kbd "C-o") 'ace-jump-mode)))

;;company 補完
(if (package-installed-p 'company)
    (progn
      ;;自動補完をoffにしたい場合は、company-idle-delayをnilに設定する
      (global-company-mode +1)
      (setq company-idle-delay 0) ; デフォルトは0.5
      ;;色の設定
      (set-face-attribute 'company-tooltip nil
              :foreground "black" :background "lightgrey")
      (set-face-attribute 'company-tooltip-common nil
              :foreground "black" :background "lightgrey")
      (set-face-attribute 'company-tooltip-common-selection nil
              :foreground "white" :background "steelblue")
      (set-face-attribute 'company-tooltip-selection nil
              :foreground "black" :background "steelblue")
      (set-face-attribute 'company-preview-common nil
              :background nil :foreground "lightgrey" :underline t)
      (set-face-attribute 'company-scrollbar-fg nil
              :background "orange")
      (set-face-attribute 'company-scrollbar-bg nil
              :background "gray40")
      (global-set-key (kbd "C-t") 'company-complete)
      ;; C-n, C-pで補完候補を次/前の候補を選択
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      ;; C-sで絞り込む
      (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
      ;; TABで候補を設定
      (define-key company-active-map (kbd "<C-tab>") 'company-complete-selection)
      (define-key company-active-map (kbd "C-h") nil)
      ;; 各種メジャーモードでも C-tで company-modeの補完を使う
      (define-key emacs-lisp-mode-map (kbd "C-t") 'company-complete)
      (setq company-minimum-prefix-length 2) ; デフォルトは4
      (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
      (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
      ))

;;php-mode
(if (package-installed-p 'php-mode)
    (progn
      (require 'php-mode)
      ))

;; 単語を色付け
(if (package-installed-p 'highlight-symbol)
    (progn
      (require 'highlight-symbol)
      (define-key global-map (kbd "C-x C-l") 'highlight-symbol)
      (define-key global-map (kbd "C-x l") 'highlight-symbol-remove-all)
      (highlight-symbol-nav-mode)
      ))


(if (package-installed-p 'web-mode)
    (progn
      (require 'web-mode)
      ;; web-modeで開くファイル
      (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
      ;; インデント
      (add-hook 'web-mode-hook
        '(lambda ()
           (setq web-mode-attr-indent-offset t)
           (setq web-mode-markup-indent-offset 2)
           (setq web-mode-css-indent-offset 2)
           (setq web-mode-code-indent-offset 2)
           (setq web-mode-sql-indent-offset 2)
           (setq indent-tabs-mode nil)
           (setq tab-width 2)
           ))

      ;; 色
      (custom-set-faces
       '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
       '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
       '(web-mode-html-tag-bracket-face  ((t (:foreground "#4A8ACA"))))
       '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
       '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
       '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
       '(web-mode-comment-face           ((t (:foreground "#587F35"))))
       '(web-mode-server-comment-face    ((t (:foreground "#587F35"))))

       '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
       '(web-mode-comment-face           ((t (:foreground "#587F35"))))
       '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
       '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
       '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
       '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
       )
      ;; cosoleでも閉じタグを自動補完
      (setq web-mode-auto-close-style 1)
      (setq web-mode-tag-auto-close-style t)
      (setq web-mode-enable-auto-pairing t)
      (define-key web-mode-map (kbd "C-c '") 'web-mode-element-close)
      ))


(if (package-installed-p 'neotree)
    (progn
      (require 'neotree)
      ;; 隠しファイルをデフォルトで表示
      (setq neo-show-hidden-files t)
      ;; neotree でファイルを新規作成した後、自動的にファイルを開く
      (setq neo-create-file-auto-open t)
      ;; delete-other-window で neotree ウィンドウを消さない
      (setq neo-persist-show t)
      ;; キーバインドをシンプルにする
      (setq neo-keymap-style 'concise)
      ;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
      (setq neo-smart-open t)
      ;; color
      (custom-set-faces
       '(col-highlight ((t (:background "color-233"))))
       '(hl-line ((t (:background "color-233"))))
       ;; '(lazy-highlight ((t (:background "black" :foreground "white" :underline t))))
       '(lazy-highlight ((t (:background "black" :foreground "yellow" :underline t))))
       '(neo-dir-link-face ((t (:foreground "cyan"))))
       '(neo-file-link-face ((t (:foreground "white")))))
      (custom-set-variables)
      ))

;; C-aをいい感じにする
(if (package-installed-p 'mwim)
    (progn
      (define-key global-map (kbd "C-a") 'mwim-beginning-of-line-or-code)
      (define-key global-map (kbd "C-e") 'mwim-end-of-line-or-code)))

(if (package-installed-p 'hungry-delete)
    (progn
      (global-hungry-delete-mode 1)
    ))


(if (package-installed-p 'magit)
    (progn
      (require 'magit)
      ))
;; 変更点を表示
;; (if (package-installed-p 'git-gutter+)
;;     (progn
;;       (require 'git-gutter+)
;;       (global-git-gutter+-mode 1)
;;       (define-key git-gutter+-mode-map (kbd "C-c g") 'git-gutter+-show-hunk-inline-at-point)
;;       ))
(if (package-installed-p 'git-gutter-fringe+)
    (progn
      (require 'git-gutter-fringe+)
      (git-gutter-fr+-minimal)
      (setq git-gutter-fr:side 'left-fringe)
      (set-face-foreground 'git-gutter-fr+-modified "yellow")
      (set-face-foreground 'git-gutter-fr+-added    "blue")
      (set-face-foreground 'git-gutter-fr+-deleted  "white")
      ))



(if (package-installed-p 'open-junk-file)
    (progn
      (require 'open-junk-file)
      ))

;; 日本語をローマ字で検索する(cmigemo必須:sudo apt install cmigemo)
(if (package-installed-p 'migemo)
    (progn
      (setq migemo-command "cmigemo")
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-dictionary "/usr/local/var/homebrew/linked/cmigemo/share/migemo/utf-8/migemo-dict")
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8-unix)
      (load-library "migemo")
      (migemo-init)
      ))

;; アクティブウィンドウ以外の背景色を変更する
(if nil ;; (package-installed-p 'hiwin)
    (progn
      (hiwin-activate)                           ;; hiwin-modeを有効化
      (set-face-background 'hiwin-face "gray14") ;; 非アクティブウィンドウの背景色を設定
      ))

;; spaceを消す
;; (if (package-installed-p 'hungry-delete)
;;     (progn
;;       (require 'hungry-delete)
;;       ;; (global-hungry-delete-mode 1)
;;       ))

;; (if (package-installed-p 'tuareg))
;; ocaml用
(if (package-installed-p 'merlin)
    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
        (autoload 'merlin-mode "merlin" nil t nil)
        (add-hook 'tuareg-mode-hook 'merlin-mode t)
        (add-hook 'caml-mode-hook 'merlin-mode t))
      ; Make company aware of merlin
      (with-eval-after-load 'company
        (add-to-list 'company-backends 'merlin-company-backend))
      ; company on merlin managed buffers
      (add-hook 'merlin-mode-hook 'company-mode)
      ; Or enable it globally:
      ; (add-hook 'after-init-hook 'global-company-mode))
      ))

;; javascript IDE
(if (package-installed-p 'js2-mode)
    (progn
      (require 'js2-mode)
      (if (package-installed-p 'rjsx-mode)
          (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
      (add-hook 'js2-mode-hook
          (lambda ()
             (setq my-js-mode-indent-num 2)
             (setq js2-basic-offset my-js-mode-indent-num)
             (setq js-switch-indent-offset my-js-mode-indent-num)
             ))))

;; いい感じに定義元にジャンプする
(if (package-installed-p 'smart-jump)
    (progn
      (smart-jump-setup-default-registers)))

;; angular
(if (package-installed-p 'ng2-mode)
    (progn
      (require 'ng2-mode)
      ))

;; key bindの候補を出す
(if (package-installed-p 'which-key)
    (progn
      (require 'which-key)
      (which-key-mode)
      ;; (which-key-setup-side-window-right)
      (which-key-setup-side-window-bottom)
      ))

(if (package-installed-p 'use-package)
    (require 'use-package))

;; lsp
(use-package lsp-mode
  :if (and (not (and (boundp 'low-power) low-power)) nil)
  :config
  (require 'lsp-mode)
  (defvar lsp-language-id-configuration
    '((js2-jsx-mode . "JavaScript/TypeScript")))
  (if (package-installed-p 'lsp-javascript-typescript)
      (progn
        (add-hook 'js2-mode-hook #'lsp)
        ))
  (if (package-installed-p 'lsp-vue)
      (add-hook 'vue-mode-hook #'lsp))
  (if (package-installed-p 'lsp-ui)
      (progn
        (require 'lsp-ui)
        (add-hook 'lsp-mode-hook 'lsp-ui-mode)
        ))
  (if (package-installed-p 'company-lsp)
      (require 'company-lsp))
  )

(use-package eglot
  :config
  (require 'eglot)
  (add-hook 'js2-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  ;; https://langserver.org/
  ;; (add-to-list 'eglot-server-programs '(foo-mode . ("foo-language-server" "--args")))
  )

;; カーソルを移動前に戻す
;; (if (package-installed-p 'jumplist)
;;     (progn
;;       (require 'jumplist)
;;       (global-set-key (kbd "M-[") 'jumplist-previous)
;;       (global-set-key (kbd "M-]") 'jumplist-next)
;;       (global-set-key (kbd "C-c z") 'jumplist--set)
;;       (custom-set-variables
;;        '(jumplist-hook-commands
;;          '(isearch-forward swiper isearch end-of-buffer beginning-of-buffer mark-whole-buffer))
;;        '(jumplist-ex-mode t))
;;       ))

;; スニペット
(if (package-installed-p 'yasnippet)
    (progn
      (require 'yasnippet)
      (yas-global-mode 1)
      (if (package-installed-p 'ivy-yasnippet)
          (progn
            (require 'ivy-yasnippet)
            ))
      ))

;; vue mode
(if (package-installed-p 'vue-mode)
    (progn
      (require 'vue-mode)
      (add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))
      ))

;; json
(if (package-installed-p 'json-mode)
    (progn
      (require 'json-mode)
      ))

;; open-junk-file
(if (package-installed-p 'open-junk-file)
    (progn
      (require 'open-junk-file)
      ))

;; go-mode
(if (package-installed-p 'go-mode)
    (progn
      (require 'go-mode)
      ))

(provide 'setting-packages)
