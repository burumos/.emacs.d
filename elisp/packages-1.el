;;; Code:

;;add package ripository "melpa"
(customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; install leaf(package maganger)
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(leaf leaf-keywords
    :ensure t
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

;;skk 日本語入力メソッド
;;辞書は下記URLでSKK-JISSYO.Lをダウロードしてに.emcsa.d/に配置
;;http://openlab.ring.gr.jp/skk/skk/dic/
(leaf ddskk
  :ensure t
  :init (let ((dict-file "~/.emacs.d/SKK-JISYO.L"))
          (if (not (file-exists-p dict-file))
              (url-copy-file
               "https://github.com/skk-dev/dict/raw/master/SKK-JISYO.L"
               dict-file)))
  :custom (
           (skk-large-jisyo . "~/.emacs.d/SKK-JISYO.L")
           (skk-sticky-key . ";");; ";"で変換始め。;;で';'を入力
           (skk-egg-like-newline . t) ;;C-mで改行しないで変換決定
           )
  :bind (("C-x j" . skk-mode))
  )

;; 構文チェック
(leaf flycheck
  :ensure t
  :config (global-flycheck-mode)
  )
;; (add-hook 'after-init-hook #'global-flycheck-mode)


;; redo packageがない...
;; ( nil 'undo-tree
;;                    (global-undo-tree-mode t)
;;                    ;; C-?(C-Shift-/) redo
;;                    ;; (global-set-key (kbd "M-z") 'undo-tree-redo)
;;                    (global-set-key (kbd "C-M-_") 'undo-tree-redo)
;;                    )

;; 画面内どこでもすぐにカーソルを移動
(leaf ace-jump-mode
  :ensure t
  :bind (("C-o" . ace-jump-mode)))

;; C-aをいい感じにする
(leaf mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-line-or-code)
         ("C-e" . mwim-end-of-line-or-code)))

;; git client
(defun quite-magit-buffer ()
  (interactive)
  (magit-mode-bury-buffer t))
(leaf magit
  :ensure t
  :bind ((:magit-diff-mode-map
          ("q" . quite-magit-buffer))
         (:magit-mode-map
          ("q" . quite-magit-buffer))
         ))

;; tempolaryファイルを作る
(leaf open-junk-file
  :ensure t
  :custom (open-junk-file-format . "~/junk/%Y/%m-%d-%H%M%S."))

;; 日本語をローマ字で検索する(cmigemo必須:sudo apt install cmigemo)
(leaf migemo
  :ensure t
  :custom ((migemo-command . "cmigemo")
           (migemo-options . '("-q" "--emacs"))
           ;; (migemo-dictionary "/usr/local/var/homebrew/linked/cmigemo/share/migemo/utf-8/migemo-dict")
           ;; (migemo-user-dictionary . nil)
           ;; (migemo-regex-dictionary . nil)
           (migemo-coding-system . 'utf-8-unix))
  :config
  (load-library "migemo")
  (migemo-init)
  )


;; いい感じに定義元にジャンプする
(leaf smart-jump
  :ensure t
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references))
  :config
  ;; (smart-jump-setup-default-registers)
  )


;; key bindの候補を出す
(leaf which-key
  :ensure t
  :config
  (which-key-mode)
  ;; (which-key-setup-side-window-right)
  (which-key-setup-side-window-bottom))

(leaf eglot
  :ensure t
  :hook
  ;; (js2-mode-hook . eglot-ensure)
  (js-mode-hook . eglot-ensure)
  (rjsx-mode-hook . eglot-ensure)
  (typescript-mode-hook . eglot-ensure)
  (go-mode-hook . eglot-ensure)
  (rust-mode-hook . eglot-ensure)
  (swift-mode-hook . eglot-ensure)
  (css-mode-hook . eglot-ensure)
  (ruby-hook . eglot-ensure)
  ;; gem install solargraph
  (ruby-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(php-mode . ("php" "vendor/felixfbecker/language-server/bin/php-language-server.php")))
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("sourcekit-lsp")))
  ;; npm i -g vscode-css-languageserver-bin
  (add-to-list 'eglot-server-programs
               '(css-mode . ("css-launguageserver" "--stdio")))
  ;; https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary
  ;; インストール後、pathを通す
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  )

;; スニペット
(leaf yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode)
  :config
  (yas-global-mode 1)
  ;; snippetの追加は M-x yas-new-snippet
  ;; #binding: C-x C-q の様にキーバインドにモード別で設定できる
  ;; #--以降に入力されたものが挿入される
  ;; snippet interface by ivy
  (leaf ivy-yasnippet
    :ensure t
    :bind ("C-x C-x y" . ivy-yasnippet))
  ;; snippet collection
  (leaf yasnippet-snippets
      :ensure t)
  )

;; 単語を色付け
(leaf highlight-symbol
  :ensure t
  :bind (("C-x C-l" . highlight-symbol)
         ("C-x l" . highlight-symbol-remove-all))
  :config
  (highlight-symbol-nav-mode)
  )

(leaf origami
  :ensure t
  :hook (prog-mode-hook . (lambda () (origami-mode)))
  :bind (:origami-mode-map
         (("C-c C-c [" . origami-close-all-nodes)
          ("C-c C-c ]" . origami-open-all-nodes)
          ("C-c C-[" . origami-close-node-recursively)
          ("C-c C-]" . origami-open-node-recursively)
          ("C-c [" . origami-close-node)
          ("C-c ]" . origami-open-node))))

;; M-xやfind-fileのUIを提供する
(leaf vertico
  :ensure t
  :config
  (vertico-mode)
  :custom
  ((vertico-cycle . t)))

;; M-xやfind-fileでの絞り込みを先頭一致から部分一致に変更する
(leaf orderless
  :ensure t
  :custom
  ((completion-styles . '(orderless basic))
   (completion-category-defaults . nil)
   (completion-category-overrides . '((file (styles partial-completion)))))
  )

;; コマンドを追加する
(leaf consult
  :ensure t
  :bind
  (("C-x C-r" . consult-recent-file)
   ("M-y" . consult-yank-pop)
   ("C-x C-x g" . consult-git-grep))
  :config
  (leaf consult-ls-git
    :ensure t
    :bind (("C-x C-x f" . consult-ls-git))))

;; dockerへtramp C-x C-f /docker: でアクセス
;; (leaf docker-tramp
;;   :ensure t
;;   :when (= (shell-command "which docker") 0)
;;   :custom (docker-tramp-use-names . t) ;; IDでなくコンテナ名で補完
;;   )

;;company 補完
(leaf company
  :ensure t
  :custom ((company-idle-delay . 0) ; デフォルトは0.5
           (company-minimum-prefix-length . 2) ; デフォルトは4
           (company-selection-wrap-around . t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
           (company-transformers . '(company-sort-by-occurrence company-sort-by-backend-importance)))
  :bind (("C-t" . company-complete)
         (:company-active-map
          ;; C-n, C-pで補完候補を次/前の候補を選択
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ;; C-sで絞り込む
          ("C-s" . company-filter-candidates)
          ;; TABで候補を設定
          ("<C-tab>" . company-complete-selection)
          ("C-h" . nil))
         ;; 各種メジャーモードでも C-tで company-modeの補完を使う
         (:emacs-lisp-mode-map ("C-t" . company-complete)))
  :config
  ;;自動補完をoffにしたい場合は、company-idle-delayをnilに設定する
  (global-company-mode +1)
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
                      :background "gray40"))


;;高速grep ripgrep
;;C:/Users/naoki/AppData/Roaming/ripgrep-0.3.2-gnu/rg.exe -S --no-heading --vimgrep "aaa" .
(leaf ripgrep
  :ensure t
  :when nil
  :custom
  ;; rgバイナリの位置の設定
  ;;(setq ripgrep-executable "C:/Users/naoki/AppData/Roaming/ripgrep-0.3.2-gnu/rg.exe")
  ;; rgに渡すオプション
  (ripgrep-arguments . '("-S")))

(leaf neotree
  :ensure t
  :when nil
  :custom (
           ;; 隠しファイルをデフォルトで表示
           (neo-show-hidden-files . t)
           ;; neotree でファイルを新規作成した後、自動的にファイルを開く
           (neo-create-file-auto-open . t)
           ;; delete-other-window で neotree ウィンドウを消さない
           (neo-persist-show . t)
           ;; キーバインドをシンプルにする
           (neo-keymap-style . concise)
           ;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
           (neo-smart-open . t))
  :config
  ;; color
  (custom-set-faces
   '(col-highlight ((t (:background "color-233"))))
   '(hl-line ((t (:background "color-233"))))
   ;; '(lazy-highlight ((t (:background "black" :foreground "white" :underline t))))
   '(lazy-highlight ((t (:background "black" :foreground "yellow" :underline t))))
   '(neo-dir-link-face ((t (:foreground "cyan"))))
   '(neo-file-link-face ((t (:foreground "white")))))
  (custom-set-variables)
  )

;; アクティブウィンドウ以外の背景色を変更する
(leaf hiwin
  :ensure t
  :when nil
  :config
  (hiwin-activate)                           ;; hiwin-modeを有効化
  (set-face-background 'hiwin-face "gray14") ;; 非アクティブウィンドウの背景色を設定
  )

(leaf editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(leaf google-translate
  :ensure t
  :bind (("C-c T" . google-translate-smooth-translate))
  :custom ((google-translate-default-source-language . "en")
           (google-translate-default-target-language . "ja")
           (google-translate-translation-directions-alist . '(("en" . "ja"))))
  :config
  (leaf popup
    :ensure t
    )
  ;; 一時的な対応: https://github.com/atykhonov/google-translate/issues/137
  )
(defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

;; 言語別インデント等の設定を行なう. ~/.emacs.d/.editorconfigをユーザールートにリンクさせる必要あり
(leaf editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(add-to-load-path "elpa")
(provide 'packages-1)
;;; packages-1.el ends here
