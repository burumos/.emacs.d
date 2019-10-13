 ;;インストールするelisp
;;eval-buffer

;; パッケージ情報の更新
(package-refresh-contents)

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    ;;;; for auto-complete
    ;; auto-complete
    ;; fuzzy ;;popup pos-tip

    company
    popwin
    ;; undo-tree
    ;;;; flymake
    flycheck
    ;; helm
    ;; helm-bm ;;ブックマーク
    ;; helm-git-grep ;; grep
    ;; helm-ls-git ;; search-file
    ddskk
    php-mode
    json-mode
    markdown-mode
    ace-jump-mode
    web-mode
    js2-mode ;; javascript
    bind-key
    highlight-symbol
    mwim ;; C-aの動作をいい感じにする
    magit ;; git client
    git-gutter-fringe+
    hungry-delete
    migemo
    hiwin
    smart-jump ;; tag jumpみたいな
    undo-tree ;; redoのため
    open-junk-file

    ;; golang
    go-mode
    ;; company-go
    ;; go-eldoc

    ;; ivy
    ivy
    counsel
    ivy-rich ;; 情報を追加
    flyspell-correct-ivy ;; spell checkする奴のivy拡張
    avy-migemo
    ivy-yasnippet

    which-key ;; key bind一覧
    jumplist ;; カーソル位置管理
    yasnippet
    use-package

    ;; lsp
    eglot
    ;; lsp-mode
    ;; lsp-ui
    ;; lsp-vue
    ;; lsp-javascript-typescript
    ;; lsp-php
    ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))
