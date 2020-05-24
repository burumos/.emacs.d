;;; Code:

;;add package ripository "melpa"
(when (require 'package nil t)
  ;;パッケージリポジトリにMarmaladeと開発者運営のELPAの追加
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" , "http://marmalade-repo.org/packages/"))
  ;; (add-to-list 'package-archives
  ;;              '("ELPA" , "http://tromey.com/com/elpa/"))
  ;;インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

(defvar-local already-refresh-package nil
  "package-refresh-contentsを行なったかどうか")

(defmacro my/manage-package (package &rest body)
  "Packageの管理と設定を行なう. 有効にしない場合はpackageにnilを指定してくれ."
  `(if ,package
       (progn
         (if (package-installed-p ,package)
             (progn ,@body)
             (let* ((package-name (symbol-name ,package))
                    (result
                     (ignore-errors
                       (unless already-refresh-package
                         (progn
                           (package-refresh-contents)
                           (setq already-refresh-package t)))
                       (package-install ,package)
                       t)))
               (if result
                   (progn ,@body)
                 (debug-message "Fail install package: %s" package-name)
                 ))))))


;;skk 日本語入力メソッド
;;辞書は下記URLでSKK-JISSYO.Lをダウロードしてに.emcsa.d/に配置
;;http://openlab.ring.gr.jp/skk/skk/dic/
(my/manage-package 'ddskk
                   ;;(require 'skk-autoloads)
                   (define-key global-map (kbd "C-x j") 'skk-mode)
                   ;;(define-key global-map (kbd "C-x C-t") 'skk-tutorial)
                   (setq skk-large-jisyo "~/.emacs.d/SKK-JISYO.L")
                   (setq skk-sticky-key ";");; ";"で変換始め。;;で';'を入力
                   (setq skk-egg-like-newline t) ;;C-mで改行しないで変換決定
                   )

;; 構文チェック
(my/manage-package 'flycheck
                   (require 'flycheck)
                   (global-flycheck-mode)
                   )
;; (add-hook 'after-init-hook #'global-flycheck-mode)


;; redo
(my/manage-package nil 'undo-tree
                   (global-undo-tree-mode t)
                   ;; C-?(C-Shift-/) redo
                   ;; (global-set-key (kbd "M-z") 'undo-tree-redo)
                   (global-set-key (kbd "C-M-_") 'undo-tree-redo)
                   )

;; 画面内どこでもすぐにカーソルを移動
(my/manage-package 'ace-jump-mode
                   (require 'ace-jump-mode)
                   (global-set-key (kbd "C-o") 'ace-jump-mode))

;; C-aをいい感じにする
(my/manage-package 'mwim
                   (define-key global-map (kbd "C-a") 'mwim-beginning-of-line-or-code)
                   (define-key global-map (kbd "C-e") 'mwim-end-of-line-or-code))

(my/manage-package nil 'hungry-delete
                   (global-hungry-delete-mode 1)
                   )

;; git client
(my/manage-package 'magit
                   (require 'magit)
                   (define-key magit-diff-mode-map (kbd "q")
                     (lambda () (interactive) (magit-mode-bury-buffer t)))
                   (define-key magit-mode-map (kbd "q")
                     (lambda () (interactive) (magit-mode-bury-buffer t)))
                   )

;; tempolaryファイルを作る
(my/manage-package 'open-junk-file
                   (require 'open-junk-file)
                   )

;; 日本語をローマ字で検索する(cmigemo必須:sudo apt install cmigemo)
(my/manage-package 'migemo
                   (setq migemo-command "cmigemo")
                   (setq migemo-options '("-q" "--emacs"))
                   ;; (setq migemo-dictionary "/usr/local/var/homebrew/linked/cmigemo/share/migemo/utf-8/migemo-dict")
                   (setq migemo-user-dictionary nil)
                   (setq migemo-regex-dictionary nil)
                   (setq migemo-coding-system 'utf-8-unix)
                   (load-library "migemo")
                   (migemo-init)
                   )


;; いい感じに定義元にジャンプする
(my/manage-package 'smart-jump
                   (require 'smart-jump)
                   (smart-jump-setup-default-registers))



;; key bindの候補を出す
(my/manage-package 'which-key
                   (require 'which-key)
                   (which-key-mode)
                   ;; (which-key-setup-side-window-right)
                   (which-key-setup-side-window-bottom)
                   )

(my/manage-package 'eglot
                   (require 'eglot)
                   (add-hook 'js2-mode-hook 'eglot-ensure)
                   (add-hook 'rjsx-mode-hook 'eglot-ensure)
                   (add-hook 'typescript-mode-hook 'eglot-ensure)
                   ;; (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
                   ;; (add-to-list 'eglot-server-programs '(typescript-mode . ("javascript-typescript-langserver")))
                   (add-hook 'go-mode-hook 'eglot-ensure)
                   ;; https://langserver.org/
                   ;; (add-to-list 'eglot-server-programs '(foo-mode . ("foo-language-server" "--args")))
                   (add-to-list 'eglot-server-programs '(php-mode . ("php" "vendor/felixfbecker/language-server/bin/php-language-server.php")))
                   )

;; スニペット
(my/manage-package 'yasnippet
                   (require 'yasnippet)
                   (yas-global-mode 1)
                   (add-hook 'prog-mode-hook #'yas-minor-mode)
                   ;; snippetの追加は M-x yas-new-snippet
                   ;; #binding: C-x C-q の様にキーバインドにモード別で設定できる
                   ;; #--以降に入力されたものが挿入される
                   ;; snippet interface by ivy
                   (my/manage-package 'ivy-yasnippet
                                      (require 'ivy-yasnippet)
                                      (global-set-key (kbd "C-x C-x y") 'ivy-yasnippet)
                                      )
                   ;; snippet collection
                   (my/manage-package 'yasnippet-snippets
                                      (require 'yasnippet-snippets))
                   )

;; 単語を色付け
(my/manage-package 'highlight-symbol
                   (require 'highlight-symbol)
                   (define-key global-map (kbd "C-x C-l") 'highlight-symbol)
                   (define-key global-map (kbd "C-x l") 'highlight-symbol-remove-all)
                   (highlight-symbol-nav-mode)
                   )

(my/manage-package 'origami
                   (require 'origami)
                   (add-hook 'prog-mode-hook
                             (lambda () (origami-mode)))
                   (define-key origami-mode-map (kbd "C-c C-c [") 'origami-close-all-nodes)
                   (define-key origami-mode-map (kbd "C-c C-c ]") 'origami-open-all-nodes)
                   (define-key origami-mode-map (kbd "C-c C-[") 'origami-close-node-recursively)
                   (define-key origami-mode-map (kbd "C-c C-]") 'origami-open-node-recursively)
                   (define-key origami-mode-map (kbd "C-c [") 'origami-close-node)
                   (define-key origami-mode-map (kbd "C-c ]") 'origami-open-node)
                   )

;;;;;;;;;;;;;; ivy/counsel
(my/manage-package 'counsel
                   (require 'counsel)
                   (ivy-mode 1)
                   (setq ivy-use-virtual-buffers t)
                   (setq enable-recursive-minibuffers t)
                   (global-set-key (kbd "M-s s") 'swiper)
                   (global-set-key (kbd "M-x") 'counsel-M-x)
                   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
                   (global-set-key (kbd "C-x C-x f") 'counsel-git)
                   (global-set-key (kbd "C-x C-x g") 'counsel-git-grep)
                   (global-set-key (kbd "C-x C-r") 'counsel-recentf)
                   (global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
                   (global-set-key (kbd "M-y") 'counsel-yank-pop)
                   (define-key counsel-find-file-map (kbd "M-h") 'ivy-backward-kill-word)
                   ;; 検索文字列
                   (setq ivy-re-builders-alist
                         '((t . ivy--regex-ignore-order)))
                   ;; 情報量を増やす
                   (my/manage-package 'ivy-rich
                                      (require 'ivy-rich)
                                      (ivy-rich-mode 1)
                                      (setq ivy-format-function #'ivy-format-function-line)
                                      )
                   ;; ivyを使ったflyspell
                   (my/manage-package 'flyspell-correct-ivy
                                      (require 'flyspell-correct-ivy)
                                      (define-key flyspell-mode-map (kbd "C-x C-x f") 'flyspell-correct-wrapper)
                                      (setq flyspell-correct-interface #'flyspell-correct-ivy)
                                      )
                   ;; tramp ivy interface
                   (my/manage-package 'counsel-tramp
                                      (require 'counsel-tramp)
                                      )
                   )

;; dockerへtramp C-x C-f /docker: でアクセス
(my/manage-package 'docker-tramp
                   (require 'docker-tramp)
                   (require 'docker-tramp-compat)
                   (set-variable 'docker-tramp-use-names t) ;; IDでなくコンテナ名で補完
                   )
;; tabバーを表示する
(my/manage-package nil 'tabbar
                   (require 'tabbar)
                   (tabbar-mode)
                   (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
                   (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
                   ;;----- 左側のボタンを消す
                   (dolist (btn '(tabbar-buffer-home-button
                                  tabbar-scroll-left-button
                                  tabbar-scroll-right-button))
                     (set btn (cons (cons "" nil)
                                    (cons "" nil))))
                   ;; (tabbar-buffer-groups)
                   (setq tabbar-cycle-scope 'tabs)
                   ;; 色
                    '(tabbar-default ((t (:inherit variable-pitch :background "MediumPurple3" :foreground "grey75" :height 0.8))))
                    '(tabbar-selected ((t (:inherit tabbar-default :foreground "light green" :box (:line-width 1 :color "white" :style pressed-button)))))
                   )


;;company 補完
(my/manage-package 'company
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
                   )


;;高速grep ripgrep
;;C:/Users/naoki/AppData/Roaming/ripgrep-0.3.2-gnu/rg.exe -S --no-heading --vimgrep "aaa" .
(my/manage-package nil 'ripgrep
                   ;; rgバイナリの位置の設定
                   ;;(setq ripgrep-executable "C:/Users/naoki/AppData/Roaming/ripgrep-0.3.2-gnu/rg.exe")
                   ;; rgに渡すオプション
                   (setq ripgrep-arguments '("-S")))

(my/manage-package nil 'neotree
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
                   )

;; アクティブウィンドウ以外の背景色を変更する
(my/manage-package nil 'hiwin
                   (hiwin-activate)                           ;; hiwin-modeを有効化
                   (set-face-background 'hiwin-face "gray14") ;; 非アクティブウィンドウの背景色を設定
                   )


;;helmの設定
;; M-y でキルリングを検索
;; helm-miniにemacs-commandを追加
(my/manage-package nil 'helm
                   (require 'helm-config)
                   (helm-mode 1)
                   (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

                   (define-key helm-map (kbd "C-h") 'delete-backward-char)
                   (define-key helm-find-files-map (kbd "ESC") nil)
                   (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
                   (define-key helm-find-files-map (kbd "C-[") 'helm-find-files-up-one-level)
                   (define-key helm-find-files-map (kbd "ESC") nil)
                   (define-key helm-find-files-map (kbd "M-C-h") 'helm-find-files-up-one-level)
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

                   ;;ブックマーク 設定しだいでmigemoにも対応するらしい
                   (my/manage-package nil 'helm-bm
                                      (require 'helm-bm)
                                      (define-key global-map (kbd "C-c C-l") 'helm-bm)
                                      (define-key global-map (kbd "C-c C-t") 'bm-toggle)
                                      )

                   ;; helmを使ったファイル検索
                   (my/manage-package 'helm-ls-git
                                      (require 'helm-ls-git)
                                      (define-key global-map (kbd "C-x C-x f") 'helm-ls-git-ls)
                                      )

                   ;; helmを使ったgit grep
                   (my/manage-package 'helm-git-grep
                                      (require 'helm-git-grep)
                                      (define-key global-map (kbd "C-x C-x g") 'helm-git-grep)
                                      )

                   (my/manage-package 'counsel
                                      (require 'counsel)
                                      ;; helmだとdesctibe-functionが激重なため
                                      (define-key global-map (kbd "C-c f") 'counsel-describe-function))

                   (my/manage-package 'swiper-helm
                                      (require 'swiper-helm)
                                      (define-key global-map (kbd "M-s s") 'swiper-helm)
                                      )

                   (my/manage-package 'helm-tramp
                                      (require 'helm-tramp))

                   )

(provide 'packages-1)
;;; packages-1.el ends here
