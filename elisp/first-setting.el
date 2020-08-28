;;;  $Id: $
;;; -*- mode:emacs-lisp; coding:utf-8 -*-
;;; Time-stamp: <2018-05-16 09:45:27 kawajiri>
;;; Code:

;;; This init.el came from
;;; http://www.clear-code.com/blog/2012/3/20.html
;;; The newest version can be found at
;;; https://github.com/clear-code/emacs.d
;;; Special thanks to kou in Clear Code Co.

;; ~/.emacs.d/以下に
;; ディレクトリを作成
(let ((require-dirs '("ehist" "tmp" "elisp" "elpa")))
  (dolist (d require-dirs)
    (let ((dir (concat user-emacs-directory d)))
      (unless (file-exists-p dir)
            (make-directory dir)))))

;;;================= Load Path =================
(defun add-to-load-path (&rest paths)
  "package.elなどでインストールしたパッケージをロードする関数"
  (let ((path
         (dolist (path paths paths)
           (let ((default-directory
                   (expand-file-name (concat user-emacs-directory path))))
             (add-to-list 'load-path default-directory)
             (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
                 (normal-top-level-add-subdirs-to-load-path))))))))
(add-to-load-path "elpa")

;;;================= Global Keys =================
(define-prefix-command 'C-x2-map) ;; C-x C-x押下時のキーマップ作成
(define-key global-map (kbd "C-x C-x") C-x2-map) ;; C-x C-xで先程作成したキーマップを適用
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ;;ミニバッファでも削除
(define-key global-map (kbd "M-?") 'help-for-help)        ; ヘルプ
(define-key global-map (kbd "C-c h") 'describe-key)      ; キーバインドを調べる
(define-key global-map (kbd "C-c b") 'describe-bindings) ; キーバインドの割り当て表を表示
(define-key global-map (kbd "C-c f") 'describe-function) ; 関数の説明
(define-key global-map (kbd "C-c v") 'describe-variable) ; 変数の説明
(define-key global-map (kbd "C-c i") 'indent-region)      ; インデント
(define-key global-map (kbd "C-c C-i") 'hippie-expand)    ; 補完
(define-key global-map (kbd "C-M-n") 'next-multiframe-window) ;; フレーム切り替え(次へ移動)
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window) ;;フレーム切り替え(前へ移動)
(global-set-key (kbd "C-m") 'newline-and-indent);;newline and indent C-m
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines);;折返しを toggleで
(define-key global-map (kbd "M-h") 'backward-kill-word) ;;前の単語を削除
(define-key global-map (kbd "C-C C-r") 'point-to-register) ;;カーソル位置を保存
(define-key global-map (kbd "C-C C-t") 'jump-to-register) ;;記憶したカーソル位置に戻る
(define-key global-map (kbd "C-\\") 'hs-toggle-hiding) ;;折り畳み
(define-key global-map (kbd "C-a") 'back-to-indentation) ;;スペースを除いた行頭に移動
(define-key global-map (kbd "C-c C-c C-h") 'highlight-phrase) ;;ワードを指定して色をつける
(define-key global-map (kbd "C-c C-c C-u") 'unhighlight-regexp) ;;色つけ除去
(define-key global-map (kbd "C-M-S-s") 'replace-string) ;;文字列置換
(define-key global-map (kbd "M-r") 'repeat) ;; 直前のコマンドを実行
(define-key global-map (kbd "C-x C-x C-r") 'repeat-complex-command) ;; 直前のコマンドを編集->実行
(define-key global-map (kbd "M-d") 'my-delete-word) ;; カーソル直後の単語を削除(自作)
(define-key global-map (kbd "M-h") 'my-backward-delete-word) ;; カーソル直前の単語を削除(自作)
(define-key global-map (kbd "C-x C-x <down>") 'enlarge-window) ;; windowの縦を広げる
(define-key global-map (kbd "C-x C-x <up>")
  (lambda (n) (interactive "p") (enlarge-window (- n)))) ;; windowの縦を狭める
(define-key global-map (kbd "C-x C-x <right>") 'enlarge-window-horizontally) ;; windowの横を広げる
(define-key global-map (kbd "C-x C-x <left>")
  (lambda (n) (interactive "p") (enlarge-window-horizontally (- n)))) ;; windowの横を狭める
(define-key global-map (kbd "M-;") 'my-comment-dwim) ;; region選択なしでもコメントアウト
(global-set-key (kbd "M-[") 'my-point-history-back) ;; back point
(global-set-key (kbd "M-]") 'my-point-history-go) ;; back back point
(define-key global-map (kbd "C-x C-x k") 'kill-buffer) ;; bufferを選んで閉じる
(define-key global-map (kbd "C-x k") 'kill-this-buffer) ;; 今開いてるbufferを閉じる
(define-key global-map (kbd "C-x C-x TAB") (lambda () (interactive) (insert "\t"))) ;; tabを入力
(global-set-key (kbd "C-m") 'c-m) ;; indentしたり、しなかったりしろ

;; window別buffer履歴
(require 'buffer-history)
(global-set-key (kbd "M-0") 'mybh-switch-next-buffer) ;; バッファ移動(前)
(global-set-key (kbd "M-9") 'mybh-switch-prev-buffer) ;; バッファ移動(後)
(global-set-key (kbd "C-x C-x 0") 'mybh-remove-buffer) ;; バッファリストから削除

(if (functionp 'global-tab-line-mode)
    (progn
      ;; 左のタブに切り替え。最左であれば最右に切り替え
      (define-key global-map (kbd "M-9") 'my-switch-to-prev-tab)
      ;; 右のタブに切り替え。最右であれば最左に切り替え
      (define-key global-map (kbd "M-0") 'my-switch-to-next-tab)
      ;; 現在のバッファをタブバーから消す
      (define-key global-map (kbd "C-x C-x 0") 'bury-buffer)
      ))

;; redo
(require 'redo+)
(setq undo-no-redo t) ;; undoにredoした履歴を含まない
(define-key global-map (kbd "C-x C-/") 'redo) ;; redo
(define-key global-map (kbd "C-x C-_") 'redo) ;; redo


;; smerge用
(require 'smerge-mode)
(define-key smerge-mode-map (kbd "M-n") 'smerge-next)
(define-key smerge-mode-map (kbd "M-p") 'smerge-prev)

;;C-c C-RET で矩形編集モード
(define-key global-map (kbd "C-c C-SPC") 'cua-set-rectangle-mark)
;;M-o 半角スペース
;;M-n 連番
(define-key global-map (kbd "C-c C-s") 'flyspell-auto-correct-word)

;; emacs lisp mode
(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)

;; grep
(require 'grep)
(define-key grep-mode-map (kbd "q") (lambda () (interactive) (quit-window t)))

;; help
(require 'help-mode)
(define-key help-mode-map (kbd "q") (lambda () (interactive) (quit-window t)))

;;;================= Coding System =================
;;; 日本語を使う (Localeに従う)
(set-locale-environment nil)

;;;;================= Basic Settings =================
;; ウィンドウを透明にする
;; アクティブウィンドウ／非アクティブウィンドウ（alphaの値で透明度を指定）
(add-to-list 'default-frame-alist '(alpha . (1.0 0.9)))

;;; 環境設定
;; (set-language-environment "Japanese")

;;文字コードの変更
(prefer-coding-system 'utf-8)

;; タブの幅
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
;; (make-tab-stop-list (setq default-tab-width (setq-default tab-width 4)))

;; タブの無効化してスペースに(nilでスペース化)
(setq-default indent-tabs-mode nil)

;; 空白文字の可視化
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
;;                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(global-whitespace-mode 1)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)
;;; メニューバーの表示 (正数でON, 0以下でOFF)
(menu-bar-mode -1)
;;; ツールバーの表示 (正数でON, 0以下でOFF)
(tool-bar-mode -1)
;;; スクロールバーを右側に表示する
;(set-scroll-bar-mode 'right)
;;; カーソルの点滅 (正数でON, 0以下でOFF
;(blink-cursor-mode -1)

;; 複数ウィンドウを禁止する
(setq ns-pop-up-frames nil)

;; スペース、タブなどを可視化する
;; (global-whitespace-mode nil)

;;; evalした結果を全部表示
(setq eval-expression-print-length nil)
;; Value for `print-length' while printing value in `eval-expression'.
;; A value of nil means no limit.

;;; 括弧
;;; 対応する括弧を光らせる (正数でON, 0以下でOFF)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
;;(setq show-paren-style 'expression)
;;(setq show-paren-style 'mixed)
;;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style t);;'mixed)
;; 閉じ括弧を補完
(electric-pair-mode 1)

;;; 空白
;; 空白や長すぎる行を視覚化する。
(require 'whitespace)
;; 1行が80桁を超えたら長すぎると判断する。
;(setq whitespace-line-column 80)
;(setq whitespace-style '(face           ; faceを使って視覚化する。
;                         trailing       ; 行末の空白を対象とする。
;                         lines-tail     ; 長すぎる行のうち whitespace-line-column以降のみを対象とする。
;                         indentation    ; indent-tabs-modeと逆のインデントを対象とする。
;                         space-before-tab  ; タブの前にあるスペースを対象とする。
;                         space-after-tab ; タブの後にあるスペースを対象とする。
;			 ))
;; デフォルトで視覚化を有効にする。
;(global-whitespace-mode 1)

;;; 位置
;; 現在行を目立たせる (正数でON, 0以下でOFF)
(global-hl-line-mode 0)
;; カーソルの位置が何文字目かを表示する (正数でON, 0以下でOFF)
;(column-number-mode 1)
;; カーソルの位置が何行目かを表示する (正数でON, 0以下でOFF)
(line-number-mode 0)

;;; カーソルの位置を保存し、次回ファイルを開いたときに、
;;; カーソルの位置を復帰させる。
(require 'saveplace)
(setq-default save-place t)
(save-place-mode 1)


;;; M-x linum-mode
;;; バッファの左側に行番号を表示
(global-linum-mode 1)      ; デフォルトで linum-mode を有効にする (正数でON, 0以下でOFF)
;(setq linum-format "%5d ") ; 5 桁分の領域を確保して行番号のあとにスペースを入れる

;;; 行
;; 行の先頭でC-kを一回押すだけで行全体を消去する
;(setq kill-whole-line t)
;; 最終行の改行を(し忘れたら)自動的に入れる
;; (setq require-final-newline t)
;; バッファの最後でカーソル下移動したとき、新規行を追加する (Non-nilでON, nilでOFF)
(setq next-line-add-newlines nil)

;; 1行ずつスクロールさせる
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; C-vによるスクロールでpage-x分移動する
(setq next-screen-context-lines 4)

;;; 補完
;; 補完時に大文字小文字を区別しない (Non-nilでON, nilでOFF)
(setq completion-ignore-case nil)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
;; 補完可能なものを随時表示 (正数でON, 0以下でOFF)
;; 少しうるさい
(icomplete-mode 1)

;;; 履歴
;; 履歴数
(setq history-length 10000) ; default 30
;; ミニバッファの履歴を保存する (正数でON, 0以下でOFF)
(savehist-mode 1)

;;; File メニューに「Open Recent」(最近開いたファイル)というサブメニュー
;;; が追加される。(正数でON, 0以下でOFF)
(recentf-mode 1)
(setq recentf-max-saved-items 10000) ;履歴保存数 default 20
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
;; (setq recentf-auto-cleanup 'never) ; 存在しないファイルでも履歴を削除しない
;; (setq recentf-max-menu-items 10) ; default 10

;; recentf の メッセージをエコーエリア(ミニバッファ)に表示しない
;; (*Messages* バッファには出力される)
(defun recentf-save-list-inhibit-message:around (orig-func &rest args)
  (setq inhibit-message t)
  (apply orig-func args)
  (setq inhibit-message nil)
  'around)
(advice-add 'recentf-cleanup   :around 'recentf-save-list-inhibit-message:around)
(advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message:around)


;;; diff
;; ediffを1ウィンドウで実行
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのオプション
(setq diff-switches '("-u" "-p" "-N"))

;;; バッファ名
;; ファイル名が重複するバッファ名には、ディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; 現在の関数名をウィンドウ上部に表示する。
(which-function-mode 0)

;;; Emacsサーバー
;; emacsclientで接続できるようにする。
(server-start)

;; Executing time-stamp when file is saved.
(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks
          (cons 'time-stamp write-file-hooks)))

(setq tramp-default-method "scp")

;; 再帰回数
(setq max-specpdl-size 10000) ;; デフォルト 1300
(setq max-lisp-eval-depth 10000) ;; デフォルト 600

;;現時刻を表示する
;; (setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
;; (display-time-mode t)

;;ノートパソコンの場合にバッテリ残量を表示
;;(display-battery-mode t)

;;ファイルサイズを表示
;; (size-indication-mode t)

 ;; ビープ音禁止
(setq ring-bell-function 'ignore)

;; 選択領域を削除キーで一括削除
(delete-selection-mode t)

;; 行頭 kill-line (C-k) で行全体をカット
(setq kill-whole-line t)

;; 読み取り専用バッファーでもカット系でコピー可能
(setq kill-read-only-ok t)

;; png, jpg などのファイルを画像として表示
(setq auto-image-file-mode t)

;;;================= Grep =================
;;; 再帰的にgrep
;;; -rオプションを追加して常に再帰的にgrepするようにします。
;;; grep-findなどを使い分けなくてもすみます。
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;;;================= Dired ================= 
;;; diredを便利にする
(require 'dired-x)

;;; diredから"r"でファイル名をインライン編集する
;;; ファイル名をそのまま変更できるのは便利
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(define-key dired-mode-map "[" 'dired-up-directory)
(define-key dired-mode-map "q" (lambda ()(interactive) (quit-window t)))
(define-key dired-mode-map (kbd "M-C-n") nil)
(define-key dired-mode-map (kbd "M-C-p") nil)
;; ファイル名のみを表示
;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; ディレクトリの再帰的コピーを問い合わせ無く行う
(setq dired-recursive-copies 'always)
;; ディレクトリの削除時は最初の一回だけ尋く
(setq dired-recursive-deletes 'top)

;;;================= upcase-region and downcase-region ================= 
;; Enabling upcase-region and downcase-region commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; For safety, un-setting the key bindings for these commands.
(global-unset-key "\C-x\C-l") ; default binding is 'downcase-region
(global-unset-key "\C-x\C-u") ; default binding is 'upcase-region


;;;================= Text-Mode =================
(add-hook 'text-mode-hook (function (lambda () (auto-fill-mode 1))))
(setq fill-column nil)

;; backup ファイルオープン時のバックアップ (xxx~)
;; -------------------------------------------

;; 実行の有無
(setq make-backup-files t)

;; 格納ディレクトリーの変更
;;   (対象ディレクトリー . 格納ディレクトリー) のリスト
(setq backup-directory-alist '((".*" . "~/.ehist")))


;; 番号付けによる複数保存
(setq version-control     t)  ;; 実行の有無
(setq kept-new-versions   5)  ;; 最新の保持数
(setq kept-old-versions   1)  ;; 最古の保持数
(setq delete-old-versions t)  ;; 範囲外を削除

;;; =============バックアップ======================
;;
;; バックアップファイル(foo.txt~)を作らない (Non-nilでON, nilでOFF)編集前ファイル
;; (setq backup-inhibited t)
;; 終了時にオートセーブファイルを消す (Non-nilでON, nilでOFF)
(setq delete-auto-save-files t)

 ;; backup ファイルオープン時のバックアップ (xxx~)
;; -------------------------------------------
;; 実行の有無
(setq make-backup-files t)
;; 格納ディレクトリーの変更
;;   (対象ディレクトリー . 格納ディレクトリー) のリスト(今は全てを対象)l
(setq backup-directory-alist '((".*" . "~/.emacs.d/ehist")))


;; 番号付けによる複数保存
(setq version-control     t)  ;; 実行の有無
(setq kept-new-versions   5)  ;; 最新の保持数
(setq kept-old-versions   1)  ;; 最古の保持数
(setq delete-old-versions t)  ;; 範囲外を削除

;; auto-save 自動保存ファイル (#xxx#)
;; 正常終了時に削除
;; -------------------------------------------
;; ;; 実行の有無 (t/nil)
(setq auto-save-default t)

;;;; 格納ディレクトリーの変更
;;;;   (対象ファイルのパターン . 保存ファイルパス) のリスト
(setq auto-save-file-name-transforms
      (append auto-save-file-name-transforms
              '((".*" "~/.emacs.d/tmp/" t))))
;; 保存の間隔
(setq auto-save-timeout 10)     ;; 秒   (デフォルト : 30)
(setq auto-save-interval 100)   ;; 打鍵 (デフォルト : 300)

;; auto-save-list 自動保存のリスト  (~/.emacs.d/auto-save-list/.saves-xxx)
;; --------------------------------------------------------------------
;; 実行の有無 t/nil
(setq auto-save-list-file-prefix nil)
;; ;; 格納ディレクトリーの変更
;; (setq auto-save-list-file-prefix "~/tmp/.saves-")


;; lock ロックファイル (.#xxx)
;; -------------------------------------------
;; 実行の有無 t/nil
(setq create-lockfiles nil)

;;; ================= Loading Additional Configs =================
;;; Emacsをさらに強力に設定したい人へ!
;;; 玄人向けのEmacsの設定を(株)クリアコードのkou氏が下記URLで紹介してい
;;; ます。かなり勉強になります。
;;; http://www.clear-code.com/blog/2012/3/20.html
;;; 設定ファイルの最新版は、下記URLにあります。
;;; https://github.com/clear-code/emacs.d
;;; 興味があれば、このURLからinit.el以外のファイルを自分で取ってきてく
;;; ださい。必要なファイルを取ってきてから、以下のloadのコメントを外し
;;; て有効にしてください。

;; 標準Elispの設定
;(load "config/builtins")
;; 非標準Elispの設定
;(load "config/packages")
;; 個別の設定があったら読み込む
;(load "config/local" t)

;;;玄人向けのEmacsの設定として必要なファイルはこんな感じです。
;; .emacs.d
;; |-- init.el                ;; 基本的な設定を記述
;; |-- local.el               ;; （カスタマイズ用）
;; |-- config                 ;; 特定のモードや非標準のElispの設定をこの下に置く
;; |   |-- builtins.el        ;; 標準Elispの設定
;; |   |-- builtins           ;; 標準Elispのうち、設定が多くなるものはこの下に置く
;; |   |   |-- local.el       ;; （カスタマイズ用）
;; |   |   `-- cc-mode.el     ;; （例）標準Elispであるcc-modeの設定
;; |   |-- packages.el        ;; 非標準Elispの設定
;; |   |-- packages           ;; 非標準Elispのうち、設定が多くなるものはこの下に置く
;; |   |   |-- local.el       ;; （カスタマイズ用）
;; |   |   `-- sdic.el        ;; （例）非標準Elispであるsdicの設定
;; |   `-- el-get             ;; el-getの設定はこの下に置く
;; |       |-- recipies       ;; el-getのレシピはこの下に置く
;; |       `-- local-recipies ;; （カスタマイズ用）
;; `-- el-get                 ;; el-get管理のパッケージをこの下に置く

;;; ================= Custom Set Variables =================



;;設定ファイルの読み込み
(let ((load-files
       '(
         ;; "~/.emacs-d-/setting-packages.el"
         )))
  (dolist (file load-files)
    (load-file file)
    ))


;;矩形選択
(cua-mode t)
;;cua keybindの無効
(setq cua-enable-cua-keys nil)

;;; タブ機能を設定(キーバインドは別場所で設定)
(if (functionp 'global-tab-line-mode)
    (progn
      (global-tab-line-mode 1)
      (setq tab-bar-close-button-show 1)
      ))


;; 折り畳み
(add-hook 'c++-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'c-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'scheme-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'python-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'xml-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))

;; 配色テーマ
(load-theme 'wombat t)
(set-cursor-color "gold")

;; mac クリップボート共有
(if (eq system-type 'darwin)
    (progn
      (defun copy-from-osx ()
        (shell-command-to-string "pbpaste"))
      (defun paste-to-osx (text &optional push)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx)
      ))


;; spell check(Aspell)の設定
;; 参考：http://keisanbutsuriya.hateblo.jp/entry/2015/02/10/152543
;; aspellのインストールが必要
;; Aspellが日本語の辞書を探してしまうらしい。常に英語の辞書を使うようにするためfile作成
;; echo "lang en_US" >> ~/.aspell.conf
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;; (setq-default ispell-dictionary "english")
(setq ispell-extra-args '("--sug-mode=fast" "--run-together" "--run-together-limit=5" "--run-together-min=2"))
(mapc   ;; 以下flyspell-modeの設定
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 ;; ここに書いたモードではコメント領域のところだけflyspell-mode有効化
 '(
   ))
(mapc
   (lambda (hook)
     (add-hook hook
               '(lambda () (flyspell-mode 1))))
   ;; ここに書いたモードではflyspell-modeが有効化
   '(
     lisp-mode
     js2-mode
     php-mode
     web-mode
     ))

(provide 'first-setting)
;;; first-setting.el ends here
