;;; Code:

;; markdown
(leaf markdown-mode
  :ensure t
  :config
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
  )

;;php-mode
(leaf php-mode
  :ensure t)

(leaf web-mode
  :ensure t
  :hook (web-mode-hook
         . (lambda ()
               ;; インデント
             (set-variable 'web-mode-attr-indent-offset t)
             (set-variable 'web-mode-markup-indent-offset 2)
             (set-variable 'web-mode-css-indent-offset 2)
             (set-variable 'web-mode-code-indent-offset 2)
             (set-variable 'web-mode-sql-indent-offset 2)
             (set-variable 'indent-tabs-mode nil)
             (set-variable 'tab-width 2)
             ))
  :custom (;; cosoleでも閉じタグを自動補完
           (web-mode-auto-close-style . 1)
           (web-mode-tag-auto-close-style . t)
           (web-mode-enable-auto-pairing . t))
  :bind (:web-mode-map ("C-c '" . web-mode-element-close))
  :config
  ;; web-modeで開くファイル
  (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
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
   ))

;; ocaml用
(leaf merlin
  :ensure t
  :config
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t))
    ;; Make company aware of merlin
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))
    ;; company on merlin managed buffers
    (add-hook 'merlin-mode-hook 'company-mode)
    ;; Or enable it globally:
    ;; (add-hook 'after-init-hook 'global-company-mode))
    ))

;; jsx
(leaf rjsx-mode
  :ensure t
  :mode  "\\.js\\'"
  )

;; javascript IDE
(leaf js2-mode
  :ensure t rjsx-mode
  :mode "\\.js\\'"
  :hook (js2-mode-hook
         . (lambda ()
             (set-variable 'my-js-mode-indent-num 4)
             (set-variable 'js2-basic-offset 4)
             (set-variable 'js-switch-indent-offset 4)
             )))

;; typescript mode
(leaf typescript-mode
  :ensure t
  ;; :mode "\\.tsx\\'"
  )

;; vue mode
(leaf vue-mode
  :ensure t
  :hook (mmm-mode-hook
         . (lambda ()
             (set-face-background 'mmm-default-submode-face nil))))

;; json
(leaf json-mode
  :ensure t)

;; go-mode
(leaf go-mode
  :ensure t)

;; yaml
(leaf yaml-mode
  :ensure t)

;; dockerfile
(leaf dockerfile-mode
  :ensure t)

;; clojure
(leaf cider
  :ensure t)

;; rust
(leaf rust-mode
  :ensure t
  :hook
  (rust-mode-hook
   . (lambda () (setq indent-tabs-mode nil)))
  :custom ((rust-format-on-save . t))
  :bind (:rust-mode-map ("C-c C-c t" . rust-run))
  )

;; swift
(leaf swift-mode
  :ensure t)


(provide 'packages-mode)
;;; packages-mode.el ends here
