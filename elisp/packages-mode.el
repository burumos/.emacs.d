;;; Code:

;; markdown
(my/manage-package 'markdown-mode
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
                   )

;;php-mode
(my/manage-package 'php-mode
                   (require 'php-mode)
                   )

(my/manage-package 'web-mode
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
                   )

;; ocaml用
(my/manage-package 'merlin
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
(my/manage-package 'rjsx-mode
                   (require 'rjsx-mode)
                   (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
                   )


;; javascript IDE
(my/manage-package 'js2-mode
                   (require 'js2-mode)
                   (if (package-installed-p 'rjsx-mode)
                       (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
                     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
                   (add-hook 'js2-mode-hook
                             (lambda ()
                               (setq my-js-mode-indent-num 2)
                               (setq js2-basic-offset my-js-mode-indent-num)
                               (setq js-switch-indent-offset my-js-mode-indent-num)
                               )))

;; typescript mode
(my/manage-package 'typescript-mode
                   (require 'typescript-mode)
                   ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
                   )

;; angular
(my/manage-package'ng2-mode
      (require 'ng2-mode)
      )

;; vue mode
(my/manage-package 'vue-mode
                   (require 'vue-mode)
                   (add-hook 'mmm-mode-hook
                             (lambda ()
                               (set-face-background 'mmm-default-submode-face nil)))
                   )

;; json
(my/manage-package 'json-mode
                   (require 'json-mode)
                   )

;; go-mode
(my/manage-package 'go-mode
                   (require 'go-mode)
                   )

;; yaml
(my/manage-package 'yaml-mode
                   (require 'yaml-mode))

;; dockerfile
(my/manage-package 'dockerfile-mode
                   (require 'dockerfile-mode))

(provide 'packages-mode)
;;; packages-mode.el ends here
