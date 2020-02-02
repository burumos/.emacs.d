;;; Code:
;;;kill ringへの保存なしの単語削除
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun my-delete-word (arg)
  "My delete forward word."
  (interactive "p")
  (let* ((proc-length 100)
         (string-after-point (if (> 0 arg)
                                 (reverse (buffer-substring-no-properties
                                           (point) (max (point-min) (- (point) proc-length))))
                               (buffer-substring (point)
                                                 (min (point-max) (+ (point) proc-length)))))
         (string-on-point (if (= 0 (length string-after-point))
                              ""
                            (substring string-after-point 0 1))))
    (cond
     ((equal string-on-point "") nil)
     ((or (and (<= 2 (length string-after-point))
               (equal (substring string-after-point 0 2) "  "))
          (equal string-on-point "	"))
      (let ((count 0))
        (loop
         (setq count (1+ count))
         (when (or (>= count (string-width string-after-point))
                   (not (equal string-on-point
                               (substring string-after-point count (1+ count)))))
           (return)))
        (delete-char (if (> 0 arg) (- count) count))
        (cond
         ((<= arg -2) (my-delete-word (1+ arg)))
         ((>= arg 2) (my-delete-word (1- arg))))))
     ((equal string-on-point "\n")
      (delete-char (if (> 0 arg) -1 1))
      (cond
       ((<= arg -2) (my-delete-word (1+ arg)))
       ((>= arg +2) (my-delete-word (1- arg)))))
     (t (delete-region (point) (progn (forward-word arg) (point)))))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-comment-dwim ()
  "My comment dwim."
  (interactive)
  (let* ((end-point-of-line (save-excursion
                              (end-of-line)
                              (point)))
         (beginning-point-of-line (save-excursion
                                    (beginning-of-line)
                                    (point)))
         (is-not-empty-line ;; 空白文字以外を含むか
          (string-match "\[^\s\]" (buffer-substring
                                   beginning-point-of-line
                                   end-point-of-line))))
    (if (and (not (region-active-p))
             is-not-empty-line)
            (save-excursion
              (comment-line 1))
      (comment-dwim nil))))


;; for test
;; (define-key global-map (kbd "M-a") 'my-delete-word)
;; (define-key global-map (kbd "M-z") 'my-backward-delete-word)


;; go-modeでcompanyを使って補完すると補完直前の文字が
;; 補完した語句の後に来てしまうため応急処置
(setq company-completion-finished-hook
      '(lambda (s)
         (lexical-let ((str (buffer-substring (line-beginning-position) (point))))
           (run-at-time "0.1 sec" nil
                        (lambda ()
                          (if (equal major-mode "go-mode")
                              (let ((pnt (point)))
                                (goto-char (line-beginning-position))
                                (delete-char (length str))
                                (insert str)
                                (goto-char pnt)
                                ))
                          )
                        )
           )))

;;;;;;;
(defvar my-point-history-list '())
;; (setq my-point-history-list '())
(defvar my-point-history-limit 30)
;; (setq my-point-history-limit 30)
(defvar my-point-history-line-range 10)
(defvar my-point-history-latest-line nil)
(defvar my-point-history-back-history nil "Hisotryでbackで戻ったList.")
(defvar my-point-history-sublist nil)
(defvar my-point-history-timer nil)

(defun my-point-history-rec-start ()
  "Timer start."
  (interactive)
  (my-point-history-rec-stop)
  (setq my-point-history-timer
        (run-at-time
         1 2 (lambda ()
               (if (buffer-file-name (current-buffer))
                   (let ((latest-hist (car my-point-history-list))
                         (current (list (cons :buffer (current-buffer)) (cons :line (current-line)))))
                     (if (or (not latest-hist)
                             (and (equal (assoc-val :buffer my-point-history-latest-line)
                                         (assoc-val :buffer current))
                                  (not (equal (assoc-val :buffer current)
                                              (assoc-val :buffer (car my-point-history-list)))))
                             (and (not (my-point-history-distant-p my-point-history-latest-line current))
                                  (my-point-history-distant-p latest-hist current)))
                         (progn
                           (setq my-point-history-list
                                 (cons current my-point-history-list))
                           ;; 履歴のredoのための変数を初期化
                           (setq my-point-history-back-history nil)
                           (setq my-point-history-sublist nil)
                           ;; 履歴長を制限
                           (if (> (length my-point-history-list) my-point-history-limit)
                               (setcdr (nthcdr (1- my-point-history-limit) my-point-history-list) nil))))
                     (setq my-point-history-latest-line current)
                     ))))))

(defun my-point-history-rec-stop ()
  "Stop timer."
  (interactive)
  (if (timerp my-point-history-timer)
      (cancel-timer my-point-history-timer)))

(defun my-point-history-back ()
  "Back point."
  (interactive)
  (if my-point-history-list
      (let ((history
             (test-car-list
              (lambda (lst)
                (some (lambda (buffer)
                        (equal (assoc-val :buffer lst) buffer))
                      (buffer-list)))
              (or my-point-history-sublist my-point-history-list))))
        (let ((pnt (car history)))
          (switch-to-buffer (assoc-val :buffer pnt))
          (goto-char (point-min))
          (forward-line (assoc-val :line pnt))
          (setq my-point-history-back-history (cons pnt my-point-history-back-history))
          (setq my-point-history-sublist (cdr history))
          (message "back IF: %s  LN: %d %s"
                   (buffer-name (assoc-val :buffer pnt))
                   (assoc-val :line pnt)
                   (if (equal pnt (car my-point-history-list)) "HISTORY_FIRST" ""))))
    (message "Point history is empty")))

(defun my-point-history-go ()
  "Go point if gone back history point."
  (interactive)
  (if my-point-history-back-history
      (let ((pnt (car my-point-history-back-history)))
        (switch-to-buffer (assoc-val :buffer pnt))
        (goto-char (point-min))
        (forward-line (assoc-val :line pnt))
        (setq my-point-history-back-history (cdr my-point-history-back-history))
        (setq my-point-history-sublist (cons pnt my-point-history-sublist))
        (message "FILE: %s LN: %d" (buffer-name (assoc-val :buffer pnt)) (assoc-val :line pnt)))
    (message "Back history is none.")))

(my-point-history-rec-start)
;;;;;;;

(defvar is-c-m-indent t)

(defun c-m ()
  "C-mの動作を定義する."
  (interactive)
  (if is-c-m-indent
      (newline-and-indent)
    (newline)))

(defun toggle-c-m-indent ()
  "is-c-m-indentをtoggleする"
  (interactive)
  (setq is-c-m-indent (not is-c-m-indent)))


;; debug関数 別バッファにメッセージを流す
(defvar debug-message-buffer-name "debug-message-buffer")
(defun debug-message (message &rest args)
  "Output message to debug buffer."
  (get-buffer-create debug-message-buffer-name)
  ;; debug bufferを画面上で開いていなかったら開く
  (if (not (some (lambda (window)
                   (equal (buffer-name window) debug-message-buffer-name))
                 (mapcar 'window-buffer (window-list))))
      (let ((cur-win (selected-window)))
        (switch-to-buffer-other-window debug-message-buffer-name)
        (goto-char (max-char))
        (select-window cur-win)))
  (save-excursion
    (set-buffer debug-message-buffer-name)
    (insert (concat (apply 'format (cons message args)) "\n")))
  (let ((debug-win (get-buffer-window debug-message-buffer-name)))
    (set-window-point debug-win (buffer-size (window-buffer debug-win)))))


;; how to use ---------------
;; (debug-message "aaa %s" "test")

(provide 'my-command)
;;; my-command.el ends here
