;; -*- lexical-binding: t; -*-
;;; buffer-history.el --- window別のbuffer hisotoryを辿る関数 -*- coding: utf-8-unix -*-
;;; Commentary:
;;; Code:

(require 'my-util)

(defvar mybh-only-file-buffer t)

(let* ((buf-lst '())
       (last-window nil)
       (last-buffer nil)
       (display-base-buffer nil)
       (timer nil))

  (defun mybh-set-list (window buffers &rest arg)
    "WINDOWのbuffer listにBUFFERSを代入する"
    (setq buf-lst (plist-put buf-lst window buffers)))
  (defun mybh-get-list (window)
    "WINDOWのbuffer litを返す"
    (plist-get buf-lst window))

  (defun mybh-get-buf-lst ()
    "全てのbuffer listを返す"
    buf-lst)
  (defun mybh-set-buf-lst (lst)
    "buffer listにLSTを代入する"
    (setq buf-lst lst))

  (defun mybh-add-all-window-buffer-history()
    "操作しているframe内の全てのwindowのbuffer履歴を追加する"
    (interactive)
    (dolist (window (window-list))
      (let* ((buffer (window-buffer window))
             (buffer-lst (filter-list (lambda (buf) (and (not (equal buffer buf))
                                                         (buffer-live-p buf)))
                                      (mybh-get-list window))))
            ;; ファイルバッファ以外も含める、もしくはファイルバッファ
        (if (or (not mybh-only-file-buffer)
                (buffer-file-name buffer))
            (mybh-set-list window (cons buffer buffer-lst) "all-window"))))
    )

  (defun mybh-switch-prev-buffer ()
    "今操作しているwindowを前のbufferに切り替える"
    (interactive)
    (let* ((window (selected-window))
           (buffer (window-buffer window))
           (buffer-lst (filter-list (lambda (buf) (and (not (equal buffer buf))
                                                       (buffer-live-p buf)))
                                    (mybh-get-list window)))
           (prev-buffer (car buffer-lst)))
      (if prev-buffer
          (progn
            (mybh-set-list window
                           (append buffer-lst (list buffer)) "prev")
            (switch-to-buffer prev-buffer)
            (message (mybh-create-switch-message window)))
        (message "Not exist previous buffer"))))

  (defun mybh-switch-next-buffer ()
    "今のwindowを次のbufferに切り替える"
    (interactive)
    (let* ((window (selected-window))
           (buffer (window-buffer window))
           (buffer-lst (filter-list (lambda (buf) (and (not (equal buffer buf))
                                                       (buffer-live-p buf)))
                                    (mybh-get-list window)))
           (next-buffer (car (last buffer-lst))))
      (if next-buffer
          (progn
            (mybh-set-list window (append (list next-buffer buffer)
                                          (delete next-buffer buffer-lst)))
            (switch-to-buffer next-buffer)
            (message (mybh-create-switch-message window)))
        (message "Not exist next buffer"))
      ))

  (defun mybh-remove-buffer ()
    "buffer listからcurrent bufferを消す"
    (interactive)
    (let* ((window (selected-window))
           (buffer (window-buffer window))
           (buffer-lst (filter-list (lambda (b)
                                      (and (buffer-live-p b)
                                           (not (equal b (current-buffer)))))
                                    (mybh-get-list window))))
      (if buffer-lst
          (progn
            (switch-to-buffer (car buffer-lst))
            (mybh-set-list window buffer-lst))
        (message "failed removeing becouse not exist other buffer in current window's buffer list."))
    ))

  (defun mybh-create-switch-message (window)
    "bufferをスイッチしたときにミニッファバッファに表示するメッセージ作る"
    (let* ((buf-lst (mybh-get-list window))
           ;; display-base-bufferを先頭にしたlist.なければそのまま.
           (disp-buf-lst
            (if (and (buffer-live-p display-base-buffer)
                     (find display-base-buffer buf-lst))
                (let ((pred (lambda (buf) (equal buf display-base-buffer))))
                  (append (test-car-list pred buf-lst)
                          (list-to-pass-predicate pred buf-lst)))
              (progn
                (setq display-base-buffer (car buf-lst))
                buf-lst)))
           (cur-buf (current-buffer)))
      ;; buffer名を改行を挟めて繋げる
      ;; 今表示しているバッファ名の文字色を赤に
      (reduce (lambda (s1 s2) (concat s1
                                      (propertize " | " 'face '(:foreground "green"))
                                      s2))
              (mapcar (lambda (buf)
                        (let ((buf-name (buffer-name buf)))
                          (if (equal cur-buf buf)
                              (propertize buf-name 'face '(:foreground "red"))
                            buf-name)))
                      (reverse disp-buf-lst)))))

  (defun mybh-start-rec-timer ()
      (setq timer
            (run-at-time 1 1
                         (lambda ()
                           (if (or (not (equal last-window (selected-window)))
                                   (not (equal last-buffer (current-buffer))))
                               (mybh-add-all-window-buffer-history))
                           (setq last-window (selected-window))
                           (setq last-buffer (current-buffer))))))

  (defun mybh-stop-rec-timer ()
      (if (timerp timer)
          (cancel-timer timer)))

  (mybh-start-rec-timer)
  )

(provide 'buffer-history)
;;; buffer-history.el ends here
