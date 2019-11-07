;;; Code:

(defun assoc-val (key lst)
  "LST連想配列からKEYに紐付く要素を返す."
  (cdr (assoc key lst)))

(defun my-point-history-distant-p (lst1 lst2)
  "連想配列を2つ引数とする. bufferが同じで行が遠ければt"
  (and (equal (assoc-val :buffer lst1) (assoc-val :buffer lst2))
       (> (abs (- (assoc-val :line lst1) (assoc-val :line lst2)))
          my-point-history-line-range)))

(defun test-car-list (predicate lst)
  "LSTの要素でPREDICATEがnot-nilとなる要素を先頭とするリストを返す."
  (if lst
      (if (funcall predicate (car lst))
          (progn
            lst)
        (test-car-list predicate (cdr lst)))
    nil))

(unless (functionp 'current-line)
  (defun current-line ()
    (1- (line-number-at-pos))
    ))

(unless (functionp 'some)
  (defun some (test lst)
    (if lst
        (or (funcall test (car lst))
            (some test (cdr lst))))))

(defun filter-list (predicate lst)
  "LSTの各要素をPREDICATEで評価して非nilとなる要素のリストを返す.
(filter-list 'numberp '( 1 2 3 \"4\" 5 6))
=> (1 2 5 6)"
  (if lst
      (if (funcall predicate (car lst))
          (cons (car lst) (filter-list predicate (cdr lst)))
        (filter-list predicate (cdr lst)))
    nil))
;; (filter-list 'numberp '( 1 2 3 "4" 5 6))

(defun list-to-pass-predicate (predicate lst)
  "LSTを先頭から走査してPREDICATEが非nilを返したら終了して、それまでのリストを返す
全ての要素がnilを返したらLSTを返す
(list-to-pass-predicate (lambda (x) (= x 4)) '(1 2 3 4 5 6))
=> (1 2 3)"
  (and lst
       (not (funcall predicate (car lst)))
       (cons (car lst) (list-to-pass-predicate predicate (cdr lst)))))

(provide 'my-util)
;;; my-util.el ends here
