;;; lexical-bind.el --- make lexical symbol

;; Author: acple <silentsphere110@gmail.com>

;; Keywords: let
;; Version: 0.0.7

;;; Commentary:
;;
;; lexical-letの超強化版
;;
;; lexical-letとの違い
;; マクロ展開が超高速
;; コンパイルされた関数の実行が超高速
;; シンプルにレキシカルなシンボルを作成するだけなので自由度が高く扱いやすい
;; 宣言したシンボルで関数を作るとレキシカル関数を作ることができる
;; 作ったレキシカル変数をさらにバッファローカルにすることもできる

;;; Code:

((macro
  lambda ()
  (let ((symbol-alist (make-symbol "-"))
        (value-alist (make-symbol "-"))
        (symbol-name-alist (make-symbol "-"))
        (make-symbol-alist (make-symbol "-"))
        (replace-symbol (make-symbol "-"))
        (make-defvalue-list (make-symbol "-"))
        (make-symbol-definition (make-symbol "-"))
        (x (make-symbol "x")))
    `(let nil
       (defvar ,symbol-alist)
       (defvar ,value-alist)
       (defvar ,symbol-name-alist)
       (declare-function ,make-symbol-alist nil)
       (declare-function ,replace-symbol nil)
       (declare-function ,make-defvalue-list nil)
       (declare-function ,make-symbol-definition nil)
       (fset ',make-symbol-alist
             #'(lambda (x)
                 (setq x (if (consp x) x (list x nil)))
                 (add-to-list ',value-alist x t
                              #'(lambda (x y) (eq (car x) (car y))))
                 (list (car x)
                       (list 'make-symbol
                             (concat (symbol-name (car x)) "-lx")))))
       (fset ',replace-symbol
             #'(lambda (sexp)
                 (when sexp
                   (mapcar #'(lambda (x)
                               (cond ((consp x)
                                      (,replace-symbol x))
                                     ((cdr (assq x ,symbol-name-alist)))
                                     (x)))
                           sexp))))
       (fset ',make-defvalue-list
             #'(lambda (,x)
                 (mapcar #'(lambda (,x)
                             (prog1
                                 (cons (car ,x) (,replace-symbol (cdr ,x)))
                               (add-to-list ',symbol-name-alist
                                            (cons (car ,x)
                                                  (symbol-value (car ,x))))))
                         ,x)))
       (fset ',make-symbol-definition
             #'(lambda ()
                 (apply
                  'nconc
                  (mapcar
                   #'(lambda (,x)
                       (list
                        (list 'defvar (symbol-value (car ,x)))
                        (list 'setq (symbol-value (car ,x)) (cadr ,x))
                        (list 'declare-function (symbol-value (car ,x)) nil)
                        (list 'fset (list 'quote (symbol-value (car ,x)))
                              (list 'quote (car ,x)))))
                   (,make-defvalue-list ,value-alist)))))
       (defmacro lexical-bind (lexical-sym &rest body)
         "use like `let*'.\n\n\(fn ((SYMBOL-NAME VALUE)...) BODY...)"
         (declare (indent 1))
         (setq ,value-alist nil
               ,symbol-name-alist nil
               ,symbol-alist (mapcar #',make-symbol-alist
                                     lexical-sym))
         (eval (list 'let ,symbol-alist
                     '(append '(let nil)
                              (,make-symbol-definition)
                              '(nil)
                              (,replace-symbol body)))))))))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(lexical-bind\\)"
                           1 'font-lock-keyword-face)))

(provide 'lexical-bind)

;;; lexical-bind.el ends here
