;; -*- coding: utf-8; lexical-binding: t; -*-



(require 'cnfonts)


;; 让 cnfonts 在 Emacs 启动时自动生效。
(cnfonts-mode 1)
;; 添加两个字号增大缩小的快捷键
(define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
(define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)



;;; 设置字号随着行距自动调整
(defvar my-line-spacing-alist
      '((9 . 0.1) (10 . 0.9) (11.5 . 0.2)
        (12.5 . 0.2) (14 . 0.2) (16 . 0.2)
        (18 . 0.2) (20 . 1.0) (22 . 0.2)
        (24 . 0.2) (26 . 0.2) (28 . 0.2)
        (30 . 0.2) (32 . 0.2)))

(defun my-line-spacing-setup (fontsizes-list)
  (let ((fontsize (car fontsizes-list))
        (line-spacing-alist (copy-list my-line-spacing-alist)))
    (dolist (list line-spacing-alist)
      (when (= fontsize (car list))
        (setq line-spacing-alist nil)
        (setq-default line-spacing (cdr list))))))

(add-hook 'cnfonts-set-font-finish-hook #'my-line-spacing-setup)
;;;


(provide 'init-cnfonts)
