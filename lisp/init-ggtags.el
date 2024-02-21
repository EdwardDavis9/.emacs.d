(require 'ggtags)

;; 使用流程
;
; 在项目目录下，执行命令生成 tags 文件。先使用 ggtags 提供的命令 ggtags-create-tags 来生成 tags 文件， C-c g c
;
;
; M-. 是 Emacs 中的标准命令之一，通常用于跳转到定义处。
; M-, 也是 Emacs 中的标准命令之一，通常用于跳转到定义的上一个位置（即跳回）
;
; ggtags-find-definition 是 ggtags 插件提供的一个命令，用于通过 ggtags 进行代码导航，跳转到定义处
; pop-tag-mark 是 Emacs 内建的命令，用于返回上一个标记位置。
; 在使用 M-. 或 ggtags-find-definition 等命令跳转到定义后，你可以使用 pop-tag-mark 返回到跳转前的位置。
;
; 综上所述，M-, 和 M-. 是 Emacs 的标准命令，用于代码导航；
; 而 ggtags-find-definition 是 ggtags 提供的一个特定于 GNU Global 的命令，用于通过 ggtags 进行代码导航；
; pop-tag-mark 是 Emacs 内建的命令，用于返回上一个标记位置，通常用于与跳转命令配合使用。
;



;; Set ggtags configuration variables
(setq ggtags-enable-navigation-keys nil
      ggtags-use-idutils t
      ggtags-global-abbreviate-filename nil
      ggtags-sort-by-nearness t)

;; Enable ggtags for specific modes
(add-hook 'c-mode-common-hook #'ggtags-mode)
(add-hook 'python-mode-hook #'ggtags-mode)
(add-hook 'java-mode-hook #'ggtags-mode)
(add-hook 'go-mode-hook #'ggtags-mode)
(add-hook 'asm-mode-hook #'ggtags-mode)
;; Add more modes as needed

;; Optimize for large projects (optional)
(setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB

;; Keybindings (optional)  ggtags-navigation-map
;(define-key ggtags-navigation-map (kbd "M-]") 'ggtags-find-definition)
(define-key ggtags-mode-map (kbd "M-]") 'ggtags-find-definition)
(define-key ggtags-mode-map (kbd "M-[") 'pop-tag-mark)
;(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

;; Optimize compilation process (optional)
;(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
;(setq-local company-backends (remove 'company-gtags company-backends))
;(add-to-list 'company-backends 'company-gtags)

;; Disable unnecessary features (optional)
;(setq ggtags-eldoc-function nil
;    ggtags-navigation-mode-lighter nil
;      ggtags-show-children-mode-lighter nil)

;;; Additional languages
;(add-hook 'go-mode-hook (lambda ()
;                          (setq-local ggtags-executable "gopkgs")
;                          (ggtags-mode 1)))

;; Python specific configurations
;(add-hook 'python-mode-hook (lambda ()
;                             ;; Add Python-specific ggtags configurations here
;                             (ggtags-mode 1)))

;;; Lua specific configurations
;(add-hook 'lua-mode-hook (lambda ()
;                          ;; Add Lua-specific ggtags configurations here
;                          (ggtags-mode 1)))

;;; Add more language support as needed


(provide 'init-ggtags)
