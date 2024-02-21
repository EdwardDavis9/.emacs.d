;; 初始化软件包管理器
(require 'package)
(unless (bound-and-true-p package--initialized)
    (package-initialize))


;; 刷新软件源索引
(unless package-archive-contents
    (package-refresh-contents))


;; 第一个扩展插件：use-package，用来批量统一管理软件包
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))


;; `use-package-always-ensure' 避免每个软件包都需要加 ":ensure t"
;; `use-package-always-defer' 避免每个软件包都需要加 ":defer t"
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)


;;; Code:
(use-package restart-emacs)

;Test start time
(use-package benchmark-init
  ;;:ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
             ;:init (benchmark-init/activate)
             ;:hook (after-init . benchmark-init/deactivate))

;alias setting
(use-package emacs
             :config (defalias 'yes-or-no-p 'y-or-n-p))

;Delete  current line
;;(use-package crux
;;             :bind ("C-c k" . crux-smart-kill-line))

;;(use-package drag-stuff
;;             :bind (("<M-up>" . drag-stuff-up)
;;                    ("<M-down>" . drag-stuff-down)))

;;(use-package ivy
;;  :defer 1
;;  :demand
;;  :hook (after-init . ivy-mode)
;;  :config
;;  (ivy-mode 1)
;;  (setq ivy-use-virtual-buffers t
;;        ivy-initial-inputs-alist nil
;;        ivy-count-format "%d/%d "
;;       enable-recursive-minibuffers t
;;        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))
;;  ;(ivy-posframe-mode 1))

;;(use-package counsel
;;  :after (ivy)
;;  :bind (("M-x" . counsel-M-x)
;;         ("C-x C-f" . counsel-find-file)
;;         ("C-c f" . counsel-recentf)
;;         ("C-c g" . counsel-git)))

;search list, we can chose list to select
;;(use-package swiper
;;  :after ivy
;;  :bind (("C-s" . swiper)
;;         ("C-r" . swiper-isearch-backward))
;;  :config (setq swiper-action-recenter t
;;;                swiper-include-line-number-in-search t))

;auto complete code
(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access t))
;(use-package company
;  :ensure t
;  :init (global-company-mode)
;  :config
;  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
;  (setq company-tooltip-align-annotations t)
;  (setq company-idle-delay 0.5)
;  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
;  (setq company-selection-wrap-around t))
  ;(setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉


;icon
;(use-package company-box
;  :ensure t
;  :if window-system
;  :hook (company-mode . company-box-mode))

;gramar check
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

;Ai code
;(use-package company-tabnine
;  :ensure t
;  :init (add-to-list 'company-backends #'company-tabnine))

;key means
;;(use-package which-key
;;  :defer nil;不需要延迟加载
;;;  :config (which-key-mode));使用which-key-mode

;Minibuffers 移到屏幕中间, use package-install ivy-postframe
;; display at `ivy-posframe-style'
;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
 ;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
 ;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
 ;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;(ivy-posframe-mode 1)

; quick switch window
;; (use-package ace-window
;;  :bind (("M-o" . 'ace-window)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (defun lsp-save-actions ()
    "LSP actions before save."
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
	(add-hook 'before-save-hook #'lsp-format-buffer t t))
  :hook ((lsp-mode . #'lsp-enable-which-key-integration)
         (lsp-mode . #'lsp-save-actions)
         ((c-mode
           c++-mode
           go-mode
           java-mode
           js-mode
           python-mode
           rust-mode
           web-mode) . lsp-deferred))
  :config
  (setq lsp-auto-guess-root t
	    lsp-headerline-breadcrumb-enable nil
	    lsp-keymap-prefix "C-c l"
	    lsp-log-io nil)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
    :after lsp-mode
    :init
    (setq lsp-ui-doc-include-signature t
	      lsp-ui-doc-position 'at-point
          lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (add-hook 'lsp-ui-mode-hook 'lsp-modeline-code-actions-mode)
    :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;debug
(use-package dap-mode
    :init
    (add-hook 'lsp-mode-hook 'dap-mode)
    (add-hook 'dap-mode-hook 'dap-ui-mode)
    (add-hook 'dap-mode-hook 'dap-tooltip-mode)
     (add-hook 'python-mode-hook (lambda() (require 'dap-python)))
     (add-hook 'go-mode-hook (lambda() (require 'dap-go)))
     (add-hook 'java-mode-hook (lambda() (require 'dap-java)))
    (add-hook 'java-mode-hook (lambda() (require 'dap-lldb)))
    )


(provide 'init-use-package)
;;; init-use-package.el ends here
