 ;;; -*- coding: utf-8; lexical-binding: t; -*-

 ;; Lazy load company
(autoload 'global-company-mode "company" nil t)

;; Customize company settings
(eval-after-load 'company
  '(progn
     (setq company-minimum-prefix-length 1
           company-show-quick-access t)))

;; Enable company globally after Emacs has started
(add-hook 'after-init-hook 'global-company-mode)
 ;;;

 ;; Load lsp-mode
(require 'lsp-mode)


;; Define a function for LSP actions before save
(defun lsp-save-actions ()
  "LSP actions before save."
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

;; Add hooks and configuration for lsp-mode
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'lsp-mode-hook #'lsp-save-actions)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'java-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'lsp-deferred)
;; (add-hook 'vue-mode-hook #'lsp-deferred)

;; Configure lsp-mode
(setq lsp-auto-guess-root t
      lsp-headerline-breadcrumb-enable nil
      lsp-keymap-prefix "C-c l"
      lsp-log-io nil)

;; Define key in lsp-mode-map
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)





 ;; Load lsp-ui
(require 'lsp-ui)

;; Set variables for lsp-ui
(setq lsp-ui-doc-include-signature t
      lsp-ui-doc-position 'at-point
      lsp-ui-sideline-ignore-duplicate t)

;; Add hooks for lsp-ui
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
(add-hook 'lsp-ui-mode-hook #'lsp-modeline-code-actions-mode)

;; Configure key bindings in lsp-ui-mode-map
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)



;; Load dap-mode
(require 'dap-mode)

;; Add hooks for dap-mode
(add-hook 'lsp-mode-hook 'dap-mode)
(add-hook 'dap-mode-hook 'dap-ui-mode)
(add-hook 'dap-mode-hook 'dap-tooltip-mode)

;; Add hooks for language-specific configurations
(add-hook 'python-mode-hook (lambda() (require 'dap-python)))
(add-hook 'go-mode-hook (lambda() (require 'dap-go)))
(add-hook 'java-mode-hook (lambda() (require 'dap-java)))
(add-hook 'java-mode-hook (lambda() (require 'dap-lldb)))



;; Load flycheck
(require 'flycheck)

;; Enable global-flycheck-mode
(add-hook 'after-init-hook 'global-flycheck-mode)



;; Load company
(require 'company)

;; Enable global-company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; Customize company settings
(setq company-minimum-prefix-length 1
      company-show-quick-access t)


;; LSP ggtags integration
;;(require 'lsp-ggtags)
;;(add-hook 'lsp-mode-hook #'lsp-ggtags-mode) ; 在 LSP 模式下启用 LSP ggtags 整合



;; Optional: Configure company-lsp backend
;;(require 'company-lsp)
;;(push 'company-lsp company-backends)        ; 将 company-lsp 添加到 company 的后端列表


;; Optional: Treemacs integration
(require 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)                  ; 启用 LSP Treemacs 整合

;; Optional: Project management with Projectile
(require 'projectile)
(projectile-mode)                           ; 启用 Projectile 项目管理

;; Optional: Helm integration
;;(require 'helm-lsp)
;;(global-set-key (kbd "M-x") #'helm-lsp-workspace-symbol) ; 设置 Helm-LSP 工作空间符号的键绑定

 (provide 'init-lsp)
