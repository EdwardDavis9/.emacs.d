;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Comment

;; Emacs 版本检查：

;; 检查 Emacs 版本是否符合要求（26.1 及以上）。
;; 加载 elpa-mirror（包管理器镜像）：

;; 将 elpa-mirror 所在的目录添加到加载路径。
;; 定义了一个函数 refresh-all-packages，用于刷新包列表。
;; 系统平台检查：

;; 设置一系列变量来表示系统平台，如 macOS、Windows、Linux 等。
;; GC 调整：

;; 在启动时临时调整垃圾回收参数，以提高启动速度。
;; 初始化目录变量：

;; 定义了一些常量，表示 Emacs 配置目录、site-lisp 目录、lisp 目录等。
;; 配置文件加载：

;; 加载初始化相关的配置文件，如自动加载的模块（init-autoload）、模型行（init-modeline）、工具（init-utils）、文件类型（init-file-type）、ELPA 配置（init-elpa）等。
;; 将 site-lisp/ 目录下的子目录添加到 load-path 中。
;; 针对不同的模块，调用 require-init 函数加载对应的初始化文件。
;; 轻量模式与 GC 调整：

;; 在轻量模式下，跳过部分模块的加载。
;; 临时调整 GC 参数。
;; 初始化自定义设置：

;; 在非轻量模式下，加载个人的自定义设置，如颜色主题等。
;; 设置 custom-file 和加载自定义设置文件。
;; GC 优化：

;; 在 Emacs 启动后，通过定时器再次调整 GC 参数以优化性能。
;; 输出启动信息：

;; 在 Emacs 启动完成后输出启动耗时和 GC 次数。
;; 局部变量设置：

;; 设置局部变量，如 no-byte-compile。
;; 可选功能启用：
;; 启用 erase-buffer 函数。

;C-x C-e 执行emacs代码
;C-h f查找函数
;C-h C-f
;C-h v搜索变量
;
;停留在一个变量上，C-h v查看当前变量的信息
;C-x C-h 查看键绑定
;
;M-x eval-buber不重启emacs,执行emacs配置代码
;
;C-h C-f find-function 查看源码, 查看订阅的快捷键
;
;super-c super-v复制粘贴代码，好像是改键后
;
;(set-face-attribute 'default nil :height 160)



;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
(package-initialize)

;; check emacs version
(let* ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required" minver)))

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(defvar my-debug nil "Enable debug mode.")

;;Disable emacs's prompt bell
(setq ring-bell-function 'ignore)

;; load the emacs elpa-mirror
(add-to-list 'load-path "~/.emacs.d/site-lisp/elpa-mirror")
(require 'elpa-mirror)

;; 定义函数，用于自动刷新包列表, 加上这个，emacs启动时自动刷新包列表，特别特别慢
;;(defun refresh-all-packages ()
;;  "Refresh all packages in the package.el system."
;;  (interactive)
;;  (package-refresh-contents)
;;  (message "Package refresh complete."))

;; 调用函数，执行刷新操作
;;(refresh-all-packages)

;;快速打开配置文件
(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f8>") 'open-init-file)

;; 加载 benchmark-init 模块
(require 'benchmark-init)

;; 在 `after-init-hook` 中添加钩子
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;;;增强min-buffer显示的内容，包含文件其他属性信息等
;(package-install 'marginalia)
(marginalia-mode t)
;;;

;;;
;(package-install 'embark)
;(global-set-key (kbd "C-;") 'embark-act)
;;;

;;set SHELL
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name explicit-shell-file-name)
(setq explicit-bash-args '("--noediting" "--login" "-i"))
(setq tramp-verbose 10) ;; 如果你使用 TRAMP 模式连接远程主机的话，增加此行
;(setenv "TERM" "xterm-256color")

;;; 高亮当前行
(global-hl-line-mode 1)

;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; set OS variables
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs28* (>= emacs-major-version 28))


;; don't GC during startup to save time
(unless (bound-and-true-p my-computer-has-smaller-memory-p)
  (setq gc-cons-percentage 0.6)
  (setq gc-cons-threshold most-positive-fixnum))


;; {{ emergency security fix
;; https://bugs.debian.org/766397
(with-eval-after-load 'enriched
  (defun enriched-decode-display-prop (start end &optional param)
    (list start end)))
;; }}

(setq *no-memory* (cond
                   (*is-a-mac*
                    ;; @see https://discussions.apple.com/thread/1753088
                    ;; "sysctl -n hw.physmem" does not work
                    (<= (string-to-number (shell-command-to-string "sysctl -n hw.memsize"))
                        (* 4 1024 1024)))
                   (*linux* nil)
                   (t nil)))

(defconst my-emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d.")

(defconst my-site-lisp-dir (concat my-emacs-d "site-lisp")
  "Directory of site-lisp.")

(defconst my-lisp-dir (concat my-emacs-d "lisp")
  "Directory of personal configuration.")

;; Light weight mode, fewer packages are used.
(setq my-lightweight-mode-p (and (boundp 'startup-now) (eq startup-now t)))

(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not my-lightweight-mode-p))
    (load (file-truename (format "%s/%s" my-lisp-dir pkg)) t t)))

(defun my-add-subdirs-to-load-path (lisp-dir)
  "Add sub-directories under LISP-DIR into `load-path'."
  (let* ((default-directory lisp-dir))
    (setq load-path
          (append
           (delq nil
                 (mapcar (lambda (dir)
                           (unless (string-match "^\\." dir)
                             (expand-file-name dir)))
                         (directory-files lisp-dir)))
           load-path))))

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let* ((file-name-handler-alist nil))

  (require-init 'init-autoload)
  ;; `package-initialize' takes 35% of startup time
  ;; need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
  (require-init 'init-modeline)
  (require-init 'init-utils)
  (require-init 'init-file-type)
  (require-init 'init-elpa)

  ;; make all packages in "site-lisp/" loadable right now because idle loader
  ;; are not used and packages need be available on the spot.
  (when (or my-lightweight-mode-p my-disable-idle-timer)
    (my-add-subdirs-to-load-path (file-name-as-directory my-site-lisp-dir)))

  ;; Any file use flyspell should be initialized after init-spelling.el
  (require-init 'init-spelling t)
  (require-init 'init-ibuffer t)
  (require-init 'init-bookmark)
  (require-init 'init-ivy)
  (require-init 'init-windows)
  (require-init 'init-javascript t)
  (require-init 'init-org t)
  (require-init 'init-python t)
  (require-init 'init-lisp t)
  (require-init 'init-yasnippet t)
  (require-init 'init-cc-mode t)
  (require-init 'init-linum-mode)

  ;(set-fringe-mode 0)
  ;(require-init 'init-cmake-ide t)
  ;;没有和并好，本Emacs配置中存在一个MakeFile,但目标仓库也存在一个MakeFile代码
  ;;@see https://github.com/atilaneves/cmake-ide

  (require-init 'init-git)
  (require-init 'init-gtags t)
  (require-init 'init-clipboard)
  (require-init 'init-ctags t)
  (require-init 'init-gnus t)
  (require-init 'init-lua-mode t)
  (require-init 'init-term-mode)
  (require-init 'init-web-mode t)
  (require-init 'init-company t)
  (require-init 'init-chinese t) ;; cannot be idle-required
  ;; need statistics of keyfreq asap
  (require-init 'init-keyfreq t)
  (require-init 'init-httpd t)
  ;;(require-init 'init-cnfonts t)
  ;;(require-init 'init-use-package)
  (require-init 'init-lsp)
  ;; projectile costs 7% startup time

  ;; don't play with color-theme in light weight mode
  ;; color themes are already installed in `init-elpa.el'
  (require-init 'init-theme)
  (require-init 'init-rainbow-delimiters t)
  (require-init 'init-ggtags t)


  ;; essential tools
  (require-init 'init-essential)
  ;; tools nice to have
  (require-init 'init-misc t)
  (require-init 'init-dictionary t)
  (require-init 'init-emms t)

  (require-init 'init-browser t)
  (require-init 'init-shackle t)
  (require-init 'init-dired t)
  (require-init 'init-writting t)
  (require-init 'init-hydra) ; hotkey is required everywhere

  ;; use evil mode (vi key binding)
  (require-init 'init-evil) ; init-evil dependent on init-clipboard
  (require-init 'init-pdf)

  ;; ediff configuration should be last so it can override
  ;; the key bindings in previous configuration
  (when my-lightweight-mode-p
    (require-init 'init-ediff))

  ;; @see https://github.com/hlissner/doom-emacs/wiki/FAQ
  ;; Adding directories under "site-lisp/" to `load-path' slows
  ;; down all `require' statement. So we do this at the end of startup
  ;; NO ELPA package is dependent on "site-lisp/".
  (unless my-disable-idle-timer
    (my-add-subdirs-to-load-path (file-name-as-directory my-site-lisp-dir)))

  (require-init 'init-no-byte-compile t)

  (unless my-lightweight-mode-p
    ;; @see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
    ;; See `custom-file' for details.
    (setq custom-file (concat my-emacs-d "custom-set-variables.el"))
    (if (file-exists-p custom-file) (load custom-file t t))

    ;; my personal setup, other major-mode specific setup need it.
    ;; It's dependent on *.el in `my-site-lisp-dir'
    (my-run-with-idle-timer 1 (lambda () (load "~/.custom.el" t nil)))))


;; @see https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; Emacs 25 does gc too frequently
;; (setq garbage-collection-messages t) ; for debug
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 4 nil #'my-cleanup-gc)

(message "*** Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done)
(put 'erase-buffer 'disabled nil)

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
