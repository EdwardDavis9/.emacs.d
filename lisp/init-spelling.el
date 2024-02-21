;; -*- coding: utf-8; lexical-binding: t; -*-


;;;  find aspell and hunspell automatically, to ensure emacs use en_US by the following configure
(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
;;;


;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (when run-together
        (cond
         ;; Kevin Atkinson said now aspell supports camel case directly
         ;; https://github.com/redguardtoo/emacs.d/issues/796
         ((string-match-p "--camel-case"
                          (shell-command-to-string (concat ispell-program-name " --help")))
          (setq args (append args '("--camel-case"))))

         ;; old aspell uses "--run-together". Please note we are not dependent on this option
         ;; to check camel case word. wucuo is the final solution. This aspell options is just
         ;; some extra check to speed up the whole process.
         (t
          (setq args (append args '("--run-together" "--run-together-limit=16")))))))

     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "-d en_US")))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary' itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup `ispell-local-dictionary-alist'
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 (t (setq ispell-program-name nil)))

;; `ispell-cmd-args' is useless, it's the list of *extra* arguments we will append to the ispell process when `ispell-word' is called.
;; `ispell-extra-args' is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, `ispell-extra-args' will NOT be used.
;; Hack `ispell-local-dictionary-alist' instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))

(defun my-ispell-word-hack (orig-func &rest args)
  "Use Emacs original arguments when calling `ispell-word'.
When fixing a typo, avoid pass camel case option to cli program."
  (let* ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original argument
    (setq ispell-extra-args (my-detect-ispell-args))
    (apply orig-func args)
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(advice-add 'ispell-word :around #'my-ispell-word-hack)
(advice-add 'flyspell-auto-correct-word :around #'my-ispell-word-hack)

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)


;;;



(defvar my-default-spell-check-language "en_US"
  "Language used by aspell and hunspell CLI.")

(with-eval-after-load 'flyspell
  ;; You can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
  ;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
  (global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

  ;; better performance
  (setq flyspell-issue-message-flag nil))

  ;; flyspell-lazy is outdated and conflicts with latest flyspell

;; Basic Logic Summary:
;; If (aspell is installed) { use aspell}
;; else if (hunspell is installed) { use hunspell }
;; English dictionary is used.
;;
;; I prefer aspell because:
;; - aspell is very stable and easy to install
;; - looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun my-detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words.
RUN-TOGETHER makes aspell less capable to find plain English typo.
So it should be used in `prog-mode-hook' only."
  (let* (args)
    (when ispell-program-name
      (cond
       ;; use aspell
       ((string-match "aspell" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
        ;; For aspell's option "--lang", "two letter ISO 3166 country code after a underscore" is OPTIONAL.
        (setq args (list "--sug-mode=ultra" (format "--lang=%s" my-default-spell-check-language)))
        ;; "--run-together-min" could not be 3, see `check` in "speller_impl.cpp".
        ;; The algorithm is not precise.
        ;; Run `echo tasteTableConfig | aspell --lang=en_US -C --run-together-limit=16  --encoding=utf-8 -a` in shell.
        (when run-together
          (cond
           ;; Kevin Atkinson said now aspell supports camel case directly
           ;; https://github.com/redguardtoo/emacs.d/issues/796
           ((string-match "--.*camel-case"
                            (shell-command-to-string (concat ispell-program-name " --help")))
            (setq args (append args '("--camel-case"))))

           ;; old aspell uses "--run-together". Please note we are not dependent on this option
           ;; to check camel case word. wucuo is the final solution. This aspell options is just
           ;; some extra check to speed up the whole process.
           (t
            (setq args (append args '("--run-together" "--run-together-limit=16")))))))

       ;; use hunspell
       ((string-match "hunspell" ispell-program-name)
        (setq args nil))))
    args))

;; Aspell Setup (recommended):
;; It's easy to set up aspell. So the details are skipped.
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;;
;; 2. Download openoffice dictionary extension from
;; http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;;
;; 3. Say `dict-en.oxt' is downloaded. Rename it to `dict-en.zip' and unzip
;; the contents to a temporary folder. Got "en_US.dic" and "en_US.aff" in
;; that folder.
;;
;; 4. Hunspell's option "-d en_US" means finding dictionary "en_US"
;; Modify `ispell-local-dictionary-alist' to set that option of hunspell
;;
;; 5. Copy "en_US.dic" and "en_US.aff" from that temporary folder to
;; the place for dictionary files. I use "~/usr_local/share/hunspell/".
;;
;; 6. Add that folder to shell environment variable "DICPATH"
;;
;; 7. Restart emacs so when hunspell is run by ispell/flyspell to make
;; "DICPATH" take effect
;;
;; hunspell searches a dictionary named "en_US" in the path specified by
;; "DICPATH" by default.

(defvar my-force-to-use-hunspell nil
  "Force to use hunspell.  If nil, try to detect aspell&hunspell.")

(defun my-configure-ispell-parameters ()
  "Set `ispell-program-name' and other parameters."
  (cond
   ;; use aspell
   ((and (not my-force-to-use-hunspell) (executable-find "aspell"))
    (setq ispell-program-name "aspell"))

   ;; use hunspell
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary my-default-spell-check-language)
    (setq ispell-local-dictionary-alist
          (list (list my-default-spell-check-language
                      "[[:alpha:]]" "[^[:alpha:]]" "[']"
                      nil
                      (list "-d" my-default-spell-check-language)
                      nil
                      'utf-8)))
    ;; New variable `ispell-hunspell-dictionary-alist' is defined in Emacs
    ;; If it's nil, Emacs tries to automatically set up the dictionaries.
    (when (boundp 'ispell-hunspell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

   (t (setq ispell-program-name nil)
      (message "You need install either aspell or hunspell for ispell"))))

;; You could define your own configuration and call `my-configure-ispell-parameters' in "~/.custom.el"
(my-configure-ispell-parameters)

(defun my-ispell-word-hack (orig-func &rest args)
  "Use Emacs original arguments when calling `ispell-word'.
When fixing a typo, avoid pass camel case option to cli program."
  (let* ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (my-detect-ispell-args))
    (apply orig-func args)
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(advice-add 'ispell-word :around #'my-ispell-word-hack)
(advice-add 'flyspell-auto-correct-word :around #'my-ispell-word-hack)

(defvar my-disable-wucuo nil
  "Disable wucuo.")

(defun text-mode-hook-setup ()
  "Set up text mode."
  ;; Turn off RUN-TOGETHER option when spell check text.
  (unless my-disable-wucuo
    (setq-local ispell-extra-args (my-detect-ispell-args))
    (my-ensure 'wucuo)
    (wucuo-start)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

(defun my-clean-aspell-dict ()
  "Clean ~/.aspell.pws (dictionary used by aspell)."
  (interactive)
  (let* ((dict (file-truename "~/.aspell.en.pws"))
         (lines (my-read-lines dict))
         ;; sort words
         (aspell-words (sort (cdr lines) 'string<)))
    (save-buffer)
    (sit-for 1)
    (with-temp-file dict
      (insert (format "%s %d\n%s"
                        "personal_ws-1.1 en"
                        (length aspell-words)
                        (mapconcat 'identity aspell-words "\n"))))))

;; {{ langtool setup
(with-eval-after-load 'langtool
  (setq langtool-generic-check-predicate
        '(lambda (start end)
           ;; set up for `org-mode'
           (let* ((begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
                  (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
                  (case-fold-search t)
                  (ignored-font-faces '(org-verbatim
                                        org-block-begin-line
                                        org-meta-line
                                        org-special-keyword
                                        org-property-value
                                        org-tag
                                        org-link
                                        org-table
                                        org-level-1
                                        org-document-info))
                  (rlt t)
                  th
                  b e)
             (save-excursion
               (goto-char start)

               ;; ignore certain errors by set rlt to nil
               (cond
                ((cl-intersection (my-what-face start) ignored-font-faces)
                 ;; check current font face
                 (setq rlt nil))
                ((or (string-match "^ *- $" (buffer-substring (line-beginning-position) (+ start 2)))
                     (string-match "^ *- $" (buffer-substring (line-beginning-position) (+ end 2))))
                 ;; dash character of " - list item 1"
                 (setq rlt nil))

                ((and (setq th (thing-at-point 'evil-WORD))
                      (or (string-match "^=[^=]*=[,.]?$" th)
                          (string-match "^\\[\\[" th)
                          (string-match "^=(" th)
                          (string-match ")=$" th)))
                 ;; embedded code like =code= or org-link [[https://gnu.org][GNU Operation System]] or [[www.gnu.org]]
                 ;; langtool could finish checking before major mode prepare font face for all texts
                 (setq rlt nil))
                (t
                 ;; inside source block?
                 (setq b (re-search-backward begin-regexp nil t))
                 (if b (setq e (re-search-forward end-regexp nil t)))
                 (if (and b e (< start e)) (setq rlt nil)))))
             rlt))))
;; }}


(with-eval-after-load 'wucuo
  ;; {{ wucuo is used to check camel cased code and plain text.  Code is usually written
  ;; in English. If your code uses other language (Spanish?),
  ;; Un-comment and modify below two lines:

  ;; (setq wucuo-aspell-language-to-use "en")
  ;; (setq wucuo-hunspell-dictionary-base-name "en_US")

  ;; }}

  ;; do NOT turn on `flyspell-mode' automatically.
  ;; check buffer or visible region only
  ;; spell check buffer every 30 seconds
  (setq wucuo-update-interval 2))

(provide 'init-spelling)
