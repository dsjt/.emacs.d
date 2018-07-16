;;; init.el

(setq debug-on-error t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Server
(require 'server)
(server-start)

;; personal settings
(load-file "~/.emacs.d/private.el")

;; A Part Of Key-Bindings
(global-unset-key (kbd "C-l"))          ;prefix key
(global-unset-key (kbd "C-x l"))        ;prefix key
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "<f1> h"))
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-l c") 'compile)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-:") 'recenter-top-bottom)
(global-set-key (kbd "C-l C-r") 'revert-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Basic Configuration
(progn
  (setq inhibit-startup-screen t)
  (setq show-paren-style 'parenthesis)
  (setq frame-title-format
        (format "%%f - Emacs@%s" (system-name)))
  (setq gc-cons-threshold (* 10 gc-cons-threshold))
  (setq mark-ring-max 64
        kill-whole-line t
        visible-bell nil
        ring-bell-function 'ignore)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 80)
  (setq-default line-move-visual nil)
  (setq-default cursor-type 'bar)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs)
  (show-paren-mode +1)
  (global-hl-line-mode +1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode +1)
  (transient-mark-mode +1)
  (global-visual-line-mode 1)
  (ffap-bindings)
  (put 'narrow-to-region 'disabled nil)
  (mouse-avoidance-mode 'exile)
  (add-hook 'emacs-lisp-mode 'electric-indent-mode)
  (savehist-mode 1)
  (require 'saveplace)
  (save-place-mode 1)
  (setq save-place t
        save-place-file "~/.emacs.d/saved-places"))
;;;###autoload
(defun my/switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (delete-other-windows))
(global-set-key (kbd "C-l SPC") 'my/switch-to-scratch-buffer)
(global-set-key (kbd "C-M-;") 'comment-line)
;;;###autoload
(defun other-window-or-split ()
  "other window or split window"
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-,") 'other-window-or-split)
(require 'cl)
;;;###autoload
(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (loop for buf in (buffer-list)
        unless (or
                (get-buffer-window buf)
                (string= (substring (buffer-name buf) 0 1) " ")
                (get-buffer-process buf)
                (member (buffer-name buf) ;; 消さないバッファ名を指定
                        '("*Messages*" "*Compile-Log*" "*Help*"
                          "*scratch*" "*init log*")))
        do (kill-buffer buf)))
(global-set-key (kbd "C-x C-c C-c") 'my/kill-other-buffers)
;;;###autoload
(defun toggle-fill-and-unfill ()
  "Toggle fill and unfill paragraph."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'toggle-fill-and-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(global-set-key [remap fill-paragraph] #'toggle-fill-and-unfill)
(global-set-key (kbd "M-q") #'fill-paragraph)

;; Backup
(let ((target-dir (expand-file-name "/"))
      (dest-dir (expand-file-name "~/.Trash/")))
  ;; 自動保存ファイル(#*#)の作成先変更
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat target-dir "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat dest-dir "\\2")
                 t))
  ;; バックアップファイル(*~)の作成先変更
  (add-to-list 'backup-directory-alist (cons target-dir dest-dir))
  ;; 自動保存リスト(.saves-<PID>-<HOSTNAME>)の作成先変更
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" dest-dir)))


;;Smartrep
(package-install 'smartrep)
(require 'smartrep)

(package-install 'restart-emacs)

(package-install 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'execute-extended-command)
(global-set-key (kbd "C-x C-f") 'ffap)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-M-c") 'helm-resume)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "<f1> h") 'helm-apropos)
(global-unset-key (kbd "M-."))
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "M-/") 'helm-dabbrev)
(helm-mode)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(setq helm-completing-read-handlers-alist
      (append helm-completing-read-handlers-alist
              '((ffap . nil)
                (dired-create-directory . nil)
                (howm-list-grep-fixed . nil))))
(setq helm-ff-skip-boring-files t)

(require 'recentf nil t)
(setq recentf-auto-cleanup 'never
      recentf-max-saved-items 10000)
(package-install 'recentf-ext)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'helm-recentf)

(package-install 'sequential-command)
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; Org-Mode
(package-install 'org)
(require 'org)
(require 'ox "~/.emacs.d/elpa/org-20180709/ox.el")
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(define-key org-mode-map (kbd "C-,") 'other-window-or-split)
(define-key org-mode-map (kbd "C-S-n") 'org-metaright)
(define-key org-mode-map (kbd "C-S-p") 'org-metaleft)
(define-key org-mode-map (kbd "M-S-<up>") 'org-move-subtree-up)
(define-key org-mode-map (kbd "M-S-<down>") 'org-move-subtree-down)

(setq org-startup-folded nil
      org-hide-leading-stars nil
      org-log-done 'time
      org-use-fast-todo-selection nil
      org-use-speed-commands t
      org-global-properties
      '(("Effort_ALL" . "00:10 00:20 00:30 01:00 01:30 02:00 02:30 03:00"))
      org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
      org-agenda-columns-add-appointments-to-effort-sum t
      org-tag-alist '(("Action" . ?a) ("Project" . ?p) ("Wait" . ?w))
      org-agenda-skip-unavailable-files t)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-timer-set-hook 'org-clock-in)
(add-hook 'org-timer-done-hook 'org-clock-out)
(add-hook 'org-timer-stop-hook 'org-clock-out)
;; capture
(setq org-capture-templates
      '(("b" "Inbox (active time stamp)" entry
         (file+headline my/gtd-main-file "Inbox")
         "* TODO %?\n\n Captured at %u\n")))
;;;###autoload
(defun my/org-capture ()
  (interactive)
  (org-capture nil "b"))
(global-set-key (kbd "C-c c") #'my/org-capture)
(global-set-key (kbd "C-l C-q") #'my/org-capture)
(global-set-key (kbd "C-q C-l") #'my/org-capture)
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
        ("d" "1 days agenda"
         ((agenda
           "reminder"
           ((org-agenda-format-date #'(lambda (date) "reminder"))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
            (org-agenda-span 1)
            (org-agenda-overriding-header "")
            (org-agenda-use-time-grid nil)
            ))
          (agenda
           ""
           ((org-agenda-span 1)
            (org-agenda-skip-scheduled-if-done t)
            (org-agenda-show-log t)
            (org-agenda-log-mode-items '(state))
            (org-agenda-skip-function '(org-agenda-skip-entry-if
                                        'regexp
                                        "^<[0-9\-].+>$"))
            (org-agenda-use-time-grid nil)
            ))
          (tags "+Inbox/TODO")
          (tags
           "+Action"
           ((org-agenda-sorting-strategy
             '((agenda habit-down time-up priority-down category-keep)
               (todo priority-down tsia-up category-keep)
               ;; カテゴリーごとに取り組めるように
               (tags category-keep priority-down tsia-up)
               (search category-keep)))
            (org-agenda-skip-function '(org-agenda-skip-entry-if
                                        'todo
                                        '("DONE" "CANC")))))
          (tags-todo "+Project/TODO")
          ))
        ("w" "8 days agenda"
         ((agenda "" ((org-agenda-span 8)
                      (org-agenda-start-day "-1")
                      (org-agenda-columns-add-appointments-to-effort-sum t)
                      (org-agenda-log-mode-items '(state clock closed))))
          (tags-todo "Inbox")
          (todo "TODO")))))
(defvar my/toggle-including-org-agenda-archives nil)
(defun my/toggle-including-org-agenda-archives ()
  (interactive)
  (setq my/toggle-including-org-agenda-archives
        (not my/toggle-including-org-agenda-archives))
  (if my/toggle-including-org-agenda-archives
      (loop for x in org-agenda-files
            if (file-exists-p (concat x "_archive_2018"))
            do (add-to-list 'org-agenda-files (concat x "_archive_2018"))
            finally (message "org-agenda-files include archive files"))
    (setq org-agenda-files
          (remove-if (lambda (x) (string-match "_archive_2018" x))
                     org-agenda-files))
    (message "org-agenda-files exclude archive files")))



(require 'solar)
(setq holiday-general-holidays nil
      holiday-local-holidays t
      holiday-solar-holidays nil
      holiday-bahai-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-other-holidays nil
      mark-holidays-in-calendar t)
(setq holiday-local-holidays
      '((holiday-fixed 1 1 "元日")
        (holiday-float 1 1 2 "成人の日")
        (holiday-fixed 2 11 "建国記念の日")
        (holiday-sexp '(map 'list 'truncate (solar-equinoxes/solstices 0 year))
                      "春分の日")
        (holiday-fixed 4 29 "昭和の日")
        (holiday-fixed 5 3 "憲法記念日")
        (holiday-fixed 5 4 "みどりの日")
        (holiday-fixed 5 5 "こどもの日")
        (holiday-float 7 1 3 "海の日")
        (holiday-float 7 1 3 "敬老の日")
        (holiday-sexp '(map 'list 'truncate (solar-equinoxes/solstices 2 year))
                      "秋分の日")
        (holiday-float 10 1 2 "体育の日")
        (holiday-fixed 11 3 "文化の日")
        (holiday-fixed 11 23 "勤労感謝の日")
        (holiday-fixed 12 23 "天皇誕生日")))
(setq org-columns-default-format
      "%50ITEM{Task} %TODO %8EFFORT{:} %6CLOCKSUM_T{Total}")
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages 'org-babel-load-languages
                             '((C . t)
                               (emacs-lisp . t)
                               (gnuplot . t)
                               (ruby . t)
                               (python . t)
                               (dot . t)
                               (org . t)))
(setq org-src-window-setup 'other-window)
(setq org-image-actual-width '(256))
(define-key org-mode-map (kbd "C-c C-7") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c C-7") 'org-edit-src-exit)

;; (el-get-bundle 'f)
;; (el-get-bundle! 'gregsexton/ob-ipython)
;; (autoload 'org-babel-execute:python "ob-python.el")
;; (with-eval-after-load 'ob-python
;;   (setq org-babel-python-command "python3")
;;   (setq org-src-preserve-indentation t)
;;   (setq org-babel-default-header-args:python '((:session . "my_session")
;;                                                (:results . "output")
;;                                                (:tangle . "yes"))))
;; (autoload 'org-babel-execute:sh "ob-sh.el")
;; (with-eval-after-load 'org-link
;;   (setq org-file-apps '(("\\.rd\\'" . emacs)
;;                         ("\\.pdf\\'" . evince)
;;                         (auto-mode . emacs)
;;                         ("\\.mm\\'" . default)
;;                         ("\\.x?html?\\'" . default)))
  ;; (setq org-link-file-path-type 'relative))
(require 'org-archive)
(setq org-archive-default-command 'org-archive-to-archive-sibling)
(setq org-archive-location
      (concat "%s_archive_"
              (format-time-string "%Y::" (current-time))))
(defun my/org-preview ()
  (interactive)
  (let ((html (org-export-output-file-name ".html"))
        (orgf (buffer-file-name)))
    (if (file-newer-than-file-p orgf (expand-file-name html))
        (org-html-export-to-html))
    (async-start-process "org-preview"
                         "xdg-open"
                         'ignore
                         html)))
(global-set-key (kbd "C-l p") 'my/org-preview)

;; Visual
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/")
(global-set-key (kbd "<f1>C-d") 'describe-face)
(package-install 'solarized-theme)
(setq solarized-scale-org-headlines nil
      solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)
(set-face-attribute 'default nil :family "IPAGothic" :height 130)
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "IPAGothic"
                             :size 18))
;; aaaaaaaaAA
;; あああああ
(add-to-list 'face-font-rescale-alist
             '(("IPAGothic" . 1.4)))
(package-install 'rainbow-mode)
;;;###autoload
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;; Region
(package-install 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

;; Search
(package-install 'helm-ag)
(global-set-key (kbd "C-l g") 'helm-ag)
(package-install 'anzu)
(global-anzu-mode 1)
(global-set-key (kbd "C-x q") 'anzu-query-replace)
(package-install 'visual-regexp)
(global-unset-key (kbd "C-x q"))
(global-set-key (kbd "C-x Q") 'vr/query-replace)
(global-set-key (kbd "C-S-c m") 'vr/mc-mark)
(setq vr/default-replace-preview t)
(setq case-replace nil)

;; ;; Move
(require 'point-undo)
(global-set-key (kbd "C-.") 'point-undo)
(global-set-key (kbd "C-M-.") 'point-redo)
(global-set-key (kbd "<f7>") 'point-undo)
(global-set-key (kbd "S-<f7>") 'point-redo)
;; (el-get-bundle 'goto-chg)
;; (global-set-key (kbd "<f8>") 'goto-last-change)
;; (global-set-key (kbd "S-<f8>") 'goto-last-change-reverse)
(package-install 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(package-install 'crux)
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "M-o") 'crux-smart-open-line-above)
(global-set-key (kbd "C-S-y") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-M-+")
                'crux-duplicate-and-comment-current-line-or-region)
(package-install 'avy)
(global-set-key (kbd "C-M-j") 'avy-goto-char)
(smartrep-define-key
    global-map "C-c" '(("[" . (backward-paragraph))
                       ("]" . (forward-paragraph))))
(package-install 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; Buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "*[^*]+*")
;; Window
(winner-mode 1)

;; Undo
(package-install 'undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "C-M-/") 'undo-tree-redo)
(with-eval-after-load 'undo-tree
  (setq undo-no-redo nil
        undo-limit 600000
        undo-strong-limit 900000))

;; Dired
(package-install 'dired-hacks-utils)
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")
(require 'dired)
(setq dired-dwim-target t)
(put 'dired-find-alternate-file 'disabled nil)
;; diredで開いたpdfをrecentfに追加するための設定．
(defadvice dired-find-file (before add-recentf)
  (let ((file (dired-get-filename)))
    (when file
      (recentf-add-file file))))
(ad-activate 'dired-find-file)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(package-install 'dired-filter)
(with-eval-after-load 'dired-filter
  (setq dired-filter-group-saved-groups
        '(("default"
           ("PDF"      (extension  "pdf"))
           ("LaTeX"    (extension "tex" "bib"))
           ("Org"      (extension  "org"))
           ("Archives" (extension "zip" "rar" "gz" "bz2" "tar"))
           ("python"   (extension "py"))
           ("cpp"      (extension "cpp"))
           ("h"        (extension "h"))
           ("hpp"      (extension "hpp"))
           ("sh"       (extension "sh")))))
  (add-hook 'dired-mode-hook 'dired-filter-group-mode 1)
  (define-key dired-mode-map (kbd ")") #'dired-filter-group-mode))
;; my/dired-config
;;;###autoload
;; (defun my/dired-config()
;;   (dired-filter-group-mode 1))
;; (add-hook 'dired-mode-hook 'my/dired-config)
(package-install 'openwith)
(require 'openwith)
(openwith-mode 1)
(setq openwith-associations
      '(("\\.pdf\\'" "evince" (file))
        ("\\.ods\\'" "libreoffice" (file))))


;; Junk-File
(package-install 'open-junk-file)
(global-set-key (kbd "C-x f") 'open-junk-file)
(require 'open-junk-file)
;;;###autoload
(defun my/goto-junk-directory()
  (interactive)
  (let* ((file (format-time-string open-junk-file-format (current-time)))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file dir)))
(global-set-key (kbd "C-l f") 'my/goto-junk-directory)

;; Latex
(package-install 'auctex)
(require 'latex)
(add-hook 'latex-mode-hook #'tex-fold-mode)
(define-key LaTeX-mode-map (kbd "C-^") #'TeX-fold-dwim)

;; Parenthesis
(package-install 'smartparens)
(smartparens-global-mode 1)
(smartparens-global-strict-mode -1)
(setq sp-highlight-pair-overlay nil)
;; sp-elisp
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'scheme-mode "'" nil :actions nil)
(sp-local-pair 'inferior-scheme-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'org-mode "\"" nil :actions nil)
(sp-local-pair 'org-mode "$" "$")
(sp-local-pair 'org-mode "\"" "\"")
(sp-use-paredit-bindings)

;; Complete
(global-set-key (kbd "C-;") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; ;; auto-complete
;; (el-get-bundle auto-complete-config in auto-complete)
;; (with-eval-after-load 'auto-complete-config
;;   (ac-config-default)
;;   (global-auto-complete-mode 1)
;;   (add-to-list 'ac-modes 'YaTeX-mode)
;;   (setq ac-auto-start 4
;;         ac-auto-show-menu 0.8
;;         ac-use-comphist t
;;         ac-candidate-limit nil
;;         ac-use-quick-help nil
;;         ac-use-menu-map t)
;;   (define-key ac-completing-map (kbd "<tab>") 'nil)
;;   (define-key ac-completing-map (kbd "M-/")   'ac-stop)
;;   (define-key ac-completing-map (kbd "RET") nil)
;;   (with-eval-after-load 'yasnippet
;;     (setf (symbol-function 'yas-active-keys)
;;           (lambda ()
;;             (remove-duplicates
;;              (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))))
;; しばらくcompany-modeをお試し
(package-install 'company)
(global-company-mode 1)
;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; Snippet
(package-install 'yasnippet)
(global-unset-key (kbd "C-x i"))
(global-set-key (kbd "C-x i v") 'yas-visit-snippet-file)
(global-set-key (kbd "C-x i n") 'yas-new-snippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets/")
(package-install 'helm-c-yasnippet)
(require 'helm-c-yasnippet)
(global-set-key (kbd "C-x i i") 'helm-yas-complete)
(setq helm-yas-space-match-any-greedy t)
(package-install 'yasnippet-snippets)
(yasnippet-snippets-initialize)


;; Scroll
(package-install 'yascroll)
(global-yascroll-bar-mode 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 5)
(setq next-screen-context-lines 10)
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Howm
(global-unset-key (kbd "C-q"))
(setq howm-prefix "\C-q"
      howm-view-title-header "*"
      howm-menu-lang 'ja
      howm-keyword-case-fold-search t)
(package-install 'howm)                  ; 上との順序，重要なので変更しない
(require 'howm-mode)
(setq howm-list-recent-title t
      howm-list-all-title t
      howm-menu-expiry-hours 2
      howm-menu-schedule-days-before 10
      howm-menu-schedule-days 7
      howm-file-name-format "%Y/%m/%d-%H%M%S.org"
      howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$"
      howm-excluded-file-regexp (string-join '("/\\.#"
                                               "[~#]$"
                                               "\\.bak$"
                                               "/CVS/"
                                               "\\.doc$"
                                               "\\.pdf$"
                                               "\\.ppt$"
                                               "\\.xls$"
                                               "\\.html$"
                                               "\\.png$"
                                               "\\.gif$"
                                               "\\.jpg$"
                                               "\\.h5$") "\\|"))
(setq howm-menu-refresh-after-save nil)
(setq howm-view-summary-persistent nil)
(setq howm-template
      (format "%s\n%s\n%s\n"
              "#+title: %cursor"
              (format "#+HTML_HEAD: <link href=%s rel=\"stylesheet\"></link>"
                      my/simple-css-file)
              "* 概要"))
(setq howm-template-file-format "[[%s]]")
(setq howm-view-use-grep t)
(setq howm-menu-recent-num 10)
(setq howm-list-recent-days 20)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(add-hook 'org-mode-hook 'howm-mode)
(set-face-attribute 'howm-mode-title-face nil :foreground nil)
(set-face-attribute 'howm-reminder-today-face nil
                    :foreground nil :background "#2d37aa" :box nil)
(set-face-attribute 'howm-reminder-tomorrow-face nil
                    :foreground nil :background "#2d4900" :box nil)
(setq howm-view-grep-file-stdin-option nil) ;なぜか必要?
(global-set-key (kbd "C-q c") 'howm-create)
(global-set-key (kbd "C-q s") 'howm-list-grep-fixed)

;; Translate
(package-install 'popwin)
(require 'popwin)
(popwin-mode 1)

;; rotate
(package-install 'rotate)
(smartrep-define-key
    global-map "C-l" '(("w" . (rotate-window))
                       ("l" . (rotate-layout))))
(smartrep-define-key
    global-map "C-l" '(("{" . (shrink-window-horizontally 2))
                       ("}" . (enlarge-window-horizontally 2))))

;; C Language
(setq-default c-hungry-delete-key nil)
(add-hook 'c++-mode-hook 'electric-indent-mode)
(add-hook 'c-mode-hook 'electric-indent-mode)
;;;###autoload
(defun my/astyle ()
  "Implement astyle to c-code when saving it."
  (interactive)
  (call-process "astyle" nil nil
                "--style=kr"
                "-s4"
                "-Y"
                "-M80"
                "-p"
                "-U"
                "-j"
                "-k3"
                "-c"
                (buffer-file-name))
  (revert-buffer nil t))
(package-install 'clang-format)
(setq clang-format-executable "clang-format-3.5")
(set-default 'clang-format-style
             "{BasedOnStyle: Google, IndentWidth: 4, Standard: C++11}")

;; Folding
(require 'hideshow)
(add-hook 'html-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
;; (add-hook 'latex-mode-hook 'hs-minor-mode)
(add-hook 'YaTeX-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(define-key hs-minor-mode-map (kbd "C-^") 'hs-toggle-hiding)
(define-key hs-minor-mode-map (kbd "C-M-^") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-M-~") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-l point)^")
  (lambda () (interactive) (hs-hide-level 2)))
(defun display-code-line-counts (ov)
  (overlay-put ov 'display
               (format "   ... [+%3d] "
                       (count-lines (overlay-start ov)
                                    (overlay-end ov))))
  (overlay-put ov 'face '(:foreground "yellow green")))
(setq hs-set-up-overlay 'display-code-line-counts)

;; External Program Utility
(defvar my/open-command nil)
(setq my/open-command "nautilus")
;;;###autoload
(defun start-explorer ()
  (interactive)
  (let ((dd (expand-file-name default-directory)))
    (async-start-process "Filer" my/open-command 'ignore dd)))
(global-set-key (kbd "C-\\ e") 'start-explorer)
(global-set-key (kbd "C-\\ e") 'start-explorer)
(defvar my/terminal-command nil)
(setq my/terminal-command "gnome-terminal")
;;;###autoload
(defun start-terminal ()
  (interactive)
  (let ((dd (expand-file-name default-directory)))
    (async-start-process "my/terminal"
                         my/terminal-command
                         'ignore
                         default-directory)))
;;;###autoload
(defun my/popup-eshell (arg)
  (interactive "p")
  (let (eb)
    (save-window-excursion
      (setq eb (eshell arg)))
    (popwin:popup-buffer-tail eb)))
(global-set-key (kbd "C-\\ c") 'start-terminal)
(global-set-key (kbd "C-\\ E") 'eshell)
(global-set-key (kbd "C-\\ M-e") 'my/popup-eshell)
(global-set-key (kbd "C-\\ 9") 'popwin:stick-popup-window)

;;;###autoload
(defun my/insert-emacs-init-time-in-scratch ()
  (interactive)
  (with-output-to-temp-buffer "*information*"
    (let ((str (format "Emacs init time: %s\n\n\n" (emacs-init-time))))
      (princ str))))
(add-hook 'after-init-hook 'my/insert-emacs-init-time-in-scratch)

;; Multiple-Cursors
(package-install 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c n") 'mc/insert-numbers)
(global-set-key (kbd "C-S-c s") 'mc/sort-regions)
(global-set-key (kbd "C-S-c r") 'mc/reverse-regions)
(global-set-key (kbd "C-S-c C-S-s") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-S-c C-S-m") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-S-c C-S-p") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-S-c C-S-n") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-S-c C-S-e") 'mc/mark-more-like-this-extended)

;; Magit
(package-install 'magit)
;; (require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-revert-buffers t)
(global-set-key (kbd "C-l C-m C-<SPC>") 'magit-status)

;; Git-Gutter
(package-install 'git-gutter)
(global-git-gutter-mode t)
(global-set-key (kbd "C-l C-g s") 'git-gutter:stage-hun)
(global-set-key (kbd "C-l C-g C-n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-l C-g C-p") 'git-gutter:previous-hunk)

;; Git-Gutter-Fringe
(package-install 'git-gutter-fringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

;; Number
(package-install 'number)
(require 'number)
(global-set-key (kbd "C-c C-+") 'number/add)
(global-set-key (kbd "C-c C--") 'number/sub)
(global-set-key (kbd "C-c C-*") 'number/multiply)
(global-set-key (kbd "C-c C-/") 'number/divide)
(global-set-key (kbd "C-c C-0") 'number/pad)
(global-set-key (kbd "C-c C-=") 'number/eval)

;;; Python
(require 'python)
;;;###autoload
(defun python-shell-send-line()
  "send current line to python shell."
  (interactive)
  (python-shell-send-region (line-beginning-position)
                            (line-end-position)))
;;;###autoload
(defun python-shell-send-paragraph()
  "send current line to python shell."
  (interactive)
  (let ((para (thing-at-point 'paragraph t)))
    (python-shell-send-string para)
    (message "Sent ...")))
(define-key python-mode-map (kbd "C-c C-u") 'python-shell-send-line)
(define-key python-mode-map (kbd "C-c p") 'python-shell-send-paragraph)
(define-key python-mode-map (kbd "C-M-f") 'python-nav-forward-defun)
(define-key python-mode-map (kbd "C-M-b") 'python-nav-backward-defun)
;; (add-hook 'python-mode-hook 'turn-off-smartparens-mode)
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i"
      indent-tabs-mode nil
      indent-level 4
      python-indent 4
      tab-width 4
      python-indent-guess-indent-offset nil
      python-fill-paren-function nil)
;; 他のところでエラーがでるかもしれない.
;; run-python で日本語を通すために必要
(setenv "LANG" "ja_JP.UTF-8")
(setenv "LC_ALL" "ja_JP.UTF-8")
(setenv "LC_CTYPE" "ja_JP.UTF-8")
(add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
(defvar my/py-hide-show-keywords '("class" "def" "elif" "else" "except"
                                   "for" "if" "while" "finally" "try" "with"))
(defun my/replace-hs-spectial ()
  (setq hs-special-modes-alist
        (remove-if #'(lambda (x) (equal 'python-mode (car x)))
                   hs-special-modes-alist))
  (push (list
         'python-mode
         (mapconcat #'(lambda (x) (concat "^\\s-*" x "\\>"))
                    my/py-hide-show-keywords "\\|")
         "^\\s-*"
         "#"
         #'(lambda (x) (python-nav-end-of-block))
         nil)
        hs-special-modes-alist))
(add-hook 'python-mode-hook 'my/replace-hs-spectial)




;; (el-get-bundle py-autopep8
;;   (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
;; (require 'py-autopep8)
;; ;; (el-get-bundle py-autopep8
;; ;;   (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
;; ;; py-autopep8 は 自分でいじりました. (pop kill-ring) を抜きました.
;; ;; これがあると,保存した際,kill-ringが消えます.
;; ;; el-get-updateすると,元に戻ってしまいます.極力しないこと.
;; ;; してしまい,症状が出てくる場合は,(pop kill-ring)を削除すること


;; ;; Quickrun
;; (el-get-bundle! 'quickrun)
;; (global-set-key (kbd "C-l C-l r") 'quickrun)

;; ;; Gnuplot
;; (el-get-bundle 'gnuplot-mode)

;; ;; Tramp
;; (require 'tramp)
;; (setq tramp-default-method "scp")

;; ;; path config これがないと，platexが実行できなかったりします．
;; (el-get-bundle exec-path-from-shell
;;   (require 'exec-path-from-shell)
;;   (exec-path-from-shell-initialize))

;; ;; dokuwiki
;; (el-get-bundle ox-wk :type git :url "git@github.com:w-vi/ox-wk.el")
;; (el-get-bundle org-textile :type git :url "git@github.com:yashi/org-textile")
;; ;;;###autoload
;; (defun org-textile-example-block(example-block contents info)
;;   (let ((value (org-element-property :value example-block)))
;;     (concat "<pre>" value "\n</pre>")))

;; ;; Html
;; (el-get-bundle! zencoding-mode)

;; ;; Scheme
;; ;; (setq scheme-program-name "jakld")
;; (setq process-coding-system-alist
;;       (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
;; (if (eq system-type 'darwin)
;;     (setq scheme-program-name "/usr/local/bin/gosh -i")
;;   (setq scheme-program-name "/usr/bin/gosh -i"))
;; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; (setq cmuscheme-load-hook
;;       '((lambda () (define-key scheme-mode-map (kbd "C-c C-p")
;;                      'run-scheme))))

;; ;; Json
;; (el-get-bundle 'json-mode)

;; ;; Clipmon
;; (el-get-bundle bburns/clipmon)

;; ;; Htmlize
;; (el-get-bundle htmlize)

;; ;; key-bindings 2
;; (global-set-key (kbd "C-q M-i") 'quoted-insert)
;; (global-set-key (kbd "C-x C-r") 'eval-region)

;; Whitespace
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")
;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))
(global-whitespace-mode 1)
(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Volitate
;; (package-install 'volatile-highlights)
;; (volatile-highlights-mode -1)


;; ;; 便利そうだけれども、結局あまり使えないのでオフにする
(package-install 'migemo)
(require 'migemo)
(setq migemo-command "cmigemo"
      migemo-options '("-q" "--emacs"))
(defun helm-compile-source--candidates-in-buffer (source)
  (helm-aif (assoc 'candidates-in-buffer source)
      (append source
              `((candidates
                 . ,(or (cdr it)
                        (lambda ()
                          ;; Do not use `source' because other plugins
                          ;; (such as helm-migemo) may change it
                          (helm-candidates-in-buffer (helm-get-current-source)))))
                (volatile) (match identity)))
    source))
;; [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
(defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
(defalias 'helm-mp-3-search-base 'helm-mm-3-search-base)
(helm-migemo-mode 0)
(helm-migemo-mode 1)
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"
      migemo-user-dictionary nil
      migemo-regex-dictionary nil
      migemo-coding-system 'utf-8-unix)
(migemo-init)

;; global Indirect buffer
(defvar indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'indirect-mode-name)
(defun indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode
         (if (not indirect-mode-name)
             (setq indirect-mode-name
                   (intern
                    (completing-read
                     "Mode: "
                     (mapcar (lambda (e)
                               (list (symbol-name e)))
                             (apropos-internal "-mode$" 'commandp))
                     nil t)))
           indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))
(global-set-key (kbd "C-c C-x b") 'indirect-region)

;; Rust
;; (el-get-bundle rust-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auctex python volatile-highlights yascroll company company-mode bm git-gutter-fringe popwin visual-regexp htmlize ox-reveal org-reveal git-gutter yasnippet-snippets helm-c-yasnippet yasnippet org-gcal org-dashboard rotate smartrep dired-hacks-utils org helm undo-tree solarized-theme smartparens sequential-command restart-emacs recentf-ext rainbow-mode openwith open-junk-file multiple-cursors migemo magit howm helm-swoop helm-ag expand-region dired-filter crux avy anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-install 'org-dashboard)

;;;###autoload
(defun my/kill-current-file-fullname ()
  (interactive)
  (message "\"%s\" is pushed to kill-ring."
           (kill-new (expand-file-name buffer-file-name))))
(global-set-key (kbd "C-l C-w p") 'my/kill-current-file-fullname)


(require 'ox "~/.emacs.d/elpa/org-20180709/ox.el")
(package-install 'ox-reveal)
(require 'ox-reveal)
(setq org-reveal-root (format "file://%s/hobby/reveal.js/" my/hdd-dir))
(package-install 'htmlize)
(require 'htmlize)
