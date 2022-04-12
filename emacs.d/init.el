;;;;;;;;;;;;;;;;;;;;;;;; straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; ファイル関連
;; disable backup
(setq backup-inhibited t)

;; replace Emacs default auto-save
(setq auto-save-default nil)
(use-package auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.0) ; アイドル1秒で保存
(auto-save-buffers-enhanced t)

;; session
(use-package session)
(add-hook 'after-init-hook 'session-initialize)

;; ファイル更新を検知
(global-auto-revert-mode)

;; 履歴を保存
(savehist-mode t)

;; ファイルの先頭が #! で始まるファイルに実行権限をつける
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; 新規ファイルの属するディレクトリがなかった場合は作成する
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

; disable bell
(setq ring-bell-function 'ignore)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; バッファ&ミニバッファ
;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.2)

;; ミニバッファの補完強化
(require 'ido)
(ido-mode t)
;;;;;;;;;;;;;;;;;;;;;;;; バッファ&ミニバッファ

;;;;;;;;;;;;;;;;;;;;;;;; 補完
;; 略語展開・補完
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
       ))
(global-set-key (kbd "M-/") 'hippie-expand)

;; auto-complete
(use-package auto-complete)
(global-auto-complete-mode t)

;; 括弧の自動補完
(use-package smartparens)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; Git
(setq vc-follow-symlinks t)
; Autoload on change in symlinked file
(setq auto-revert-check-vc-info t)

;; git-gutter+
(use-package git-gutter+)
(global-git-gutter+-mode t)
(global-set-key (kbd "C-c n"  ) 'git-gutter+-next-hunk)
(global-set-key (kbd "C-c C-n") 'git-gutter+-next-hunk)
(global-set-key (kbd "C-c p"  ) 'git-gutter+-previous-hunk)
(global-set-key (kbd "C-c C-p") 'git-gutter+-previous-hunk)
(global-set-key (kbd "C-c s"  ) 'git-gutter+-stage-hunks)
(global-set-key (kbd "C-c C-s") 'git-gutter+-stage-hunks)
(global-set-key (kbd "C-c d"  ) 'git-gutter+-popup-hunk)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; 検索
;; M-x grepの検索結果を編集してファイルに反映
(use-package wgrep)
(define-key grep-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode)

;; 自前grepコマンド定義
(use-package magit)
(setq grep-use-null-device nil)

(defun my-grep ()
  (interactive)
  (my-grep-impl ""))
(global-set-key (kbd "C-x g") 'my-grep)

(defun my-grep-i ()
  (interactive)
  (my-grep-impl "-i"))
(global-set-key (kbd "C-x G") 'my-grep-i)

(defun my-grep-impl (ignore-case-option)
  (let* ((word (or (thing-at-point 'symbol) ""))
         (search-pattern (read-string (format "Search for (%s): " word) nil nil word))
         (toplevel (magit-rev-parse "--show-toplevel")))
    (if (and (buffer-file-name) (stringp toplevel) (string-prefix-p toplevel (buffer-file-name)))
        (my-git-grep search-pattern ignore-case-option)
      (my-rgrep search-pattern ignore-case-option))
    (my-grep-place-buffers)))

(defun my-git-grep (search-pattern ignore-case-option)
  (let* ((repo-dir (concat (magit-rev-parse "--show-toplevel") "/"))
         (rel-path (substring (file-name-directory (buffer-file-name)) (length repo-dir)))
         (dir-arg  (replace-regexp-in-string "[^/]+" ".." rel-path)))
    (grep (format "git --no-pager grep --no-color -nI %s -e '%s' %s" ignore-case-option search-pattern dir-arg))))

(defun my-rgrep (search-pattern ignore-case-option)
  (grep (format "grep %s -rnH -e '%s' ." ignore-case-option search-pattern)))

(defun my-grep-place-buffers ()
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer "*grep*")
  (other-window 1))
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; ハイライト表示
(use-package idle-highlight-mode
  :config
  (global-idle-highlight-mode)
  (set-face-attribute 'idle-highlight nil :background "#030")
)
;;;;;;;;;;;;;;;;;;;;;;;; ハイライト表示

;;;;;;;;;;;;;;;;;;;;;;;; undo & redo
;; C-\の入力切り替えは邪魔なのでundoにしておく
(global-set-key (kbd "C-\\") 'undo)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; カーソル移動
;; 最後の変更箇所にジャンプ
(use-package goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)

;; 自動インデント
(use-package smart-newline)
(global-set-key (kbd "C-m") 'smart-newline)

;; "C-h"をbackspaceに (これで<C-backspace>が反応しなくなるので、bindしなおす)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

;; "C-a"で「行頭」と「インデントを飛ばした行頭」を行き来する
(defun u-move-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key (kbd "C-a") 'u-move-beginning-of-line)

;; 物理行単位で移動
(setq line-move-visual nil)

;; Hit a hint
(use-package ace-jump-mode)
(define-key global-map (kbd "M-SPC") 'ace-jump-word-mode)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; scrolling
(setq scroll-margin 5)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; kill ring、リージョン選択
;; clipboard連携
(when (eq system-type 'gnu/linux)
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "xsel --clipboard --output")))
  (setq interprogram-cut-function
        (lambda (text &optional rest)
          (let* ((process-connection-type nil)
                 (proc (start-process "xsel" "*Messages*" "xsel" "--clipboard" "--input")))
            (process-send-string proc text)
            (process-send-eof proc))))
)
(when (eq system-type 'darwin)
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "pbpaste")))
  (setq interprogram-cut-function
        (lambda (text &optional rest)
          (let* ((process-connection-type nil)
                 (proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))
)

;; カーソル位置から行頭まで削除する
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the beginning of the line."
  (interactive "p")
  (kill-line 0)
)
;; C-uに設定、C-tにもとのC-uの機能を振る
(global-set-key (kbd "C-u") 'backward-kill-line)
(global-set-key (kbd "C-t") 'universal-argument)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; CUA
(cua-mode t)
(setq cua-enable-cua-keys nil)                         ;; C-cやC-vの乗っ取りを阻止
(define-key cua-global-keymap (kbd "C-S-SPC") 'ignore) ;; C-S-SPCを空ける(日本語モード => 戻す)
(define-key cua-global-keymap (kbd "C-M-j") 'cua-set-rectangle-mark) ;; To use this also in terminal
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; align
(global-set-key (kbd "C-c a"  ) 'align       )
(global-set-key (kbd "C-c C-a") 'align-regexp)

(defun align-commas (beg end)
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)," 1 1 t))
(global-set-key (kbd "C-c ,") 'algin-commas)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; org-mode
(require 'org)

;; 開いたときには一旦全展開
(setq org-startup-folded nil)

(defun my-org-visit-file (filepath)
  (let ((osf-orig org-startup-folded))
    (setq org-startup-folded nil)
    (find-file filepath)
    (setq org-startup-folded osf-orig)))
(defun my-org-visit-memo-file ()
  (interactive)
  (my-org-visit-file "~/docs/memo.org"))
(global-set-key (kbd "C-c m") 'my-org-visit-memo-file)
(global-set-key (kbd "C-c l") '(lambda () (interactive) (find-file "~/vbshare/log")))
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; helm
;; Workaround to `void-function' while loading helm-mode: https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00351.html
(defun class-slot-initarg (class-name slot)
  (eieio--class-slot-initarg (eieio--class-v class-name) slot))

(use-package helm
  :config
  (set-face-attribute 'helm-selection nil :background "#005")
  )
(use-package helm-git-grep)
(helm-mode 1)
(setq helm-input-idle-delay 0.1)
(setq helm-truncate-lines t)
(setq helm-buffer-max-length 50)
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil)) ;; Don't use helm in specific commands

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(setq helm-delete-minibuffer-contents-from-point t)
(define-key helm-map (kbd "C-c C-k") 'helm-buffer-run-kill-persistent)

;; helm commands
(global-set-key (kbd "M-x"  ) 'helm-M-x)
(global-set-key (kbd "C-]"  ) 'helm-mini)
(global-set-key (kbd "C-c g") 'helm-git-grep)

(when (eq system-type 'gnu/linux)
  (setq helm-locate-command "locate %s -A %s") ;; Enable AND search in helm-locate (instead regexp cannot be used)
)
(when (eq system-type 'darwin)
  (setq helm-locate-command "mdfind -name %s %s") ;; Note that AND search isn't working
)
(setq helm-mini-default-sources '(helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-locate))

;; helm-swoop
(use-package helm-swoop)
(global-set-key (kbd "C-M-s") 'helm-swoop)
(define-key isearch-mode-map (kbd "C-M-s") 'helm-swoop-from-isearch) ;; When doing isearch, hand the word over to helm-swoop
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更
;; メニューバー、ツールバー、スクロールバーを消す, Emacs23以降
(tool-bar-mode 0)
(menu-bar-mode 0)

;; 現在行をハイライト
(global-hl-line-mode t)
(set-face-background 'hl-line "#222")
(set-face-attribute 'region nil :background "#007")

(use-package volatile-highlights)
(volatile-highlights-mode t)

;; インデントの強調表示
(use-package highlight-indentation
  :config
  (add-hook 'find-file-hook 'highlight-indentation-mode)
  (set-face-background 'highlight-indentation-face "#555")
)

;; 対応する括弧を表示させる
(show-paren-mode t)

;; 行番号・桁番号を表示
(line-number-mode t)
(column-number-mode t)

;; タブ、全角スペース、行末のスペースを見えるように (コメントを外すと改行が見えるように)
;;; インデント時にタブを使わないでスペースを使う
(setq-default tab-width 2 indent-tabs-mode nil)

(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray20"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("[ \t]+$" 0 my-face-u-1 append)
               ("\t" 0 'my-face-b-2 append)
               ("　" 0 my-face-b-1 append)
               ))))
;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更

;;;;;;;;;;;;;;;;;;;;;;;; 見た目、環境依存
(require 'server)
(unless (server-running-p) (server-start))

;; ターミナルでマウスを有効にする
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up   5)))
;;;;;;;;;;;;;;;;;;;;;;;; 見た目、環境依存

;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援
;; shell script
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; python
(require 'python)
(setq python-indent-guess-indent-offset nil)
;; returnでカーソルのいた行もインデントするのはpythonだとダメ
(defun my-newline-and-indent ()
  (interactive)
  (newline)
  (indent-according-to-mode))
(define-key python-mode-map (kbd "RET") 'my-newline-and-indent)

;; Ruby
(require 'ruby-mode)

(use-package vala-mode
  :config
  (add-hook 'vala-mode-hook (lambda ()
                              (setq c-basic-offset 2)
                              (setq indent-tabs-mode nil)
                              ))
  )

;; jsonnet
(use-package jsonnet-mode)

;; Erlang and Elixir
(use-package elixir-mode)
(use-package alchemist)
(use-package flycheck-mix
  :config
  (flycheck-mix-setup)
  )
(use-package ac-alchemist
  :config
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup)
  )

;; terraform
(use-package terraform-mode
  :config
  (define-key terraform-mode-map (kbd "C-c f") 'terraform-format-buffer)
)

;; bazel
(use-package bazel)
(add-to-list 'auto-mode-alist '("\\.bazel$"  . bazel-mode))
(add-to-list 'auto-mode-alist '("BUILD$"     . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE$" . bazel-mode))

;; markdown mode
(use-package markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$"       . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(setq markdown-command "redcarpet --parse-tables")
;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援

;;;;;;;;;;;;;;;;;;;;;;;; auto-fill
(turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; flymake
(require 'flymake)
(use-package flycheck
  :config
  (setq flymake-check-start-time 5)
  )

;; GUIのダイアログを抑制
(setq flymake-gui-warnings-enabled nil)

;; 警告エラー行にカーソルがあれば、内容をMinibuf に出力
(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))
(add-hook 'post-command-hook 'my-flymake-show-help)

;; M-P/M-N で警告/エラー行に移動
(defun my-goto-prev-error ()
  (interactive)
  (flymake-goto-prev-error)
  (previous-error))
(defun my-goto-next-error ()
  (interactive)
  (flymake-goto-next-error)
  (next-error))
(global-set-key (kbd "M-P") 'my-goto-prev-error)
(global-set-key (kbd "M-N") 'my-goto-next-error)
;;;;;;;;;;;;;;;;;;;;;;;; flymake

;;;;;;;;;;;;;;;;;;;;;;;; 文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
