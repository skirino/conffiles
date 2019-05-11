
;;;;;;;;;;;;;;;;;;;;;;;; emacs lispのpathを通す
(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(push "/usr/local/share/emacs/site-lisp" load-path)
(require 'cl)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; 環境変数
(require 'exec-path-from-shell)
(let ((envs '("PATH" "LD_LIBRARY_PATH" "http_proxy" "https_proxy" "no_proxy" "JAVA_HOME" "JDK_HOME")))
  (exec-path-from-shell-copy-envs envs))
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(require 'async-bytecomp)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; Macのキーボード
(when (eq system-type 'darwin)
  ;; swap command and option keys
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; ファイル関連
;; disable backup
(setq backup-inhibited t)

;; replace Emacs default auto-save
(setq auto-save-default nil)
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.0) ; アイドル1秒で保存
(auto-save-buffers-enhanced t)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; ファイル更新を検知
(global-auto-revert-mode)

;; 履歴を保存
(savehist-mode t)

;; 最近使ったファイル
(setq recentf-max-saved-items 2000)
(require 'recentf-ext)

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

;; (自作) 開いているファイルと関連したファイルを開く。関連するファイルは前もって登録しておく
(require 'open-related-file)
(global-set-key (kbd "C-c ; o") 'open-related-file-open)
;; rails
(open-related-file-append-group "%1/app/controllers/%2.rb" "%1/test/functional/%2_test.rb"  )
(open-related-file-append-group "%1/app/helpers/%2.rb"     "%1/test/unit/helpers/%2_test.rb")
(open-related-file-append-group "%1/app/models/%2.rb"      "%1/test/unit/%2_test.rb"        )
(open-related-file-append-group "%1/app/mailers/%2.rb"     "%1/test/mailers/%2_test.rb"     )
(open-related-file-append-group "%1/app/decorators/%2.rb"  "%1/test/decorators/%2_test.rb"  ) ;; decorators and their tests created by "draper" gem
;; Play! framework 2 for Scala
(open-related-file-append-group "%1/app/controllers/%2.scala" "%1/test/controllers/%2Spec.scala")
(open-related-file-append-group "%1/app/models/%2.scala"      "%1/test/models/%2Spec.scala"     )
(open-related-file-append-group "%1/app/utils/%2.scala"       "%1/test/utils/%2Spec.scala"      )
(open-related-file-append-group "%1/app/workers/%2.scala"     "%1/test/workers/%2Spec.scala"    )
(open-related-file-append-group "%1/app/extern/%2.scala"      "%1/test/extern/%2Spec.scala"     )
(open-related-file-append-group "%1/app/dtos/%2.scala"        "%1/test/dtos/%2Spec.scala"       )
;; Elixir mix project
(open-related-file-append-group "%1/lib/%2.ex"  "%1/test/%2_test.exs"     )
(open-related-file-append-group "%1/lib/%2.ex"  "%1/test/lib/%2_test.exs" )
(open-related-file-append-group "%1/core/%2.ex" "%1/test/core/%2_test.exs")
(open-related-file-append-group "%1/eal/%2.ex"  "%1/test/eal/%2_test.exs" )
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
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; 括弧の自動補完
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 検索
;; migemo
;; slightly modified version "~/.emacs.d/migemo.el" (toggle migemo by "M-m" in isearch-mode)
;; at present only on Linux:
(when (eq system-type 'gnu/linux)
  (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
)

;; anzuで検索のマッチ数をモードラインに表示
(require 'anzu)
(global-anzu-mode t)

;; ace-isearch: helm-swoopに移るところでエラーになる。原因不明
;(require 'ace-isearch)
;(global-ace-isearch-mode 1)

;; M-x grepの検索結果を編集してファイルに反映
(require 'wgrep)
(define-key grep-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode)

;; 自前grepコマンド定義
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
(require 'idle-highlight-mode)
(add-hook 'find-file-hook '(lambda () (idle-highlight-mode t)))
;;;;;;;;;;;;;;;;;;;;;;;; ハイライト表示



;;;;;;;;;;;;;;;;;;;;;;;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-)")       'mc/mark-next-like-this)
(global-set-key (kbd "C-(")       'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-(")   'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-)")   'mc/mark-all-like-this)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; undo & redo
(require 'undohist)
(undohist-initialize)

(require 'undo-tree)
(global-undo-tree-mode t)

;; C-\の入力切り替えは邪魔なのでundoにしておく
(global-set-key (kbd "C-\\") 'undo-tree-undo)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; カーソル移動
;; 最後の変更箇所にジャンプ
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

;; 自動インデント
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
(require 'ace-jump-mode)
(define-key global-map (kbd "C-:") 'ace-jump-mode)
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

;; カーソル位置から行頭まで削除する
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0)
)
;; C-uに設定、C-tにもとのC-uの機能を振る
(global-set-key (kbd "C-u") 'backward-kill-line)
(global-set-key (kbd "C-t") 'universal-argument)

;; popup-kill-ring
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)
(global-set-key (kbd "M-y") 'popup-kill-ring)
(setq popup-kill-ring-interactive-insert t)

;; 重複したエントリはkill-ringに入れず、順番を入れ替えるだけにする
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring))
)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 画面分割
;; "C-S-hjkl"でウィンドウ移動
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)
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

;; 要らないkey-bindingを無効化
(define-key org-mode-map [C-S-left]  nil)
(define-key org-mode-map [C-S-right] nil)
(define-key org-mode-map (kbd "C-,") nil)
(define-key org-mode-map [S-up]    nil)
(define-key org-mode-map [S-down]  nil)
(define-key org-mode-map [S-left]  nil)
(define-key org-mode-map [S-right] nil)
(define-key org-mode-map (kbd "C-c C-x C-c") nil)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; helm
;; Workaround to `void-function' while loading helm-mode: https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00351.html
(defun class-slot-initarg (class-name slot)
  (eieio--class-slot-initarg (eieio--class-v class-name) slot))

(require 'helm-mode)
;(require 'helm-git-grep)
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
(global-set-key (kbd "C-;"  ) 'helm-mini)
(global-set-key (kbd "C-+"  ) 'helm-resume)
;(global-set-key (kbd "C-c g") 'helm-git-grep)

(setq helm-locate-command "locate %s -A %s") ;; Enable AND search in helm-locate (instead regexp cannot be used)
(setq helm-mini-default-sources '(helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-locate))

;; helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "C-S-s") 'helm-swoop)
(define-key isearch-mode-map (kbd "C-S-s") 'helm-swoop-from-isearch) ;; When doing isearch, hand the word over to helm-swoop

;; don't show error message when keys that invoke helm commands multiple times
(define-key helm-map (kbd "M-x"  ) 'ignore)
(define-key helm-map (kbd "C-;"  ) 'ignore)
(define-key helm-map (kbd "C-+"  ) 'ignore)
(define-key helm-map (kbd "C-c g") 'ignore)
(define-key helm-map (kbd "C-S-s") 'ignore)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 日本語入力
;; 日本語 <=> 英数の切り替え(ibus等々にやらせる)
(global-set-key (kbd "S-C-SPC"          ) 'ignore)
(global-set-key (kbd "<zenkaku-hankaku>") 'ignore)
;;;;;;;;;;;;;;;;;;;;;;;; mozc


;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更
;; メニューバー、ツールバー、スクロールバーを消す, Emacs23以降
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(menu-bar-mode 0)

;; 現在行をハイライト
(global-hl-line-mode t)
(defface my-hl-line-face
  '((((class color) (background dark))  ; カラーかつ, 背景が dark ならば
     (:background "DarkSlateBlue" t))   ; 背景を黒に.
    (((class color) (background light)) ; カラーかつ, 背景が light でも
     (:background "DarkSlateBlue" t))   ; 背景を黒に.
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)

;; インデントの強調表示
(require 'highlight-indentation)
(add-hook 'find-file-hook 'highlight-indentation-mode)

;; 対応する括弧を表示させる
(show-paren-mode t)

;; 行番号・桁番号を表示
(line-number-mode t)
(column-number-mode t)

;; タブ、全角スペース、行末のスペースを見えるように (コメントを外すと改行が見えるように)
;;; インデント時にタブを使わないでスペースを使う
(setq-default tab-width 2 indent-tabs-mode nil)

;;(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray20"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
;;(defvar my-face-r-1 'my-face-r-1)
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
(defun my-start-gui-emacs ()
  (require 'server)
  (require 'with-editor)
  (unless (server-running-p) (server-start))
  ;; "C-x C-c"でemacsclientによる編集を終了
  (global-set-key (kbd "C-x C-c") 'server-edit)
  (global-set-key (kbd "C-c C-x C-c") 'save-buffers-kill-emacs)
  ;; "M-x exit"でemacsを終了
  (defalias 'exit 'save-buffers-kill-emacs)

  ;; elscreen
  (require 'elscreen)
  (elscreen-start)
  (global-set-key [C-S-right] 'elscreen-next)
  (global-set-key (kbd "C->") 'elscreen-next)
  (global-set-key [C-S-left]  'elscreen-previous)
  (global-set-key (kbd "C-<") 'elscreen-previous)

  ;; 新しくscreenを作ったら即分割する
  (defun my-elscreen-create ()
    (interactive)
    (elscreen-create)
    (split-window-horizontally)
  )
  (global-set-key (kbd "C-z c"  ) 'my-elscreen-create)
  (global-set-key (kbd "C-z C-c") 'my-elscreen-create)

  ;;;; Temporarily modify default-frame-alist and restore it after emacs startup to make emacsclient CUI better
  (setq original-default-frame-alist default-frame-alist)

  ;; GUIでの色付け
  (add-to-list 'default-frame-alist '(background-color . "black"))
  (add-to-list 'default-frame-alist '(foreground-color . "white"))
  ; Transparency: needs xcompmgr in Linux
  (set-frame-parameter (selected-frame) 'alpha '(75 50))
  (add-to-list 'default-frame-alist '(alpha 75 50))

  (run-with-idle-timer 3.0 nil '(lambda () (interactive)
                                  (setq default-frame-alist original-default-frame-alist)
                                  ))
)

(defun my-start-cui-emacs ()
  ;; ターミナルでマウスを有効にする
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up   5)))
)

(defun my-setup-appearances-in-xwindow ()
  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (set-face-background 'region "Blue")

  ;; maximize window (X Window System-specific implementation)
  (defun toggle-maximize (&optional f)
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  )
  (global-set-key (kbd "C-x RET RET") 'toggle-maximize)
  ;; 起動時に最大化して分割、少し間を置かないとレイアウトがおかしくなる
  (defun my-maximize-and-split ()
    (interactive)
    (toggle-maximize)
    (split-window-horizontally)
  )
  (run-with-idle-timer 0.5 nil 'my-maximize-and-split)

  ;; フォント設定
  (let (my-font-height my-font my-font-ja my-font-size my-fontset)
    (setq my-font-height 180)
    (setq my-font "DejaVu Sans Mono")
    (setq my-font-ja "IPAGothic")
    (setq face-font-rescale-alist '((my-font-ja . 1.20)))
    (set-face-attribute 'default nil :family my-font :height my-font-height)

    ;; 日本語文字に別のフォントを指定
    (when my-font-ja
      (let ((fn (or my-fontset (frame-parameter nil 'font)))
            (rg "iso10646-1"))
        (set-fontset-font fn 'katakana-jisx0201 `(,my-font-ja . ,rg))
        (set-fontset-font fn 'japanese-jisx0208 `(,my-font-ja . ,rg))
        (set-fontset-font fn 'japanese-jisx0212 `(,my-font-ja . ,rg)))
    )
  )
)

(if (display-graphic-p)
  (progn
    (my-start-gui-emacs)
    (when (eq system-type 'gnu/linux)
      (my-setup-appearances-in-xwindow))
  )
  (my-start-cui-emacs)
)
;;;;;;;;;;;;;;;;;;;;;;;; 見た目、環境依存



;;;;;;;;;;;;;;;;;;;;;;;; Git
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; git-gutter+ & git-gutter-fringe+
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)
(global-set-key (kbd "C-c n"  ) 'git-gutter+-next-hunk)
(global-set-key (kbd "C-c C-n") 'git-gutter+-next-hunk)
(global-set-key (kbd "C-c p"  ) 'git-gutter+-previous-hunk)
(global-set-key (kbd "C-c C-p") 'git-gutter+-previous-hunk)
(global-set-key (kbd "C-c s"  ) 'git-gutter+-stage-hunks)
(global-set-key (kbd "C-c C-s") 'git-gutter+-stage-hunks)

;; Workaround for git-gutter+'s bug on opening a file via symlink
(defadvice git-gutter+-process-diff (before git-gutter+-process-diff-advice activate)
  (ad-set-arg 0 (file-truename (ad-get-arg 0))))
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援
;; M-x compile
(global-set-key (kbd "C-x c") 'compile)


;; shell script
(setq sh-basic-offset 2)
(setq sh-indentation 2)


;; gdb
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t) ; "IO buffer" が必要ない場合は  nil で


;; D programming mode
(require 'd-mode)


;; go-mode
(require 'go-mode)


;; rust-mode
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(setq racer-rust-src-path "~/code/Rust/download/rust/src/")


;; python
(require 'python)
;; returnでカーソルのいた行もインデントするのはpythonだとダメ
(defun my-newline-and-indent ()
  (interactive)
  (newline)
  (indent-according-to-mode))
(define-key python-mode-map (kbd "RET") 'my-newline-and-indent)


;; Ruby
(require 'ruby-mode)

;; Java
(let* ((javac-path (shell-command-to-string "which javac"))
       (java-home (replace-regexp-in-string "/bin/javac" "" javac-path)))
  (setenv "JAVA_HOME" java-home)
  (setenv "JDK_HOME" java-home))


;; Scala
(require 'scala-mode)


;; YaTeX
(setq YaTeX-use-AMS-LaTeX t)


;; Haskell mode
(require 'haskell-mode)
;(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
;            (ghc-init)
            (flymake-mode)
            ))


;; Idris
(require 'idris-mode)


;; Coq
(require 'proof-site)


;; Erlang and Elixir
(require 'erlang-start)
(require 'elixir-mode)
(require 'alchemist)
(require 'flycheck-mix)
(flycheck-mix-setup)
(require 'ac-alchemist)
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)
;; erlang-modeに移動したあとでも戻れるようにする
(defun custom-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)


;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$"       . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(setq markdown-command "redcarpet --parse-tables")


;; GNU global
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援



;;;;;;;;;;;;;;;;;;;;;;;; auto-fill
(turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; flymake
(require 'flymake)
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
(setq flymake-check-start-time 5)

;; GUIのダイアログを抑制
(setq flymake-gui-warnings-enabled nil)

;; handler without makefile
(defun flymake-c-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "gcc" (list "-c" "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)

(defun flymake-cc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "g++" (list "-c" "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cxx$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cc$"  flymake-cc-init) flymake-allowed-file-name-masks)

(defun flymake-d-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "/opt/dmd/bin/dmd" (list "-unittest" "-c" "-w" "-I/opt/dmd/import/" "-I." "-I.." local-file))))
(push '("\\.d$" flymake-d-init) flymake-allowed-file-name-masks)
(add-to-list 'flymake-err-line-patterns
             '("^\\([^ :]+\\)(\\([0-9]+\\)): \\(.*\\)$" 1 2 nil 3))

(defun flymake-gjslint-init ()
  "Initialize flymake for gjslint"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace)))
    (list "gjslint" (list temp-file "--nosummary"))))
(push '("\\.js$" flymake-gjslint-init) flymake-allowed-file-name-masks)

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



;;;;;;;;;;;;;;;;;;;;;;;; flyspell
(when (eq system-type 'gnu/linux)

(require 'flyspell)
(ispell-change-dictionary "american")

;; 環境依存
(setq-default ispell-program-name
  (first
    (remove-if-not 'file-executable-p
                   '("/usr/bin/aspell" "/usr/local/bin/aspell" "/usr/bin/ispell" "/usr/local/bin/ispell")
    )
  )
)
;; aspellのときちょっと速くなるらしいけど観測可能な変化なし
;(setq ispell-list-command "list")

(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;; text-mode及びその亜種でオンにし、すぐにバッファ全体をチェック
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode t))))
;; texの記法は無視
(setq ispell-parser 'tex)
;; プログラムのコメントのスペルチェック
(dolist (hook '(c-mode-common-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;;; flyspell-mode を自動的に開始させたいファイルを指定 (お好みでアンコメントするなり, 変更するなり)
(add-to-list 'auto-mode-alist '("\\.tex" . flyspell-mode))
;;; 要らないkey-bindingを無効化
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-;") nil)

)
;;;;;;;;;;;;;;;;;;;;;;;; flyspell



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
 '(package-hidden-regexps (quote ("")))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
