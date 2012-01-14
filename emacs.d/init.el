
;;;;;;;;;;;;;;;;;;;;;;;; emacs lispのpathを通す
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(add-to-list 'load-path "~/.emacs.d/manual-install/")
(add-to-list 'load-path "~/.emacs.d/sdic/")
(add-to-list 'load-path "~/.emacs.d/apel-10.8/")
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; Macのキーボード
(if (eq system-type 'darwin)
    (progn
      ;; swap command and option keys
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 子プロセスのために、PATH環境変数を変更しておく
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 文字コード
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; ファイル関連
;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)

;; auto-save
(require 'auto-save-buffers)
(run-with-idle-timer 1 t 'auto-save-buffers) ; アイドル1秒で保存

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; 履歴を保存
(savehist-mode 1)

;; 最近使ったファイル
(setq recentf-max-saved-items 2000)
(require 'recentf-ext)

;; ファイルの先頭が #! で始まるファイルに実行権限をつける。
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; dired
;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; diredでフォルダを開く時, 新しいバッファを作成しない
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
  (function
   (lambda nil (interactive) (dired-single-buffer "..")))))
   ;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
  ;; we're good to go; just add our bindings
  (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; バッファ&ミニバッファ
;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.2)


;; バッファ切り替えを強化
(iswitchb-mode 1)
(iswitchb-default-keybindings)
(setq read-buffer-function 'iswitchb-read-buffer)
;; 部分文字列の代わりに正規表現を使う場合は t に設定する
(setq iswitchb-regexp nil)
;; 新しいバッファを作成するときにいちいち聞かないように
(setq iswitchb-prompt-newbuffer nil)


;; ファイル名がかぶったときのバッファ名
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;; mcompleteでミニバッファ編集時の候補を列挙
(autoload 'mcomplete-mode "mcomplete"
  "Toggle minibuffer completion with prefix and substring matching."
  t nil)
(autoload 'turn-on-mcomplete-mode "mcomplete"
  "Turn on minibuffer completion with prefix and substring matching."
  t nil)
(autoload 'turn-off-mcomplete-mode "mcomplete"
  "Turn off minibuffer completion with prefix and substring matching."
  t nil)
(turn-on-mcomplete-mode)


;; バッファ切り替え (C-, C-.)
(defvar my-ignore-blst             ; 移動の際に無視するバッファのリスト
  '("*Help*" "*Compile-Log*" "*Mew completions*" "*Completions*"
    "*Shell Command Output*" "*Apropos*" "*Buffer List*"
    "*anything*" "*anything minibuffer-history*" "*anything complete*" "*my-anything*"))
(defvar my-visible-blst nil)       ; 移動開始時の buffer list を保存
(defvar my-bslen 15)               ; buffer list 中の buffer name の最大長
(defvar my-blist-display-time 2)   ; buffer list の表示時間
(defface my-cbface                 ; buffer list 中で current buffer を示す face
  '((t (:foreground "wheat" :underline t))) nil)

(defun my-visible-buffers (blst)
  (if (eq blst nil) '()
    (let ((bufn (buffer-name (car blst))))
      (if (or (= (aref bufn 0) ? ) (member bufn my-ignore-blst))
          ;; ミニバッファと無視するバッファには移動しない
          (my-visible-buffers (cdr blst))
        (cons (car blst) (my-visible-buffers (cdr blst)))))))

(defun my-show-buffer-list (prompt spliter)
  (let* ((len (string-width prompt))
         (str (mapconcat
               (lambda (buf)
                 (let ((bs (copy-sequence (buffer-name buf))))
                   (when (> (string-width bs) my-bslen) ;; 切り詰め
                     (setq bs (concat (substring bs 0 (- my-bslen 2)) "..")))
                   (setq len (+ len (string-width (concat bs spliter))))
                   (when (eq buf (current-buffer)) ;; 現在のバッファは強調表示
                     (put-text-property 0 (length bs) 'face 'my-cbface bs))
                   (cond ((>= len (frame-width)) ;; frame 幅で適宜改行
                          (setq len (+ (string-width (concat prompt bs spliter))))
                          (concat "\n" (make-string (string-width prompt) ? ) bs))
                         (t bs))))
               my-visible-blst spliter)))
    (let (message-log-max)
      (message "%s" (concat prompt str))
      (when (sit-for my-blist-display-time) (message nil)))))

(defun my-operate-buffer (pos)
  (unless (window-minibuffer-p (selected-window));; ミニバッファ以外で
    (unless (eq last-command 'my-operate-buffer)
      ;; 直前にバッファを切り替えてなければバッファリストを更新
      (setq my-visible-blst (my-visible-buffers (buffer-list))))
    (let* ((blst (if pos my-visible-blst (reverse my-visible-blst))))
      (switch-to-buffer (or (cadr (memq (current-buffer) blst)) (car blst))))
    (my-show-buffer-list (if pos "[-->] " "[<--] ") (if pos " > "  " < " )))
  (setq this-command 'my-operate-buffer))

(global-set-key [?\C-,] (lambda () (interactive) (my-operate-buffer nil)))
(global-set-key [?\C-.] (lambda () (interactive) (my-operate-buffer t)))
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
(global-auto-complete-mode 1)

;; 括弧などの自動補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c/")
(require 'yasnippet)
(require 'yasnippet-config)
(yas/setup "~/.emacs.d/yasnippet-0.6.1c/")
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 検索
;; migemo, slightly modified version "~/.emacs.d/migemo.el" (toggle migemo by "M-m" in isearch-mode)
;(require 'migemo)

;; M-x grepの検索結果を編集してファイルに反映
(require 'grep-edit)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; undo & redo
(add-to-list 'load-path "~/.emacs.d/undo-tree/")
(require 'undo-tree)
(global-undo-tree-mode)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; カーソル移動
;; 最後の変更箇所にジャンプ
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

;; 自動インデント、RetやC-jも自動インデントになる
;; (デフォルトのnewline-and-indentは空行のインデントを消してしまう)
(defun my-newline-and-indent ()
  (interactive)
  (newline)
  (indent-according-to-mode))
(defun my-indent-newline-and-indent ()
  (interactive)
  (indent-according-to-mode)
  (newline)
  (indent-according-to-mode))
(global-set-key "\C-m" 'my-indent-newline-and-indent)

;; "C-h"をbackspaceに (これで<C-backspace>が反応しなくなるので、bindしなおす)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)


;; "C-a"で「行頭」と「インデントを飛ばした行頭」を行き来する
(defun u-move-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
      (beginning-of-line)))
(define-key global-map [(C a)] 'u-move-beginning-of-line)

;; 物理行単位で移動
(setq line-move-visual nil)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; kill ring、リージョン選択
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
(global-set-key "\M-y" 'popup-kill-ring)
(setq popup-kill-ring-interactive-insert t)


;; 重複したエントリはkill-ringに入れず、順番を入れ替えるだけにする
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring))
)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; key-chord
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 画面分割
;; "C-=" (C-S--), "C-|"で画面分割
(global-set-key (kbd "C-=") 'split-window-vertically)
(global-set-key (kbd "C-|") 'split-window-horizontally)

;; "S-<Up>"などでウィンドウ移動、C-S-hjkl
(windmove-default-keybindings)
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)

;; follow-mode
(require 'follow)
(defun my-toggle-follow-mode ()
  (interactive)
  (if (eq follow-mode t)
      (progn ; On => follow-modeをオフにして2分割に戻る
        (follow-mode nil)
        (delete-other-windows)
        (split-window-horizontally)
        (balance-windows)
      )
      (progn ; Off => 3分割してfollow-mode
        (delete-other-windows)
        (split-window-horizontally)
        (split-window-horizontally)
        (balance-windows)
        (follow-mode)
      )
  )
)
(key-chord-define-global "fw" 'my-toggle-follow-mode)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; CUA
(cua-mode t)
;; C-cやC-vの乗っ取りを阻止
(setq cua-enable-cua-keys nil)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; view-mode
(require 'view)
(setq view-read-only t)
(key-chord-define-global "vm" 'view-mode)

(define-key view-mode-map (kbd "/") 'isearch-forward)
(define-key view-mode-map (kbd "G") 'end-of-buffer)
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "j") 'next-line)
(define-key view-mode-map (kbd "k") 'previous-line)
(define-key view-mode-map (kbd "l") 'forward-char)

(require 'viewer)
;; 書き込み不能なファイルではviewer-modeから抜けない
(viewer-stay-in-setup)

;; モードラインに色をつける
(setq viewer-modeline-color-unwritable "tomato")
(setq viewer-modeline-color-view "orange")
(viewer-change-modeline-color-setup)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; picture-mode
(key-chord-define-global "pc" 'picture-mode)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; org-mode
;; org-modeでメモを取る, Emacs23以降
(if (>= emacs-major-version 23)
    (progn
      (require 'org)
      (org-remember-insinuate)                                                  ; org-rememberの初期化
      (setq org-directory "~/docs/")                                            ; メモを格納するorgファイルの設定
      (setq org-default-notes-file (expand-file-name "memo.org" org-directory)) ; メモファイル
      (setq org-remember-templates
            '(("Note" ?n "** %?\n   %i\n   %a\n   %t" nil "Inbox"))
            )
      (global-set-key (kbd "M-m") 'org-remember)

      ;; 要らないkey-bindingを無効化
      (define-key org-mode-map [C-S-left]  nil)
      (define-key org-mode-map [C-S-right] nil)
      (define-key org-mode-map (kbd "C-,") nil)
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; Git (egg.el)
(require 'egg)

;; C-x v fでファイル単位のログを出す
(define-key egg-file-cmd-map "f" 'egg-file-log)

;; C-x v dでファイルのdiff
(define-key egg-file-cmd-map (kbd "d") 'egg-file-diff)

;; ログバッファ内、dで現在のworking copyとコミットとのdiff表示
(define-key egg-log-commit-map "d" 'egg-log-buffer-diff-revs)

;; ログバッファ内、spaceでdiff表示をtoggleする
(defun egg-log-buffer-hide-show-dwim ()
  (interactive)
  (let* ((pos (point))
         (next (next-single-property-change pos :diff))
         (sha1 (and next (get-text-property next :commit)))
         (nav (get-text-property pos :navigation)))
    (if (equal (get-text-property pos :commit) sha1)
        (egg-section-cmd-toggle-hide-show nav)
        (egg-log-buffer-do-insert-commit pos)
    )
  )
)
(define-key egg-log-commit-base-map (kbd "SPC") 'egg-log-buffer-hide-show-dwim)
(define-key egg-hide-show-map (kbd "SPC") 'egg-section-cmd-toggle-hide-show)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; anything
(require 'anything-config)
(require 'anything-match-plugin)
(setq anything-input-idle-delay 0.1)
(defun my-anything-command ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-emacs-commands
     anything-c-source-buffers+
     anything-c-source-recentf)
   "*my-anything*"))
(global-set-key (kbd "M-x") 'my-anything-command)
(defun my-anything-for-file ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers+
     anything-c-source-recentf
     anything-c-source-files-in-current-dir+
     anything-c-source-filelist)
   "*my-anything*"))
(global-set-key (kbd "C-;") 'my-anything-for-file)

;; setup filelist (platform dependent)
;(setq my-filelist-ramfs "/dev/shm/")
(setq my-filelist-ramfs "~/.emacs.d/")
(setq my-filelist-basename "home.filelist")
(setq anything-c-filelist-file-name (concat my-filelist-ramfs my-filelist-basename))
(setq my-persistent-filelist-directory "~/.emacs.d/")
(setq my-persistent-filelist (concat my-persistent-filelist-directory my-filelist-basename))
(defun update-anything-persistent-filelist ()
  (interactive)
  (shell-command
   (concat "~/.emacs.d/make-filelist.py > " my-persistent-filelist " &")
  )
)
(defun update-anything-filelist ()
  (interactive)
  (shell-command
   (concat "cp " my-persistent-filelist " " my-filelist-ramfs)
  )
)
(update-anything-filelist)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更
;; メニューバー、ツールバー、スクロールバーを消す, Emacs23以降
(if (>= emacs-major-version 23)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)
    )
)

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


;; 対応する括弧を表示させる
(show-paren-mode 1)


;; 行番号・桁番号を表示
(line-number-mode 1)
(column-number-mode 1)


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

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     ;;("[\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更



;;;;;;;;;;;;;;;;;;;;;;;; 見た目、環境依存
(defun my-start-gui-emacs ()
  ;; start server for emacsclient
  (require 'server)
  (unless (server-running-p)
    (server-start)
  )
  ;; "C-x C-c"でサーバをkill
  (global-set-key (kbd "C-x C-c") 'server-edit)
  ;; "M-x exit"でemacsを終了
  (defalias 'exit 'save-buffers-kill-emacs)

  ;; auto-install
  (require 'auto-install)
  ;; ネットワークアクセスを伴って時間がかかるのでコメントアウト
  ;(auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; elscreen
  (load "elscreen" "ElScreen" t)
  (global-set-key [C-S-right] 'elscreen-next)
  (global-set-key (kbd "C->") 'elscreen-next)
  (global-set-key [C-S-left]  'elscreen-previous)
  (global-set-key (kbd "C-<")  'elscreen-previous)

  ;; 新しくscreenを作ったら即分割する
  (defun my-elscreen-create ()
    (interactive)
    (elscreen-create)
    (split-window-horizontally)
  )
  (global-set-key (kbd "C-z c") 'my-elscreen-create)
  (global-set-key (kbd "C-z C-c") 'my-elscreen-create)


  ;; GUIでの色付け
  (add-to-list 'default-frame-alist '(background-color . "black"))
  (add-to-list 'default-frame-alist '(foreground-color . "white"))
)

(defun my-start-cui-emacs ()
  ;; ターミナルでマウスを有効にする
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 5)))

  ;; CUIではdefault-frame-alistを使わないほうが良さげ
  (set-background-color "black")
)


;;;;;;; Linux
(if (eq system-type 'gnu/linux)
  (progn
    (if (eq window-system 'x)
      (progn ;; GUI
        (my-start-gui-emacs)

        (add-to-list 'default-frame-alist '(cursor-color . "white"))
        (set-face-background 'region "Blue")

        ;; fullscreen
        (defun toggle-fullscreen (&optional f)
          (interactive)
          (let ((current-value (frame-parameter nil 'fullscreen)))
            (set-frame-parameter nil 'fullscreen
                                 (if (equal 'fullboth current-value)
                                     (if (boundp 'old-fullscreen) old-fullscreen nil)
                                   (progn (setq old-fullscreen current-value) 'fullboth)
                                 )
            )
          )
        )
        (global-set-key [f11] 'toggle-fullscreen)

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
      )
      (progn ;; CUI
        (my-start-cui-emacs)
        (add-to-list 'default-frame-alist '(foreground-color . "brightwhite"))
        (add-to-list 'default-frame-alist '(cursor-color . "brightwhite"))
      )
    )

    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :background nil :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Takaoゴシック")))))
  )
)


;;;;;;; Mac
(if (eq system-type 'darwin)
  (progn
    (if (eq window-system 'ns) ;; Cocoa Emacs (GUI)
      (progn
        (my-start-gui-emacs)

        (defun my-maximize-frame ()
          (interactive)
          (set-frame-position (selected-frame) 0 0)
          (set-frame-size (selected-frame) 1000 1000)
          (split-window-horizontally)
        )
        (run-with-idle-timer 0.5 nil 'my-maximize-frame)

        (custom-set-faces
         '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "apple" :family "Monaco"))))
        )

        (set-fontset-font
          (frame-parameter nil 'font)
          'japanese-jisx0208
          '("Hiragino Kaku Gothic Pro" . "iso10646-1")
        )
      )
      (progn ;; no window, "emacs -nw"
        (my-start-cui-emacs)
      )
    )
  )
)


;;;;;;; Windows
(defun set-font-on-windows ()
  (set-face-attribute 'default nil :family "ＭＳ ゴシック")
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("ＭＳ ゴシック" . "jisx0208-sjis"))
)

(if (eq system-type 'windows-nt) ;; GNU Emacs on Windows (GUI)
  (progn
    (set-font-on-windows)
    (add-to-list 'default-frame-alist '(background-color . "black"))

    ;; Windows用の最大化
    (defun my-maximize ()
      (interactive)
      (w32-send-sys-command #xf030))
    (run-with-idle-timer 0.2 nil 'my-maximize)

    ;; GUI用の初期化(server-startなど)
    (if (eq window-system 'w32)
      (progn
        (my-start-gui-emacs)
      )
    )
  )
)
;; On Cygwin command-line (CUI)
(if (eq system-type 'cygwin)
  (progn
    (set-font-on-windows)
    (my-start-cui-emacs)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;; 見た目、環境依存



;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援
;; M-x compile
(global-set-key "\C-xc" 'compile)


;; C/C++
;; taken from libssh2-style.el
(defconst libssh2-c-style
  '((c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open before after)))
    (c-offsets-alist . ((topmost-intro        . 0)
                        (topmost-intro-cont   . 0)
                        (substatement         . +)
                        (substatement-open    . 0)
                        (statement-case-intro . +)
                        (statement-case-open  . 0)
                        (case-label           . 0)
                        ))
  )
  "Libssh2 C Programming Style"
)

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun libssh2-c-mode-common-hook ()
  "Libssh2 C mode hook"
  ;; add libssh2 style and set it for the current buffer
  (c-add-style "libssh2" libssh2-c-style t)
  (setq tab-width 8
        indent-tabs-mode nil            ; Use spaces, not tabs.
        comment-column 40
        c-font-lock-extra-types (append '("libssh2_int64_t" "LIBSSH2_USERAUTH_KBDINT_PROMPT" "LIBSSH2_SESSION" "LIBSSH2_CHANNEL" "ssize_t" "size_t" "uint32_t" "LIBSSH2_LISTENER" "LIBSSH2_POLLFD"))
        )
  ;; keybindings for C, C++, and Objective-C.  We can put these in
  ;; c-mode-base-map because of inheritance ...
  (define-key c-mode-base-map "\M-q" 'c-fill-paragraph)
  (setq c-recognize-knr-p nil)

  ;; disable c-electric-paren and use skeleton-pair-insert
  (define-key c-mode-base-map "(" nil)
  (define-key c-mode-base-map ")" nil)
  (define-key c-mode-base-map "{" nil)
  (define-key c-mode-base-map "}" nil)
)
(add-hook 'c-mode-common-hook 'libssh2-c-mode-common-hook)


;; gdb
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t) ; "IO buffer" が必要ない場合は  nil で


;; D programming mode
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))


;; go-mode
(require 'go-mode-load)


;; vala-mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))


;; python, genieもpython-modeにする
(require 'python)
(add-to-list 'auto-mode-alist '("\\.gs$" . python-mode))
;; returnでカーソルのいた行もインデントするのはpythonだとダメ
(define-key python-mode-map (kbd "RET") 'my-newline-and-indent)


;; ruby, reset ruby-mode's sucking bindings
(add-to-list 'load-path "~/.emacs.d/rinari/")
(require 'starter-kit-ruby)
(define-key rinari-minor-mode-map (kbd "C-c ; w") 'rinari-web-server-restart)
;; to run tests using spork, overwrite the bin name
(setq ruby-compilation-executable "testdrb")


;; YaTeX
(setq YaTeX-use-AMS-LaTeX t)


;; GNU global
(when (locate-library "gtags") (require 'gtags))
(global-set-key "\M-t" 'gtags-find-tag)     ;関数の定義元へ
(global-set-key "\M-r" 'gtags-find-rtag)    ;関数の参照先へ
(global-set-key "\M-s" 'gtags-find-symbol)  ;変数の定義元/参照先へ
;(global-set-key "\M-f" 'gtags-find-file)    ;ファイルにジャンプ (forward-wordを優先してコメントアウト)
(global-set-key "\M-t" 'gtags-pop-stack)    ;前のバッファに戻る
(add-hook 'c-mode-common-hook 'gtags-mode)
(add-hook 'c++-mode-hook 'gtags-mode)
(add-hook 'java-mode-hook 'gtags-mode)
;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援



;;;;;;;;;;;;;;;;;;;;;;;; flymake & gccsense
(require 'flymake)
(key-chord-define-global "fm" 'flymake-mode)

;; gccsenseおよびgccsense-flymake
(require 'gccsense)
(add-hook 'c-mode-common-hook
          (lambda ()
            (flymake-mode)
            (local-set-key (kbd "C-.") 'ac-complete-gccsense)   ;gccsense補完
            (local-set-key (kbd "C-c .") 'ac-complete-gccsense) ;gccsense補完
            (gccsense-flymake-setup)
            ))

;; handler without makefile
(defun flymake-c-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "gcc" (list "-c" "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-c" "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cxx$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cc$" flymake-cc-init) flymake-allowed-file-name-masks)
(defun flymake-d-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "gdc" (list "-c" "-Wall" "-Wextra" "-fsyntax-only" "-I/home/skirino/code/D/dsss/include/d/" local-file))))
(push '("\\.d$" flymake-d-init) flymake-allowed-file-name-masks)

;; 警告エラー行にカーソルがあれば、内容をMinibuf に出力
(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))
(add-hook 'post-command-hook 'my-flymake-show-help)

;; M-p/M-n で警告/エラー行に移動
(defun my-goto-prev-error ()
  (interactive)
  (flymake-goto-prev-error)
  (previous-error))
(defun my-goto-next-error ()
  (interactive)
  (flymake-goto-next-error)
  (next-error))
(global-set-key "\M-p" 'my-goto-prev-error)
(global-set-key "\M-n" 'my-goto-next-error)
;;;;;;;;;;;;;;;;;;;;;;;; flymake & gccsense



;;;;;;;;;;;;;;;;;;;;;;;; flyspell
(if (eq system-type 'gnu/linux) (progn

(require 'flyspell)
(ispell-change-dictionary "american")
;; 日本語ファイル中のスペルチェックを可能にする

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
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (flyspell-buffer))))
;; texの記法は無視
(setq ispell-parser 'tex)
;; プログラムのコメントのスペルチェック
(dolist (hook '(c-mode-common-hook))
  (add-hook hook (lambda ()
                   (flyspell-prog-mode)
                   (flyspell-buffer))));; スタートと同時にチェックはうまくいってない

;; flyspellの修正候補をpopup.elで表示する
(defun flyspell-correct-word-popup-el ()
  "Pop up a menu of possible corrections for misspelled word before point."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
        (word (flyspell-get-word nil)))
    (if (consp word)
        (let ((start (car (cdr word)))
              (end (car (cdr (cdr word))))
              (word (car word))
              poss ispell-filter)
          ;; now check spelling of word.
          (ispell-send-string "%\n") ;put in verbose mode
          (ispell-send-string (concat "^" word "\n"))
          ;; wait until ispell has processed word
          (while (progn
                   (accept-process-output ispell-process)
                   (not (string= "" (car ispell-filter)))))
          ;; Remove leading empty element
          (setq ispell-filter (cdr ispell-filter))
          ;; ispell process should return something after word is sent.
          ;; Tag word as valid (i.e., skip) otherwise
          (or ispell-filter
              (setq ispell-filter '(*)))
          (if (consp ispell-filter)
              (setq poss (ispell-parse-output (car ispell-filter))))
          (cond
           ((or (eq poss t) (stringp poss))
            ;; don't correct word
            t)
           ((null poss)
            ;; ispell error
            (error "Ispell: error in Ispell process"))
           (t
            ;; The word is incorrect, we have to propose a replacement.
            (flyspell-do-correct (popup-menu* (car (cddr poss)) :scroll-bar t :margin t)
                                 poss word cursor-location start end cursor-location)))
          (ispell-pdict-save t)
        )
    )
  )
)

;; 修正したい単語の上にカーソルをもっていき, C-M-return を押すことで候補を選択
(add-hook 'flyspell-mode-hook
          (lambda ()
            (define-key flyspell-mode-map (kbd "<C-M-return>") 'flyspell-correct-word-popup-el)
          )
)

;;; flyspell-mode を自動的に開始させたいファイルを指定 (お好みでアンコメントするなり, 変更するなり)
(add-to-list 'auto-mode-alist '("\\.txt" . flyspell-mode))
(add-to-list 'auto-mode-alist '("\\.tex" . flyspell-mode))
;; (add-to-list 'auto-mode-alist '("\\.properties" . flyspell-mode))
;; (add-to-list 'auto-mode-alist '("\\.dtd" . flyspell-mode))
;;; 要らないkey-bindingを無効化
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-;") nil)
))
;;;;;;;;;;;;;;;;;;;;;;;; flyspell



;;;;;;;;;;;;;;;;;;;;;;;; 英和・和英辞書
(require 'sdic)
(setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/share/dict/gene.sdic")))   ; 英和
(setq sdic-waei-dictionary-list '((sdicf-client "/usr/share/dict/jedict.sdic"))) ; 和英

;; tooltipで表示
(defun temp-cancel-read-only (function &optional jaspace-off)
  "eval temporarily cancel buffer-read-only
&optional t is turn of jaspace-mode"
  (let ((read-only-p nil)
        (jaspace-mode-p nil))
    (when (and jaspace-off jaspace-mode)
      (jaspace-mode)
      (setq jaspace-mode-p t))
    (when buffer-read-only
      (toggle-read-only)
      (setq read-only-p t))
    (eval function)
    (when read-only-p
      (toggle-read-only))
    (when jaspace-mode-p
      (jaspace-mode)
    )
  )
)

(defun my-sdic-describe-word-with-popup (word &optional search-function)
  "Display the meaning of word."
  (interactive
   (let ((f (if current-prefix-arg (sdic-select-search-function)))
         (w (sdic-read-from-minibuffer)))
     (list w f)))
  (let ((old-buf (current-buffer))
        (dict-data))
    (set-buffer (get-buffer-create sdic-buffer-name))
    (or (string= mode-name sdic-mode-name) (sdic-mode))
    (erase-buffer)
    (let ((case-fold-search t)
          (sdic-buffer-start-point (point-min)))
      (if (prog1 (funcall (or search-function
                              (if (string-match "\\cj" word)
                                  'sdic-search-waei-dictionary
                                'sdic-search-eiwa-dictionary))
                          word)
            (set-buffer-modified-p nil)
            (setq dict-data (buffer-string))
            (set-buffer old-buf))
          (temp-cancel-read-only
           '(popup-tip dict-data :scroll-bar t :truncate nil))
        (message "Can't find word, \"%s\"." word)
      )
    )
  )
)

(global-set-key "\C-cp" 'my-sdic-describe-word-with-popup)
;;;;;;;;;;;;;;;;;;;;;;;; 英和・和英辞書




(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ecb-options-version "2.32")
 '(exec-path (quote ("/opt/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((encoding . utf-8) (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby"))))
 '(session-use-package t nil (session))
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;; '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "apple" :family "Monaco"))))
)
