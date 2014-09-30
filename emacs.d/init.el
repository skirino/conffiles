
;;;;;;;;;;;;;;;;;;;;;;;; emacs lispのpathを通す
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(push "/usr/local/share/emacs/site-lisp" load-path)
(require 'cl)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; 環境変数
(let* ((paths-with-newline (shell-command-to-string "source ~/.zshrc; echo $PATH"))
       (paths (replace-regexp-in-string "[\r?\n]+" "" paths-with-newline)))
  (setenv "PATH" paths)
  (setq exec-path (append (split-string paths ":") exec-path)))

;; proxy
(let ((proxy-line (shell-command-to-string "/bin/grep 'http_proxy' /etc/environment")))
  (if (string-match "\"\\(.*\\)\"" proxy-line)
      (setenv "http_proxy" (match-string 1 proxy-line))
    (setenv "http_proxy" nil)))
(let ((proxy-line (shell-command-to-string "/bin/grep 'https_proxy' /etc/environment")))
  (if (string-match "\"\\(.*\\)\"" proxy-line)
      (setenv "https_proxy" (match-string 1 proxy-line))
    (setenv "https_proxy" nil)))
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; packages
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(require 'async-bytecomp)

(require 'save-packages)
(add-hook 'kill-emacs-hook 'my-save-packages)

;; taken from newer save-packages.el
(defun my-save-packages (&optional filename)
  "Save list of currently installed packages.
The list is written to FILENAME, or `save-packages-file' by default."
  (interactive (let ((insert-default-directory nil))
                 (list (read-file-name "Save package list to file: " nil nil nil save-packages-file))))
  (with-temp-buffer
    (pp (sort (copy-sequence (mapcar 'car package-alist)) 'string<) (current-buffer))
    (write-region (point-min) (point-max) (or filename save-packages-file))))
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; Macのキーボード
(when (eq system-type 'darwin)
  ;; swap command and option keys
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; key-chord
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; pomodoro
(require 'tomatinho)
(global-set-key (kbd "<f12>") 'tomatinho)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; ファイル関連
;; ffap
(ffap-bindings)

;; disable backup
(setq backup-inhibited t)
;; disable Emacs default auto-save
(setq auto-save-default nil)

;; auto-save
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.0) ; アイドル1秒で保存
(auto-save-buffers-enhanced t)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; auto revert
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
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; dired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; diredでフォルダを開く時, 新しいバッファを作成しない
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  (define-key dired-mode-map [return]  'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function (lambda nil (interactive) (dired-single-buffer "..")))))
(if (boundp 'dired-mode-map)
    ;; dired is already loaded; add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; モードライン
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-format "%Y年%m月%d日 %H:%M")
(setq display-time-load-average nil)
(display-time)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; バッファ&ミニバッファ
;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.2)

;; バッファ切り替えを強化
(iswitchb-mode t)
(setq iswitchb-regexp nil)           ;; 部分文字列の代わりに正規表現を使う場合は t に設定する
(setq iswitchb-prompt-newbuffer nil) ;; 新しいバッファを作成するときにいちいち聞かないように

;; ファイル名がかぶったときのバッファ名を適切に設定
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; ミニバッファの補完強化
(require 'ido)
(ido-mode t)

;; バッファ切り替え (C-, C-.)
(defvar my-ignore-blst             ; 移動の際に無視するバッファのリスト
  '("*Help*" "*Compile-Log*" "*Mew completions*" "*Completions*"
    "*Shell Command Output*" "*Apropos*" "*Buffer List*"
    "*anything*" "*anything minibuffer-history*" "*anything complete*" "*my-anything*"))
(defvar my-visible-blst       nil) ; 移動開始時の buffer list を保存
(defvar my-bslen              15 ) ; buffer list 中の buffer name の最大長
(defvar my-blist-display-time 2  ) ; buffer list の表示時間
(defface my-cbface                 ; buffer list 中で current buffer を示す face
  '((t (:foreground "wheat" :underline t))) nil)

(defun my-visible-buffers (blst)
  (unless (eq blst nil)
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

(global-set-key (kbd "C-,") (lambda () (interactive) (my-operate-buffer nil)))
(global-set-key (kbd "C-.") (lambda () (interactive) (my-operate-buffer t)))
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
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
)

;; anzuで検索のマッチ数をモードラインに表示
(require 'anzu)
(global-anzu-mode t)

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
         (search-pattern (read-string (format "Search for (%s): " word) nil nil word)))
    (if (and (buffer-file-name) (string-prefix-p (magit-rev-parse "--show-toplevel") (buffer-file-name)))
        (my-git-grep search-pattern ignore-case-option)
      (my-rgrep search-pattern ignore-case-option))
    (my-grep-place-buffers)))

(defun my-git-grep (search-pattern ignore-case-option)
  (let* ((repo-dir (concat (magit-rev-parse "--show-toplevel") "/"))
         (rel-path (substring (file-name-directory (buffer-file-name)) (length repo-dir)))
         (dir-arg  (replace-regexp-in-string "[^/]+" ".." rel-path)))
    (grep (format "git --no-pager grep %s -nHe '%s' %s" ignore-case-option search-pattern dir-arg))))

(defun my-rgrep (search-pattern ignore-case-option)
  (grep (format "grep %s -rnHe '%s' ." ignore-case-option search-pattern)))

(defun my-grep-place-buffers ()
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer "*grep*")
  (other-window 1))
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; foreign-regexp
(require 'foreign-regexp)
(global-set-key (kbd "C-x C-r") 'foreign-regexp/query-replace)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; ハイライト表示
(require 'idle-highlight-mode)
(add-hook 'find-file-hook '(lambda () (idle-highlight-mode t)))
;;;;;;;;;;;;;;;;;;;;;;;; ハイライト表示



;;;;;;;;;;;;;;;;;;;;;;;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-)") 'mc/mark-next-like-this)
(global-set-key (kbd "C-(") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-(") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-)") 'mc/mark-all-like-this)
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

;; 自動インデント、RetやC-jも自動インデントになる
(global-set-key (kbd "C-m") 'reindent-then-newline-and-indent)

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
(when window-system
  (setq x-select-enable-clipboard t)
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
;; "C-=" (C-S--), "C-|"で画面分割
(global-set-key (kbd "C-=") 'split-window-vertically)
(global-set-key (kbd "C-|") 'split-window-horizontally)

;; dsで画面分割
(defun my-show-buffer-in-two-window ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally))
(key-chord-define-global "ds" 'my-show-buffer-in-two-window)

;; "C-S-hjkl"でウィンドウ移動
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)

;; follow-mode
(require 'follow)
(defun my-toggle-follow-mode ()
  (interactive)
  (cond ((eq follow-mode t) ; On => follow-modeをオフにして2分割に戻る
         (follow-mode nil)
         (delete-other-windows)
         (split-window-horizontally)
         (balance-windows)
         )
        (t ; Off => 3分割してfollow-mode
         (delete-other-windows)
         (split-window-horizontally)
         (split-window-horizontally)
         (balance-windows)
         (follow-mode)
         ))
)
(key-chord-define-global "fw" 'my-toggle-follow-mode)

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; CUA
(cua-mode t)
(setq cua-enable-cua-keys nil)                         ;; C-cやC-vの乗っ取りを阻止
(define-key cua-global-keymap (kbd "C-S-SPC") 'ignore) ;; C-S-SPCを空ける(日本語モード => 戻す)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; view-mode
(require 'view)
(key-chord-define-global "vm" 'view-mode)

(define-key view-mode-map (kbd "/") 'isearch-forward)
(define-key view-mode-map (kbd "G") 'end-of-buffer)
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "j") 'next-line)
(define-key view-mode-map (kbd "k") 'previous-line)
(define-key view-mode-map (kbd "l") 'forward-char)

(require 'viewer)
(viewer-stay-in-setup) ;; 書き込み不能なファイルではviewer-modeから抜けない

;; モードラインに色をつける
(setq viewer-modeline-color-unwritable "tomato")
(setq viewer-modeline-color-view       "orange")
(viewer-change-modeline-color-setup)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; picture-mode
(defun toggle-picture-mode ()
  "Returns the major mode associated with a buffer."
  (interactive)
  (if (string= "picture-mode" major-mode)
      (picture-mode-exit)
    (picture-mode)
  )
)
(key-chord-define-global "pc" 'toggle-picture-mode)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; align
(global-set-key (kbd "C-c a"  ) 'align       )
(global-set-key (kbd "C-c C-a") 'align-regexp)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; org-mode
(require 'org)

;; 開いたときには一旦全展開
(setq org-startup-folded nil)

;; org-captureでメモを取る
(setq org-directory "~/docs/")
(setq org-default-notes-file (concat org-directory "memo.org"))
(require 'org-capture)
(setq org-capture-templates
      '(("m" "Inbox" entry (file+headline nil "Inbox")
         "** %?\n %U\n %a\n")
        ))
(defun my-org-capture-memo ()
  (interactive)
  (org-capture '(0) "m")
)
(global-set-key (kbd "M-m") 'my-org-capture-memo)
(define-key org-capture-mode-map (kbd "C-x C-s") 'org-capture-finalize)
(define-key org-capture-mode-map (kbd "C-x C-c") 'org-capture-kill)

(defun my-org-visit-file (filepath)
  (let ((osf-orig org-startup-folded))
    (setq org-startup-folded nil)
    (find-file filepath)
    (setq org-startup-folded osf-orig)))
(defun my-org-visit-memo-file ()
  (interactive)
  (my-org-visit-file "~/docs/memo.org"))
(global-set-key (kbd "C-c m") 'my-org-visit-memo-file)

;; 要らないkey-bindingを無効化
(define-key org-mode-map [C-S-left]  nil)
(define-key org-mode-map [C-S-right] nil)
(define-key org-mode-map (kbd "C-,") nil)
(define-key org-mode-map [S-up]    nil)
(define-key org-mode-map [S-down]  nil)
(define-key org-mode-map [S-left]  nil)
(define-key org-mode-map [S-right] nil)
(define-key org-mode-map (kbd "C-c C-x C-c") nil)

;; ox-html5presentation
(require 'ox-html5presentation)
(define-key org-mode-map (kbd "<f5>") 'org-export-as-html5presentation)
(define-key org-mode-map (kbd "<f6>") 'org-export-as-html5presentation-and-open)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; Git
;; git-gutter+ & git-gutter-fringe+
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)
(global-set-key (kbd "C-c n"  ) 'git-gutter+-next-hunk)
(global-set-key (kbd "C-c C-n") 'git-gutter+-next-hunk)
(global-set-key (kbd "C-c p"  ) 'git-gutter+-previous-hunk)
(global-set-key (kbd "C-c C-p") 'git-gutter+-previous-hunk)
(global-set-key (kbd "C-c s"  ) 'git-gutter+-stage-hunks)
(global-set-key (kbd "C-c C-s") 'git-gutter+-stage-hunks)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; helm
(require 'helm-mode)
(require 'helm-git-grep)
(helm-mode 1)
(setq helm-input-idle-delay 0.1)
(setq helm-truncate-lines t)
(setq helm-buffer-max-length 50)
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil)) ;; Don't use helm command for C-x C-f

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(setq helm-delete-minibuffer-contents-from-point t)
(define-key helm-map (kbd "C-c C-k") 'helm-buffer-run-kill-persistent)

;; helm commands
(global-set-key (kbd "M-x"  ) 'helm-M-x)
(global-set-key (kbd "C-;"  ) 'helm-mini)
(global-set-key (kbd "C-+"  ) 'helm-resume)
(global-set-key (kbd "C-c g") 'helm-git-grep)

(setq helm-locate-command "locate %s -A %s") ;; Enable AND search in helm-locate (instead regexp cannot be used)
(setq helm-mini-default-sources '(helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-locate))

;; helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "C-S-s") 'helm-swoop)
(define-key isearch-mode-map (kbd "C-S-s") 'helm-swoop-from-isearch) ;; When doing isearch, hand the word over to helm-swoop

;; don't show error message when keys that invoke helm commands multiple times
(define-key helm-map (kbd "C-;"  ) 'ignore)
(define-key helm-map (kbd "C-+"  ) 'ignore)
(define-key helm-map (kbd "M-x"  ) 'ignore)
(define-key helm-map (kbd "C-c g") 'ignore)
(define-key helm-map (kbd "C-S-s") 'ignore)
;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; w3m
(require 'w3m)
(load-library "w3m-filter")
(setq w3m-use-filter t)
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-default-display-inline-images t)
(setq w3m-session-load-crashed-sessions nil)
(setq browse-url-browser-function 'w3m-browse-url)
(global-set-key (kbd "C-x m") 'browse-url-at-point)

(define-key w3m-mode-map (kbd "n"  ) 'w3m-next-anchor)
(define-key w3m-mode-map (kbd "M-n") 'w3m-next-anchor)
(define-key w3m-mode-map (kbd "p"  ) 'w3m-previous-anchor)
(define-key w3m-mode-map (kbd "P"  ) 'w3m-previous-anchor)
(define-key w3m-mode-map (kbd "M-p") 'w3m-previous-anchor)
(define-key w3m-mode-map (kbd "b"  ) 'w3m-view-previous-page)

(defun w3m-filter-alc-alt (url)
  "英辞郎のヘッダ部分を取り除く"
  (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▼ 検索補助 ▼ -->"))
(add-to-list 'w3m-filter-rules '("\\`http://eow\.alc\.co\.jp/[^/]+/UTF-8" w3m-filter-alc-alt))

(defun w3m-search-eijiro (query)
  "w3mで英辞郎 on the Web検索"
  (interactive (list
                (let ((w (or (thing-at-point 'word) "") ))
                  (read-string (format "[ALC] Search for (%s): " w) nil nil w))))
  (w3m (concat "http://eow.alc.co.jp/" (w3m-url-encode-string query 'utf-8) "/UTF-8")))
(global-set-key (kbd "C-c e"  ) 'w3m-search-eijiro)
(global-set-key (kbd "C-c C-e") 'w3m-search-eijiro)
;;;;;;;;;;;;;;;;;;;;;;;; w3m


;;;;;;;;;;;;;;;;;;;;;;;; HTTP request
(require 'request)
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; 日本語入力
;; 日本語 <=> 英数の切り替え(ibus等々にやらせる)
(global-set-key (kbd "S-C-SPC"          ) 'ignore)
(global-set-key (kbd "<zenkaku-hankaku>") 'ignore)
;;;;;;;;;;;;;;;;;;;;;;;; mozc


;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更
;; メニューバー、ツールバー、スクロールバーを消す, Emacs23以降
(when (>= emacs-major-version 23)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (horizontal-scroll-bar-mode 0)
  (menu-bar-mode 0)
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


;; インデントの強調表示
(require 'highlight-indentation)
(add-hook 'find-file-hook 'highlight-indentation-mode)


;; 行の折り返し
(key-chord-define-global "kl" 'toggle-truncate-lines)


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
  (unless (server-running-p) (server-start))
  ;; "C-x C-c"でemacsclientによる編集を終了
  (global-set-key (kbd "C-x C-c") 'server-edit)
  (global-set-key (kbd "C-c C-x C-c") 'save-buffers-kill-emacs)
  ;; "M-x exit"でemacsを終了
  (defalias 'exit 'save-buffers-kill-emacs)

  ;; elscreen
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

  ;; GUIでの色付け
  (add-to-list 'default-frame-alist '(background-color . "black"))
  (add-to-list 'default-frame-alist '(foreground-color . "white"))
  ;; Transparency: needs xcompmgr
  (set-frame-parameter (selected-frame) 'alpha '(85 50))
  (add-to-list 'default-frame-alist '(alpha 85 50))
)

(defun my-start-cui-emacs ()
  ;; ターミナルでマウスを有効にする
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up   5)))

  ;; CUIではdefault-frame-alistを使わないほうが良さげ
  (set-background-color "black")
)


;;;;;;; Linux
(when (eq system-type 'gnu/linux)
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
                                   (progn (setq old-fullscreen current-value) 'fullboth)))
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
          (toggle-fullscreen)
          (split-window-horizontally)
        )
        (run-with-idle-timer 0.5 nil 'my-maximize-and-split)

        ;; フォント設定
        (let (my-font-height my-font my-font-ja my-font-size my-fontset)
          (setq my-font-height 115)
          (setq my-font "DejaVu Sans Mono")
          (setq my-font-ja "IPAGothic")
          (setq face-font-rescale-alist '(("IPAGothic" . 1.20)))
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
    (progn ;; CUI
      (my-start-cui-emacs)
      (add-to-list 'default-frame-alist '(foreground-color . "brightwhite"))
      (add-to-list 'default-frame-alist '(cursor-color     . "brightwhite"))
    )
  )
)



;;;;;;; Mac
(when (eq system-type 'darwin)
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


;;;;;;; Windows
(defun set-font-on-windows ()
  (set-face-attribute 'default nil :family "ＭＳ ゴシック")
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("ＭＳ ゴシック" . "jisx0208-sjis"))
)

(when (eq system-type 'windows-nt) ;; GNU Emacs on Windows (GUI)
  (set-font-on-windows)
  (add-to-list 'default-frame-alist '(background-color . "black"))

  ;; Windows用の最大化
  (defun my-maximize ()
    (interactive)
    (w32-send-sys-command #xf030))
  (run-with-idle-timer 0.2 nil 'my-maximize)

  ;; GUI用の初期化(server-startなど)
  (when (eq window-system 'w32) (my-start-gui-emacs))
)

;; On Cygwin command-line (CUI)
(when (eq system-type 'cygwin)
  (set-font-on-windows)
  (my-start-cui-emacs)
)
;;;;;;;;;;;;;;;;;;;;;;;; 見た目、環境依存



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


;; vala-mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist          '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist          '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))


;; python
(require 'python)
;; returnでカーソルのいた行もインデントするのはpythonだとダメ
(defun my-newline-and-indent ()
  (interactive)
  (newline)
  (indent-according-to-mode))
(define-key python-mode-map (kbd "RET") 'my-newline-and-indent)


;; ruby
(require 'starter-kit-ruby)
(require 'ruby-mode)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)))

;; inf-ruby関連のキーバインドを無効化
(remove-hook 'ruby-mode-hook 'inf-ruby-keys)
(remove-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(remove-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;; アクセス修飾子のインデントを下げる (taken from ruby-mode.el)
(push "public"    ruby-block-mid-keywords)
(push "protected" ruby-block-mid-keywords)
(push "private"   ruby-block-mid-keywords)
(setq ruby-block-mid-re (regexp-opt ruby-block-mid-keywords))
(setq ruby-negative
      (concat "^[ \t]*\\(\\(" ruby-block-mid-re "\\)\\>\\|"
              ruby-block-end-re "\\|}\\|\\]\\)"))

;; カッコの内側の要素のインデントを、カッコ位置ではなく通常インデントにする
(setq ruby-deep-indent-paren-style nil)

;; rinari
(require 'rinari)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)
(define-key rinari-minor-mode-map (kbd "C-c ; w") 'rinari-web-server-restart)
(setq rinari-rgrep-file-endings "*.rb *.erb *.rake *.haml") ;; rinari-rgrepのターゲットファイルを絞る
(setq ruby-compilation-executable "testdrb") ;; to run tests using spork, overwrite the name of executable

;; custom functions
(defvar my-rinari-spork-branch nil)
(defvar my-rinari-autotest-after-save nil)
(defvar my-rinari-autotest-marker nil)

(defun my-rinari-start-spork ()
  (interactive)
  (setq my-rinari-spork-branch (magit-get-current-branch))
  (let ((command (concat "cd " (rinari-root) "; spork testunit &")))
    (shell-command command)))
(define-key rinari-minor-mode-map (kbd "C-c ; s") 'my-rinari-start-spork)

(defun my-rinari-kill-spork ()
  (interactive)
  (let ((command "kill `ps x | /bin/grep 'ruby.*spork' | /bin/grep -v grep | sed -e 's/ .*$//'`"))
    (shell-command command)))
(define-key rinari-minor-mode-map (kbd "C-c ; S") 'my-rinari-kill-spork)

(defun my-rinari-is-spork-running? ()
  (let ((grep-result (shell-command-to-string "ps -ef | grep 'ruby.*spork' | grep -v grep")))
    (< 0 (length grep-result))))

(defun my-rinari-test-on-appropriate-position ()
  (save-excursion
    (cond (my-rinari-autotest-marker
           (with-current-buffer (marker-buffer my-rinari-autotest-marker)
             (goto-char my-rinari-autotest-marker)
             (rinari-test)))
          (t ;; without marker
           (rinari-test))
          )))

(defun my-rinari-test-with-preferred-window-configuration ()
  (let* ((is-test-file (string-match "_\\(test\\|spec\\)\\.rb" (buffer-file-name)))
         (orig-buffer  (current-buffer)))
    (when (open-related-file-open)
      (unless is-test-file (other-window 1))
      (my-rinari-test-on-appropriate-position)
      (select-window (get-buffer-window orig-buffer)))))

(defun my-rinari-test-using-spork ()
  "Start spork process if it is not running.
Then run tests in a preferred window configuration."
  (interactive)
  (let* ((same-branch          (equal my-rinari-spork-branch (magit-get-current-branch)))
         (need-to-launch-spork (or (not same-branch) (not (my-rinari-is-spork-running?)))))
    (cond (need-to-launch-spork
           (my-rinari-start-spork))
          (t
           (my-rinari-test-with-preferred-window-configuration))
          )))
(define-key rinari-minor-mode-map (kbd "C-c C-t") 'my-rinari-test-using-spork)

(defun my-rinari-autotest-toggle-run-after-save ()
  (interactive)
  (setq my-rinari-autotest-after-save (not my-rinari-autotest-after-save))
  (message "my-rinari-autotest is enabled: %s" my-rinari-autotest-after-save)
  (when (and my-rinari-autotest-after-save (not (my-rinari-is-spork-running?)))
    (my-rinari-start-spork)))
(define-key rinari-minor-mode-map (kbd "C-c ; T") 'my-rinari-autotest-toggle-run-after-save)

(defun my-rinari-test-with-check ()
  (interactive)
  (when my-rinari-autotest-after-save
    (my-rinari-test-using-spork)))
(add-hook 'rinari-minor-mode-hook
          (lambda () (add-hook 'after-save-hook 'my-rinari-test-with-check nil t)))

(defun my-rinari-autotest-set-marker ()
  (interactive)
  (setq my-rinari-autotest-marker (point-marker))
  (message "rinari autotest marker is set"))
(defun my-rinari-autotest-clear-marker ()
  (interactive)
  (setq my-rinari-autotest-marker nil)
  (message "rinari autotest marker is removed"))
(define-key rinari-minor-mode-map (kbd "C-c ; m") 'my-rinari-autotest-set-marker)
(define-key rinari-minor-mode-map (kbd "C-c ; M") 'my-rinari-autotest-clear-marker)

(defun my-rinari-rgrep ()
  (interactive)
  (rinari-rgrep)
  (my-grep-place-buffers))
(define-key rinari-minor-mode-map (kbd "C-c ; g") 'my-rinari-rgrep)


;; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; CoffeeScript
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile"   . coffee-mode))
;; enable compile-on-save minor mode
(add-hook 'coffee-mode-hook 'coffee-cos-mode)

(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)


;; Jade
(require 'jade-mode)
;; use jade-mode for *.dt
(add-to-list 'auto-mode-alist '("\\.dt$" . jade-mode))


;; Clojure
(require 'clojure-mode)
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(require 'nrepl)


;; Scala
(require 'scala-mode2)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; Run apk from within ensime
(defun my-ensime-run-apk-in-emulator ()
  (interactive)
  (ensime-sbt-switch)
  (ensime-sbt-action "android:start-emulator"))
(define-key ensime-mode-map (kbd "C-c C-b a") 'my-ensime-run-apk-in-emulator)

(defun make-play-doc-url (type &optional member)
  (ensime-make-java-doc-url-helper
   "file:///home/rajish/bin/play2/documentation/api/scala/" type member))
(add-to-list 'ensime-doc-lookup-map '("^play\\.api\\." . make-play-doc-url))


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


;; Erlang and Elixir
(require 'erlang-start)
(require 'elixir-mode)
(require 'flymake-elixir)
(add-hook 'elixir-mode-hook 'flymake-elixir-load)


;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$"       . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(setq markdown-command "redcarpet --parse-tables")
;; save時にHTML変換したファイルを保存し、w3mで表示
(defun my-markdown-export-and-view ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (markdown-export-and-view)
    (w3m-redisplay-this-page)
    (split-window-horizontally)
    (switch-to-buffer orig-buffer)))
(define-key gfm-mode-map (kbd "<f6>") 'my-markdown-export-and-view)


;; GNU global
(require 'gtags)
(define-key gtags-mode-map (kbd "M-t") 'gtags-find-tag)  ;関数の定義元へ
(define-key gtags-mode-map (kbd "M-r") 'gtags-find-rtag) ;関数の参照先へ
(define-key gtags-mode-map (kbd "M-t") 'gtags-pop-stack) ;前のバッファに戻る
(add-hook 'c-mode-common-hook 'gtags-mode)
(add-hook 'c++-mode-hook      'gtags-mode)
;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援



;;;;;;;;;;;;;;;;;;;;;;;; flymake & gccsense
(require 'flymake)
(require 'flycheck)
(setq flymake-check-start-time 5)
(key-chord-define-global "fm" 'flymake-mode)

;; GUIのダイアログを抑制
(setq flymake-gui-warnings-enabled nil)

;; gccsenseおよびgccsense-flymake
(require 'gccsense)
(add-hook 'c-mode-common-hook
          (lambda ()
            (flymake-mode)
            (local-set-key (kbd "C-.") 'ac-complete-gccsense)   ;gccsense補完
            (local-set-key (kbd "C-c .") 'ac-complete-gccsense) ;gccsense補完
            (gccsense-flymake-setup))
)

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
;;;;;;;;;;;;;;;;;;;;;;;; flymake & gccsense



;;;;;;;;;;;;;;;;;;;;;;;; flyspell
(when (eq system-type 'gnu/linux)

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
                   (flyspell-mode t)
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
    (when (consp word)
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
        (or ispell-filter (setq ispell-filter '(*)))
        (when (consp ispell-filter)
          (setq poss (ispell-parse-output (car ispell-filter))))
        (cond ((or (eq poss t) (stringp poss))
               t) ;; don't correct word
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
            (define-key flyspell-mode-map (kbd "<C-M-return>") 'flyspell-correct-word-popup-el)))

;;; flyspell-mode を自動的に開始させたいファイルを指定 (お好みでアンコメントするなり, 変更するなり)
(add-to-list 'auto-mode-alist '("\\.txt" . flyspell-mode))
(add-to-list 'auto-mode-alist '("\\.tex" . flyspell-mode))
;;; 要らないkey-bindingを無効化
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-;") nil)

)
;;;;;;;;;;;;;;;;;;;;;;;; flyspell



;;;;;;;;;;;;;;;;;;;;;;;; 英和・和英辞書
(require 'sdic)
(setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/local/share/dict/gene.sdic")))   ; 英和
(setq sdic-waei-dictionary-list '((sdicf-client "/usr/local/share/dict/jedict.sdic"))) ; 和英

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

(global-set-key (kbd "C-x p") 'my-sdic-describe-word-with-popup)
;;;;;;;;;;;;;;;;;;;;;;;; 英和・和英辞書


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
 '(column-number-mode t)
 '(fill-column 400)
 '(foreign-regexp/regexp-type (quote ruby))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(reb-re-syntax (quote foreign-regexp))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(text-mode-hook (quote ((lambda nil (flyspell-mode t) (flyspell-buffer)) turn-on-flyspell text-mode-hook-identify))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
