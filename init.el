;;ロードパスの設定
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/elisp/"))
       load-path))

;; リポジトリ登録
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; 初期化
(package-initialize)


;; encoding

(set-language-environment       "Japanese")
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)

;;zencording
;(add-tolist 'load-path "~/.emacs.d/elsp/")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
(add-hook 'text-mode-hook 'zencoding-mode)
(add-hook 'css-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-c C-m") 'zencoding-expand-line)
(define-key zencoding-preview-keymap (kbd "C-c C-m") 'zencoding-preview-accept)

; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止

;キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ;削除
(define-key global-map "\C-z" 'undo)                 ;undo
(define-key global-map "\C-ci" 'indent-region)       ;インデント
(define-key global-map "\C-u" 'forward-word)         ;単語forward
(define-key global-map "\C-i" 'backward-word)        ;単語backword
(define-key global-map "\C-l" 'kill-word)            ;kill word

;file名の補完で大文字、小文字区別しない
(setq completion-ignore-case t)

;同名ファイルのバッファ名の識別文字列を変更する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;自動インデント
(global-set-key "\C-m" 'newline-and-indent)

;行番号の表示
(global-linum-mode t)
;一行が80文字以上になったら、自動改行する
(setq fill-column 80)
(setq-default auto-fill-mode t)

;タブを使わない
(setq indent-tabs-mode nil)

;バックアップファイルを作らない
(setq make-backup-files nil)

;対応する括弧を光らせる
(show-paren-mode 1)

;;文字色
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t) 
(setq fast-lock nil)
(setq lazy-lock nil)
(setq jit-lock t)
;;コメントアウト文字色
;(set-face-foreground 'font-lock-comment-face "snow4")
;(set-face-foreground 'font-lock-comment-delimiter-face "snow4")


;;英字フォントの設定
(set-face-attribute 'default nil
            :family "Menlo" ;; font
            :height 200)    ;; font size

;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/") ;Emacs Lispをインストールするディレクトリの指定
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup) ;install-elisp.elとコマンド名を同期

;; anything
(global-set-key (kbd "C-x b") 'anything)

;;anythingでemacsコマンドを追加
(require 'anything-config)
(add-to-list 'anything-sources 'anything-c-source-emacs-commands)

;; multiple-cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
(require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; php-mode
(require 'php-mode)
 
;(setq php-mode-force-pear t) ;PEAR規約のインデント設定にする
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode)) ;*.phpのファイルのときにphp-modeを自動起動する

(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'mc/mark-next-like-this)
            (local-set-key (kbd "C-<") 'mc/mark-previous-like-this)
            (defun ywb-php-lineup-arglist-intro (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (+ (current-column) c-basic-offset))))
            (defun ywb-php-lineup-arglist-close (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (current-column))))
            (c-set-style "stroustrup")    ; インデントは4文字分基本スタイル
            (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro) ; 配列のインデント関係
            (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close) ; 配列のインデント関係
            (c-set-offset 'arglist-cont-nonempty' 4) ; 配列のインデント関係
            (c-set-offset 'case-label' 4) ; case はインデントする
            (make-local-variable 'tab-width)
            (make-local-variable 'indent-tabs-mode)
            (setq tab-width 4)
            (setq indent-tabs-mode nil))) ;indentにtabは使わない

;; smarty-mode
(autoload 'smarty-mode "smarty-mode")
(setq auto-mode-alist
      (cons '("\\.tpl\\'" . smarty-mode) auto-mode-alist))
(add-hook 'smarty-mode-hook
           #'(lambda ()
              (make-local-variable 'tab-width)
              (make-local-variable 'indent-tabs-mode)
              (setq indent-tabs-mode nill)
              (setq tab-width 4)))

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
;; ハイライト設定
(setq ruby-block-highlight-toggle t)


;; ruby indent-region


(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)
(setq ruby-deep-indent-paren-style nil)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; js-mode2

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))

;;set eshell aliases
(setq eshell-command-aliases-list
      (append
       (list
	(list "ll" "ls -alF")
	(list "dev" "rails s -p3000 -e development")
	(list "swipl" "/opt/local/bin/swipl"))
       eshell-command-aliases-list))

;; Magit

;;(require 'magit)
;;(add-hook 'magit-mode-hook 'magit-setup-diff)


;;expand region
(add-to-list 'load-path "~/.emacs.d/expand-region.el")
(require 'expand-region)
(global-set-key (kbd "C-^") 'er/expand-region) 
(global-set-key (kbd "C-M-^") 'er/contract-region)

;; transient-mark-modeが nilでは動作しませんので注意
(transient-mark-mode t)

;; coffeescript
(custom-set-variables
  '(coffee-tab-width 2)
   '(coffee-args-compile '("-c" "--bare")))

(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))



;; quickrun
(require 'quickrun)

;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

; ag
(setq default-process-coding-system 'utf-8-unix)  ; ag 検索結果のエンコード指定
(require 'ag)
(setq ag-highlight-search t)  ; 検索キーワードをハイライト
(setq ag-reuse-buffers t)  

;PHP modeの際に改行の表示をさせる
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." nil t)
(add-hook 'php-mode-hook
   '(lambda ()
     (whitespace-newline-mode)))
(setq whitespace-display-mappings
   '((newline-mark ?\n    [?\x2193 ?\n] [?$ ?\n]); end-of-line
))
