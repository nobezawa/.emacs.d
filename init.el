;;ロードパスの設定
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/elisp/"))
       load-path))


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

;キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ;削除
(define-key global-map "\C-z" 'undo)                 ;undo
(define-key global-map "\C-ci" 'indent-region)       ;インデント

;行番号の表示
(global-linum-mode t)
;一行が80文字以上になったら、自動改行する
(setq fill-column 80)
(setq-default auto-fill-mode t)


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

;; php-mode
(require 'php-mode)
 
;(setq php-mode-force-pear t) ;PEAR規約のインデント設定にする
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode)) ;*.phpのファイルのときにphp-modeを自動起動する


;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
;; ハイライト設定
(setq ruby-block-highlight-toggle t)


;; ruby indent-region


(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)

(add-hook 'magit-mode-hook 'magit-setup-diff)
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



;; Magit

(require 'magit)
