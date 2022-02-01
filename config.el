;; -*- lexical-binding: t; -*-

;; Editor

(defun config-doom ()
  ;; Maximize window on startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; Skip confirmation on exit
  (setq confirm-kill-emacs nil))

(defun config-clipboard ()
  (setq select-enable-clipboard t)
  (unless window-system
    (when (getenv "DISPLAY")
      (defun xsel-cut-function (text &optional push)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max)
                               "xsel" nil 0 nil "--clipboard" "--input")))
      (defun xsel-paste-function()
        (let ((xsel-output
               (shell-command-to-string "xsel --clipboard --output")))
          (unless (string= (car kill-ring) xsel-output) xsel-output)))
      (setq interprogram-cut-function 'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function))))

(defun config-font ()
  (setq doom-font (font-spec :family "JetBrains Mono" :size 18 :weight 'light)))

(defun config-theme ()
  (setq zenburn-override-colors-alist '(("zenburn-bg" . "#333333")))
  (load-theme 'zenburn t))

(defun config-current-line ()
  (global-hl-line-mode 1)
  ;; Highlight current line
  (set-face-attribute 'hl-line nil :foreground nil :background "#262626")
  ;; Highlight visual selection
  (set-face-attribute 'region nil :foreground nil :background "#801515"))

(defun config-whitespace ()
  ;; Enable global whitespace mode
  (require 'whitespace)
  (global-whitespace-mode t)
  (global-whitespace-toggle-options t)
  ;; Highlight line content exceeding the limit
  (setq-default whitespace-line-column 80)
  (setq-default whitespace-style '(face tab-mark trailing lines-tail))
  ;; Open new line after exceeding the limit
  (setq-default fill-column 80)
  (add-hook 'text-mode-hook #'auto-fill-mode))

(defun config-completion ()
  ;; Enable global company mode
  (add-hook 'after-init-hook #'global-company-mode)
  ;; Use M-j, M-k for company completion selection
  (evil-define-key nil company-active-map (kbd "M-j") #'company-select-next)
  (evil-define-key nil company-active-map (kbd "M-k") #'company-select-previous)
  ;; Use M-j, M-k for vertico completion selection
  (evil-define-key nil vertico-map (kbd "M-j") #'vertico-next)
  (evil-define-key nil vertico-map (kbd "M-k") #'vertico-previous))

(defun config-evil ()
  ;; Enable key combinations
  (require 'key-chord)
  (key-chord-mode 1)
  ;; Set key chord detection delay
  (setq key-chord-two-keys-delay 0.5)
  ;; Exit insert/replace/visual mode on jk
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
  ;; Set operation highlight duration
  (setq evil-goggles-duration 0.5))

;; Programming

(defun config-elisp ()
  ;; Treat - as part of the word on *, #, w, b, e
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))))

(defun config-zsh ()
  ;; Treat _ as part of the word on *, #, w, b, e
  (add-hook 'sh-mode-hook
            #'(lambda () (modify-syntax-entry ?_ "w" sh-mode-syntax-table))))

(defun config-nim ()
  ;; Treat _ as part of the word on *, #, w, b, e
  (add-hook 'nim-mode-hook
            #'(lambda () (modify-syntax-entry ?_ "w" nim-mode-syntax-table))))

;; Editor
(config-doom)
(config-clipboard)
(config-font)
(config-theme)
(config-current-line)
(config-whitespace)
(config-completion)
(config-evil)

;; Programming
(config-elisp)
(config-zsh)
(config-nim)
