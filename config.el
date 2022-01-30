;; -*- lexical-binding: t; -*-

(defun config-doom ()
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (setq confirm-kill-emacs nil)
  (setq display-line-numbers-type t))

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

(defun config-completion ()
  (add-hook 'after-init-hook #'global-company-mode)
  (evil-define-key nil company-active-map (kbd "M-j") #'company-select-next)
  (evil-define-key nil company-active-map (kbd "M-k") #'company-select-previous))

(defun config-evil ()
  ;; Enable key combinations
  (require 'key-chord)
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
  ;; Exit insert/replace/visual mode on jk
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
  ;; Set operation highlight duration
  (setq evil-goggles-duration 0.5))

;; Editor
(config-doom)
(config-clipboard)
(config-font)
(config-theme)
(config-current-line)
(config-completion)
(config-evil)

;; Programming

;; Markup
