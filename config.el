;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun config-doom ()
  ;; Maximize Emacs window on startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; Disable exit confirmation
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

(defun config-r ()
  ;; Treat _ as part of the word on *, #, w, b, e
  (add-hook 'ess-r-mode-hook
            #'(lambda () (modify-syntax-entry ?_ "w" ess-r-mode-syntax-table)))
  ;; R indentation
  (add-hook 'ess-r-mode-hook #'(lambda () (ess-set-style 'RStudio)))
  ;; Start comment with single #
  (add-hook 'ess-mode-hook #'(lambda () (setq comment-add 0)))
  ;; Enable company-mode
  (setq ess-use-company nil)
  (add-hook 'ess-mode-hook
            #'(lambda ()
                (make-variable-buffer-local 'company-backends)
                (add-to-list 'company-backends
                             '(company-R-args company-R-objects company-dabbrev-code :separate)))))

;; Editor
(config-doom)
(config-clipboard)
(config-font)
(config-theme)
(config-current-line)
(config-completion)
(config-evil)

;; Programming languages
(config-r)

;; Markup languages
