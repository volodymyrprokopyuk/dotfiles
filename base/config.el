;; -*- lexical-binding: t; -*-

;; Editor

(defun config-doom ()
  ;; Maximize window on startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; Skip confirmation on exit
  (setq confirm-kill-emacs nil)
  (after! calendar
    ;; Start calendar from Monday
    (setq calendar-week-start-day 1)))

(defun config-font ()
  (setq doom-font
    (font-spec :family "JetBrainsMono NF Light" :size 13.0 :weight 'light))
  (setq +ligatures-in-all-modes t))

(defun config-theme ()
  (setq doom-theme 'zenburn)
  (setq zenburn-override-colors-alist '(("zenburn-bg" . "#282523"))))

(defun config-current-line ()
  ;; Relative line numbers navigation with 5j, 5k
  (setq display-line-numbers-type 'relative)
  (global-hl-line-mode 1)
  (custom-set-faces!
    ;; Highlight current line
    `(hl-line :background "#24190E")
    ;; Highlight visual selection
    `(region :background "#801515")
    ;; Highlight line numbers
    `(line-number :background "#32312E" :foreground "#F5F5DC")
    `(line-number-current-line :foreground "#E2A665" :weight bold)))

(defun config-whitespace ()
  ;; Highlight line content exceeding the limit
  (global-whitespace-toggle-options t)
  (setq-default whitespace-line-column 80)
  (setq-default whitespace-style '(face tab-mark trailing lines-tail))
  ;; Open new line after exceeding the limit
  (setq-default fill-column 80)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  ;; Wrap long lines
  (global-visual-line-mode t))

(defun config-indent ()
  ;; Set default 2-space indentation
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  ;; Vim shift with >>, <<
  (setq evil-shift-width 2)
  ;; Set 2-space indentation
  (after! web-mode
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-attr-value-indent-offset 2)
    (setq css-indent-offset 2))
  (after! dockerfile-mode
    (setq dockerfile-indent-offset 2))
  (after! fish-mode
    (setq c-basic-offset 2)
    (setq sh-basic-offset 2)
    (setq fish-indent-offset 2))
  (after! go-mode
    (setq-hook! 'go-mode-hook indent-tabs-mode nil))
  (after! js-mode
    (setq js-indent-level 2))
  (after! lisp-mode
    (setq lisp-indent-offset 2))
  (after! lua-mode
    (setq lua-indent-level 2)))

(defun config-parentheses ()
  ;; Highlight matching parentheses
  (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook #'rainbow-delimiters-mode)
  ;; Highlight surrounding parentheses
  (require 'highlight-parentheses)
  (after! highlight-parentheses
    (set-face-attribute 'hl-paren-face nil :weight 'extra-bold)
    (setq highlight-parentheses-colors '("firebrick1" nil nil nil)))
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (add-hook 'text-mode-hook #'highlight-parentheses-mode))

(defun config-completion ()
  (after! corfu
    ;; Use M-j, M-k for corfu completion selection
    (define-key corfu-map (kbd "M-m") #'corfu-next)
    (define-key corfu-map (kbd "M-,") #'corfu-previous)
    (custom-set-faces!
      ;; Main popup window face
      `(corfu-default :background "#24190E" :foreground "#DCDCCC")
      ;; Currently selection face
      `(corfu-current :background "#480000" :foreground "#FEDC56")))
  (after! vertico
    ;; Use M-j, M-k for vertico completion selection
    (define-key vertico-map (kbd "M-m") #'vertico-next)
    (define-key vertico-map (kbd "M-,") #'vertico-previous)))

(defun config-evil ()
  ;; Set jk to escape
  (setq evil-escape-key-sequence "kj")
  (setq evil-escape-delay 0.2)
  ;; Set operation highlight duration
  (setq evil-goggles-duration 0.5))

(defun config-snippets ()
  (after! yasnippet
    (add-to-list 'yas-snippet-dirs "~/.dotfiles/base/snippets")
    (yas-load-directory "~/.dotfiles/base/snippets")
    ;; Wrap region in snippet
    (setq yas-wrap-around-region t)
    ;; Expand nested snippets
    (setq yas-triggers-in-field t)))

(defun config-spell ()
  ;; Enable jinx
  (add-hook 'doom-first-buffer-hook #'global-jinx-mode)
  ;; Set spell check languages
  (setq jinx-languages "en_US es_ES")
  ;; Set misspelled face
  (custom-set-faces!
    `(jinx-misspelled :underline (:style wave :color "red" :thickness 3)))
  ;; Set spell check key bindings
  (evil-define-key 'normal 'global (kbd "z =") #'jinx-correct)
  (evil-define-key 'normal 'global (kbd "] s") #'jinx-next)
  (evil-define-key 'normal 'global (kbd "[ s") #'jinx-previous))

;; Programming

(defun config-org ()
  (after! org
    ;; Set dark background color in source code blocs
    (custom-set-faces!
      `(org-block :background
        ,(doom-darken (face-attribute 'default :background) 0.03)))
    ;; Fold content at startup
    (setq org-startup-folded t)
    ;; Do not indent nested content
    (setq org-startup-indented nil)
    ;; Exclude angle brackets from parentheses highlight
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table)))

(defun config-lilypond ()
  (add-to-list 'load-path "~/.config/lilypond/share/emacs/site-lisp")
  (autoload 'lilypond-mode "lilypond-mode.el" nil t)
  (add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-mode))
  (add-hook 'lilypond-mode-hook #'(lambda () (display-line-numbers-mode)))
  (after! smartparens
    (sp-local-pair '(lilypond-mode) "(" nil
      :post-handlers '(:rem ("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair '(lilypond-mode) "[" nil
      :post-handlers '(:rem ("||\n[i]" "RET") ("| " "SPC"))))
  (font-lock-add-keywords 'lilypond-mode
    '(("\\<define\\>\\|\\<template\\>\\|\\<block\\>\\|\\<end\\>" . font-lock-builtin-face)
    ("\\<if\\>\\|\\<else\\>\\|\\<range\\>\\|\\<with\\>" . font-lock-builtin-face))))

(defun config-d2 ()
  (add-to-list 'auto-mode-alist '("\\.d2\\'" . js-mode))
  (add-hook 'js-mode-hook (lambda ()
    (when (and buffer-file-name (string-suffix-p ".d2" buffer-file-name))
      ;; Redefine comment symbol
      (setq-local comment-start "# ")
      (setq-local comment-end "")
      (setq-local comment-start-skip "#+\\s-*")
      ;; Highlight comments
      (set-syntax-table (make-syntax-table (syntax-table)))
      (modify-syntax-entry ?# "<" (syntax-table))
      (modify-syntax-entry ?\n ">" (syntax-table))
      (modify-syntax-entry ?/ "." (syntax-table))))))

;; Editor
(config-doom)
(config-font)
(config-theme)
(config-current-line)
(config-whitespace)
(config-indent)
(config-parentheses)
(config-completion)
(config-evil)
(config-snippets)
(config-spell)

;; Programming
(config-org)
(config-lilypond)
(config-d2)
