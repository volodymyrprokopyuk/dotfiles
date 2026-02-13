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

(defun config-spell ()
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  (setq jinx-languages "en_US es_ES")
  (custom-set-faces
   '(jinx-misspelled
     ((t (:underline (:style wave :color "orange" :thickness 3)))))
   '(jinx-incorrect
     ((t (:underline (:style wave :color "red" :thickness 3))))))
  (evil-define-key 'normal 'global (kbd "z =") #'jinx-correct)
  (evil-define-key 'normal 'global (kbd "] s") #'jinx-next)
  (evil-define-key 'normal 'global (kbd "[ s") #'jinx-previous))

(defun config-evil ()
  ;; 2-space indentation
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq evil-shift-width 2)
  ;; Enable key combinations
  (require 'key-chord)
  (key-chord-mode 1)
  ;; Set key chord detection delay
  (setq key-chord-two-keys-delay 0.5)
  ;; Exit insert/replace/visual mode on jk
  (key-chord-define evil-insert-state-map "jk" #'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" #'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" #'evil-normal-state)
  ;; Move between visual lines
  (evil-define-key nil evil-normal-state-map (kbd "gj") #'evil-next-visual-line)
  (evil-define-key nil evil-normal-state-map (kbd "gk") #'evil-previous-visual-line)
  ;; Set operation highlight duration
  (setq evil-goggles-duration 0.5))

(defun config-snippets ()
  (setq yas-snippet-dirs '("~/.dotfiles/base/snippets"))
  (yas-global-mode 1)
  (add-to-list 'auto-mode-alist '("\\.yas\\'" . snippet-mode))
  (setq yas-wrap-around-region t))

(defun config-org ()
  (after! org
    ;; Set dark background color in source code blocs
    (set-face-attribute
     'org-block nil :background
     (color-darken-name (face-attribute 'default :background) 3))
    ;; Fold content at startup
    (setq org-startup-folded t)
    ;; Do not indent nested content
    (setq org-startup-indented nil)
    ;; Exclude angle brackets from parentheses highlight
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table)))

;; Programming

(defun config-fish ()
  ;; 2-space indentation
  (setq c-basic-offset 2)
  (setq sh-basic-offset 2)
  (setq fish-indent-offset 2)
  ;; Treat _ as part of the word on *, #, w, b, e
  (add-hook 'sh-mode-hook
            #'(lambda () (modify-syntax-entry ?_ "w" sh-mode-syntax-table)))
  (add-to-list 'auto-mode-alist '("config\\'" . conf-mode)))

(defun config-sql ()
  (setq sql-product 'postgres)
  ;; Treat _ as part of the word on *, #, w, b, e
  (add-hook 'sql-mode-hook
            #'(lambda () (modify-syntax-entry ?_ "w" sql-mode-syntax-table))))

(defun config-web ()
  (setq css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))
  (setq web-mode-engines-alist '(("jinja2" . "\\.njk\\'")))
  (add-hook 'web-mode-hook
            #'(lambda ()
                (setq web-mode-markup-indent-offset 2)
                (setq web-mode-css-indent-offset 2)
                (setq web-mode-code-indent-offset 2))))

(defun config-go ()
  (setq-hook! 'go-mode-hook indent-tabs-mode nil))

(defun config-js ()
  (add-to-list 'auto-mode-alist '("\\.m?js\\'" . js2-mode))
  (setq js-indent-level 2)
  (setq js2-mode-show-strict-warnings nil))

(defun config-solidity ()
  (add-hook 'solidity-mode-hook #'(lambda () (setq c-basic-offset 2))))

(defun config-docker ()
  (setq dockerfile-indent-offset 2))

(defun config-elisp ()
  ;; Treat - as part of the word on *, #, w, b, e
  (add-hook 'emacs-lisp-mode-hook
    #'(lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))))

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
(config-parentheses)
(config-completion)
(config-spell)
(config-evil)
(config-snippets)
(config-org)

;; Programming
(config-fish)
(config-sql)
(config-web)
(config-go)
(config-js)
(config-solidity)
(config-docker)
(config-elisp)
(config-lilypond)
(config-d2)
