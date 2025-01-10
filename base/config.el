;; -*- lexical-binding: t; -*-

;; Editor

(defun config-doom ()
  ;; Maximize window on startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; Skip confirmation on exit
  (setq confirm-kill-emacs nil)
  ;; Start calendar from monday
  (setq calendar-week-start-day 1)
  )

(defun config-font ()
  (setq doom-font
        (font-spec :family "JetBrainsMono NF Light" :size 13.0 :weight 'light)))

(defun config-theme ()
  (setq doom-theme 'zenburn)
  (setq zenburn-override-colors-alist '(("zenburn-bg" . "#282523"))))

(defun config-ligatures ()
  (setq jet-brains-mono-ligatures
        '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
          "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
          "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
          "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
          "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
          "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
          ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
          "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
          "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
          "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
          "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  (ligature-set-ligatures 'go-mode jet-brains-mono-ligatures)
  (ligature-set-ligatures 'js2-mode jet-brains-mono-ligatures)
  (global-ligature-mode t))

(defun config-current-line ()
  (global-hl-line-mode 1)
  (custom-set-faces!
    ;; Highlight current line
    (set-face-attribute 'hl-line nil :foreground nil :background "#24190E")
    ;; Highlight visual selection
    (set-face-attribute 'region nil :foreground nil :background "#801515")
    ;; Highlight line numbers
    (set-face-attribute 'line-number nil :foreground nil :background "#32312E")
    (set-face-attribute 'line-number-current-line nil :foreground "#E2A665"
                        :background nil :weight 'bold)))

(defun config-whitespace ()
  ;; Enable global white space mode
  (require 'whitespace)
  (global-whitespace-mode t)
  (global-whitespace-toggle-options t)
  ;; Highlight line content exceeding the limit
  (setq-default whitespace-line-column 80)
  (setq-default whitespace-style '(face tab-mark trailing lines-tail))
  ;; Remove trailing whitespaces on save
  (add-hook 'before-save-hook #'whitespace-cleanup)
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
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (add-hook 'text-mode-hook #'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'extra-bold)
  (setq hl-paren-colors '("firebrick1" nil nil nil))
  ;; Handle parentheses automatically
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'text-mode-hook #'smartparens-mode))

(defun config-completion ()
  ;; Enable global company mode
  (add-hook 'after-init-hook #'global-company-mode)
  ;; Use M-j, M-k for company completion selection
  (evil-define-key nil company-active-map (kbd "M-m") #'company-select-next)
  (evil-define-key nil company-active-map (kbd "M-,") #'company-select-previous)
  (setq company-selection-wrap-around 1)
  ;; Use M-j, M-k for vertico completion selection
  (evil-define-key nil vertico-map (kbd "M-m") #'vertico-next)
  (evil-define-key nil vertico-map (kbd "M-,") #'vertico-previous))

(defun config-errors ()
  (after! evil-mode (unbind-key "] e"))
  (after! evil-mode (unbind-key "[ e"))
  (evil-define-key 'normal 'global (kbd "] e") #'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "[ e") #'flycheck-previous-error)
  (evil-define-key 'normal 'global (kbd "] x") #'flycheck-list-errors))

(defun config-spell ()
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
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

;; Editor
(config-doom)
(config-font)
(config-theme)
(config-ligatures)
(config-current-line)
(config-whitespace)
(config-parentheses)
(config-completion)
(config-errors)
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
