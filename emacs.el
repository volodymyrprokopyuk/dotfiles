;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Emacs configuration

(defun config-window ()
    ;; Disable toolbar
    (tool-bar-mode -1)
    ;; Disable menubar
    (menu-bar-mode -1)
    ;; Disable scrollbar
    (scroll-bar-mode -1)
    ;; Disable startup screen
    (setq inhibit-startup-screen t)
    ;; Maximize Emacs window on startup
    (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

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
    (set-frame-font "Source Code Pro Light 12")
    (global-set-key (kbd "C-+") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease))

(defun config-color ()
    (setq zenburn-override-colors-alist '(("zenburn-bg" . "#333333")))
    (add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
    (load-theme 'zenburn t))

(defun config-current-line ()
    (global-hl-line-mode 1)
    ;; Highlight current line
    (set-face-attribute 'hl-line nil :foreground nil :background "#262626")
    ;; Highlight visual selection
    (set-face-attribute 'region nil :foreground nil :background "#3C1414"))

(defun config-line-number ()
    (global-linum-mode t)
    (setq linum-format "%d "))

;; M-q format width of the selected text

(defun config-whitespace ()
    (require 'whitespace)
    (global-whitespace-mode t)
    (global-whitespace-toggle-options t)
    (setq-default whitespace-line-column 88)
    (setq-default fill-column 88)
    (add-hook 'text-mode-hook #'auto-fill-mode)
    (setq-default whitespace-style '(face tab-mark trailing lines-tail))
    (add-hook 'before-save-hook #'delete-trailing-whitespace)
    (setq require-final-newline t)
    (setq mode-require-final-newline t))

(defun config-indentation ()
    (setq-default indent-tabs-mode nil)
    (setq-default tab-width 4)
    (setq indent-line-function 'insert-tab))

(defun config-recent-file ()
    (recentf-mode 1)
    (setq recentf-max-saved-items 50)
    (setq recentf-exclude '("\\.tmp"))
    (run-with-timer 3600 (* 60 60) 'recentf-save-list))

(defun config-exec-path ()
    ;; Add the system $PATH to the Emacs exec-path
    (add-to-list 'load-path "~/.emacs.d/exec-path-from-shell")
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize))

(defun config-powerline ()
    (add-to-list 'load-path "~/.emacs.d/powerline")
    (add-to-list 'load-path "~/.emacs.d/spaceline")
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (spaceline-helm-mode 1)
    (spaceline-toggle-minor-modes-off)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq powerline-default-separator 'wave)
    (spaceline-compile))

;; Ctrl-y paste into the mini buffer

(defun config-helm ()
    (add-to-list 'load-path "~/.emacs.d/helm")
    (require 'helm-config)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-s ;") 'helm-mini)
    (global-set-key (kbd "M-s o") 'helm-occur)
    (global-set-key (kbd "M-s r") 'helm-recentf)
    (global-set-key (kbd "M-s l") 'helm-locate)
    (helm-mode 1)
    (define-key helm-map (kbd "M-j") 'helm-next-line)
    (define-key helm-map (kbd "M-k") 'helm-previous-line)
    (add-to-list 'load-path "~/.emacs.d/helm-xref")
    (require 'helm-xref))

;; Alt-s s search files content in a project
;; Alt-s f find files in a project

(defun config-helm-ag ()
    (add-to-list 'load-path "~/.emacs.d/emacs-helm-ag")
    (require 'helm-ag)
    (custom-set-variables
        '(helm-ag-base-command "ag --nocolor --nogroup")
        '(helm-ag-command-option "--hidden --follow"))
    (global-set-key (kbd "M-s s") 'helm-do-ag-project-root)
    (defun helm-ag-project-root-search-file-name ()
        "Searches file names in the project root using ag -g <pattern>"
        (interactive)
        (let ((helm-ag-command-option (concat helm-ag-command-option " -g")))
            (helm-do-ag-project-root)))
    (global-set-key (kbd "M-s f") 'helm-ag-project-root-search-file-name))

;; Tab complete the common part of suggestion list
;; Enter complete with the first suggestion
;; Alt-s Space trigger completion

(defun config-company ()
    (add-to-list 'load-path "~/.emacs.d/company-mode")
    (require 'company)
    (global-set-key (kbd "M-s SPC") 'company-complete)
    (add-hook 'after-init-hook #'global-company-mode)
    (setq company-dabbrev-downcase nil)
    (define-key company-active-map (kbd "M-j") #'company-select-next)
    (define-key company-active-map (kbd "M-k") #'company-select-previous))

;; Evil mode
;; Motions within a buffer
;;     Word/backwards/end: w/W, b/B, e/E, ge/gE
;;     Beginning/end of line: 0, ^, $,
;;     Search single char in current line: f/F, t/T -> ;/,
;;     Search regexp in a buffer: /?, */# -> n/N
;; Text objects: i/a w/s/p/t '/"/` )/}/]/>
;;     Change inside: ci_
;;     Delete around: da_
;; Operator + motion/text object: y, c, d, >, <, =, gc + w, it
;; Operator + operator: acts on the current line: cc, dd, >>, <<, ==, gcc, guu, gUU, g~~
;; Marks: m[mM]
;;     m mark in a buffer
;;     M global mark
;;     `m (go to marked position)
;; Change list in a buffer: g;/g,
;; Jump list between buffers in a window: Ctrl-o/Ctrl-i
;; Registers: "ryy, "rp
;;     r overwrite
;;     R append
;; Shift-r (replace mode: . A Shift-r , a)
;; q: (query and edit ex command history)
;; q/ (query and edit search history)
;; Insert mode/command line mode
;;     Alt-b, Alt-f (backwards, forward word)
;;     Ctrl-w (delete word backwards)
;;     Ctrl-r "/0 (paste unnamed/yank register)
;; Visual mode (should only be used when normal mode standard motions are not enough)
;;     o (other end of selection)
;;     Ctrl-v $ (ragged selection)
;; F1 k <key> show key binding

(defun config-evil ()
    (add-to-list 'load-path "~/.emacs.d/goto-chg.el")
    (require 'goto-chg)
    (setq evil-want-Y-yank-to-eol t)
    (add-to-list 'load-path "~/.emacs.d/evil")
    (require 'evil)
    (evil-mode 1)
    ;; Enable ibuffer on :ls
    (evil-ex-define-cmd "ls" 'ibuffer)
    (evil-set-initial-state 'ibuffer-mode 'normal)
    ;; Enable key combinations
    (add-to-list 'load-path "~/.emacs.d/key-chord.el")
    (require 'key-chord)
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.5)
    ;; Exit insert/replace/visual mode on jk
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-normal-state))

;; Normal mode: ys, cs, ds
;; Visual mode: S

(defun config-evil-surround ()
    (add-to-list 'load-path "~/.emacs.d/evil-surround")
    (require 'evil-surround)
    (global-evil-surround-mode 1)
    (evil-add-to-alist 'evil-surround-pairs-alist
        ?\( '("(" . ")") ?\) '("(" . ")")
        ?\[ '("[" . "]") ?\] '("[" . "]")
        ?\{ '("{" . "}") ?\} '("{" . "}")
        ?\< '("<" . ">") ?\> '("<" . ">")))

;; gcc comment a line
;; gc<movement> comment a movement
;; gc<a/i>(/[/{ comment outside/inside parentheses/brackets/braces

(defun config-evil-commenter ()
    (add-to-list 'load-path "~/.emacs.d/evil-nerd-commenter")
    (require 'evil-nerd-commenter)
    (define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator))

(defun config-evil-goggles ()
    (add-to-list 'load-path "~/.emacs.d/evil-goggles")
    (require 'evil-goggles)
    (evil-goggles-mode)
    (setq evil-goggles-duration 0.500)
    (custom-set-faces '(evil-goggles-default-face ((t (:inherit region))))))

;; Alt-j jump to a visible position in a buffer

(defun config-avy ()
    (add-to-list 'load-path "~/.emacs.d/avy")
    (require 'avy)
    ;; Smart-case search when jumping
    (setq avy-case-fold-search nil))
    (global-set-key (kbd "M-j") 'avy-goto-char-timer)

(defun config-parentheses ()
    ;; Highlight matching parentheses
    (show-paren-mode 1)
    (set-face-background 'show-paren-match (face-background 'company-preview-search))
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
    ;; Show in different collors nested matching parentheses
    (add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'text-mode-hook #'rainbow-delimiters-mode)
    ;; Highlight surrounding parentheses
    (add-to-list 'load-path "~/.emacs.d/highlight-parentheses.el")
    (require 'highlight-parentheses)
    (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
    (add-hook 'text-mode-hook #'highlight-parentheses-mode)
    (set-face-attribute 'hl-paren-face nil :weight 'extra-bold)
    (setq hl-paren-colors '("firebrick1" nil nil nil))
    ;; Handle parentheses automatically
    (add-to-list 'load-path "~/.emacs.d/smartparens")
    (require 'smartparens-config)
    (add-hook 'prog-mode-hook #'smartparens-mode)
    (add-hook 'text-mode-hook #'smartparens-mode))

;; gd go to definition

(defun config-dumb-jump ()
    (add-to-list 'load-path "~/.emacs.d/dumb-jump")
    (autoload 'dumb-jump-xref-activate "dumb-jump.elc" nil t)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defun config-scheme ()
    (add-to-list 'load-path "~/.emacs.d/racket-mode")
    (autoload 'racket-mode "racket-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))
    ;; Treat - as part of the word on *, #, w, b, e
    (add-hook 'racket-mode-hook
        #'(lambda () (modify-syntax-entry ?- "w" racket-mode-syntax-table)))
    ;; Replace lambda word with greek letter
    (defun replace-lambda-with-greek-letter ()
        (font-lock-add-keywords nil
            `(("\\<lambda\\>"
                  (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                ,(make-char 'greek-iso8859-7 107)) nil))))))
    (add-hook 'racket-mode-hook #'replace-lambda-with-greek-letter)
    ;; Rebind expand region mode keys
    (defun racket-mode-hook-setup ()
        (local-set-key (kbd "M-,") 'er/expand-region)
        (local-set-key (kbd "M-m") 'er/contract-region))
    (add-hook 'racket-mode-hook #'racket-mode-hook-setup)
    (load "~/.emacs.d/config/scheme"))

(defun config-sql ()
    (setq sql-product 'postgres)
    ;; Treat _ as part of the word on *, #, w, b, e
    (add-hook 'sql-mode-hook
        #'(lambda () (modify-syntax-entry ?_ "w" sql-mode-syntax-table))))

(defun config-zsh ()
    (setq-default sh-basic-offset 4)
    (add-hook 'sh-mode-hook #'(lambda () (sh-set-shell "zsh")))
    ;; Treat -, _ as part of the word on *, #, w, b, e
    (add-hook 'sh-mode-hook
        #'(lambda () (modify-syntax-entry ?_ "w" sh-mode-syntax-table)))
    (add-hook 'sh-mode-hook
        #'(lambda () (modify-syntax-entry ?- "w" sh-mode-syntax-table))))

(defun config-javascript ()
    (add-to-list 'load-path "~/.emacs.d/js2-mode")
    (autoload 'js2-mode "js2-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
    (setq-default js2-basic-offset 4)
    (setq-default js-indent-level 4)
    ;; Treat _ as part of the word on *, #, w, b, e
    (add-hook 'js2-mode-hook
        #'(lambda () (modify-syntax-entry ?_ "w" js2-mode-syntax-table))))

(defun config-typescript ()
    (add-to-list 'load-path "~/.emacs.d/typescript.el")
    (autoload 'typescript-mode "typescript-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    ;; Treat _ as part of the word on *, #, w, b, e
    (add-hook 'typescript-mode-hook
        #'(lambda () (modify-syntax-entry ?_ "w" typescript-mode-syntax-table))))

(defun config-r ()
    (add-to-list 'load-path "~/.emacs.d/ESS/lisp")
    (autoload 'ess-r-mode "ess-r-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
    ;; Treat _ as part of the word on *, #, w, b, e
    (add-hook 'ess-r-mode-hook
        #'(lambda () (modify-syntax-entry ?_ "w" ess-r-mode-syntax-table)))
    ;; Start comment with single #
    (add-hook 'ess-mode-hook #'(lambda () (setq comment-add 0))))

(defun config-prolog ()
    (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode)))

(defun config-sml ()
    (add-to-list 'load-path "~/.emacs.d/sml-mode.el")
    (autoload 'sml-mode "sml-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.sml\\'" . sml-mode))
    (add-to-list 'auto-mode-alist '("\\.sig\\'" . sml-mode)))

(defun config-elisp ()
    (setq-default lisp-indent-offset 4)
    ;; Treat - as part of the word on *, #, w, b, e
    (add-hook 'emacs-lisp-mode-hook
        #'(lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (setq ediff-split-window-function 'split-window-horizontally))

(defun config-c ()
    (setq-default c-basic-offset 4)
    ;; Treat _ as part of the word on *, #, w, b, e
    (add-hook 'java-mode-hook
        #'(lambda () (modify-syntax-entry ?_ "w" java-mode-syntax-table)))
    (add-hook 'python-mode-hook
        #'(lambda () (modify-syntax-entry ?_ "w"))))

(defun config-markdown ()
    (add-to-list 'load-path "~/.emacs.d/markdown-mode")
    (autoload 'markdown-mode "markdown-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(defun config-web ()
    (add-to-list 'load-path "~/.emacs.d/web-mode")
    (autoload 'web-mode "web-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
    (add-hook 'web-mode-hook
        #'(lambda ()
              (setq web-mode-markup-indent-offset 4)
              (setq web-mode-attr-indent-offset 4)
              (setq web-mode-css-indent-offset 4)
              (setq web-mode-code-indent-offset 4))))

(defun config-yaml ()
    (add-to-list 'load-path "~/.emacs.d/yaml-mode")
    (autoload 'yaml-mode "yaml-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
    (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
    (setq-default yaml-indent-offset 4))

(defun config-xml ()
    (add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode)))

(defun config-docker ()
    (add-to-list 'load-path "~/.emacs.d/dockerfile-mode")
    (autoload 'dockerfile-mode "dockerfile-mode.elc" nil t)
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply Emacs configuration

;; Common extensions
(add-to-list 'load-path "~/.emacs.d/s.el")
(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/paredit.el")
(add-to-list 'load-path "~/.emacs.d/pos-tip.el")
(add-to-list 'load-path "~/.emacs.d/faceup")

;; Basic editor
(config-window)
(config-clipboard)
(config-font)
(config-color)
(config-current-line)
(config-line-number)
(config-whitespace)
(config-indentation)
(config-recent-file)

;; Extended editor
(config-exec-path)
(config-powerline) ;; s.el dash.el
(config-helm) ;; emacs-async
(config-helm-ag) ;; helm
(config-company)
(config-evil)
(config-evil-surround)
(config-evil-commenter)
(config-evil-goggles)
(config-avy)

;; Common programming
(config-parentheses)
(config-expand-region)
(config-dumb-jump) ;; popup-el

;; Programming languages
(config-scheme) ;; paredit.el pos-tip.el faceup
(config-sql)
(config-zsh)
(config-javascript)
(config-typescript)
(config-r)
(config-prolog)
(config-sml)
(config-elisp)
(config-c)

;; Markup languages
(config-markdown)
(config-web)
(config-yaml)
(config-xml)
(config-docker)
