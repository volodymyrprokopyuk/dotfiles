;; Disable toolbar
(tool-bar-mode -1)
;; Disable menubar
(menu-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)
;; Disable startup screen
(setq inhibit-startup-screen t)
;; Maximize emacs window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Set calendar week start day to Monday
(add-hook 'calendar-mode-hook '(lambda () (setq calendar-week-start-day 1)))

;; Show line numbers
(global-linum-mode t)
(add-hook 'linum-mode-hook '(lambda () (setq linum-format "%d ")))
;; Highlight current line
(global-hl-line-mode 1)
;; Highlight matching parenthesis
(show-paren-mode 1)

;; Recentf mode
(recentf-mode 1)
(setq recentf-max-menu-items 512)
(add-to-list 'recentf-exclude (expand-file-name "~/.emacs.d/recentf"))
(run-with-timer 3600 (* 60 60) 'recentf-save-list)

;; Terminal clipboard yank/paste
(setq select-enable-clipboard t)
(unless window-system
    (when (getenv "DISPLAY")
        (defun xsel-cut-function (text &optional push)
            (with-temp-buffer
                (insert text)
                (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
        (defun xsel-paste-function()
            (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
                (unless (string= (car kill-ring) xsel-output) xsel-output )))
        (setq interprogram-cut-function 'xsel-cut-function)
        (setq interprogram-paste-function 'xsel-paste-function)))

;; WiteSpace mode
(require 'whitespace)
(global-whitespace-mode t)
(global-whitespace-toggle-options t)
;; Columns margin
(setq-default whitespace-line-column 88)
;; M-q -> format selected text
(setq-default fill-column 88)
;; Show tab marks, trailing blanks, long lines
(setq-default whitespace-style '(face tab-mark trailing lines-tail))
;; Remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Put newline at the end of a file
(setq require-final-newline t)
(setq mode-require-final-newline t)
;; Automatically format text when typing
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
;; Emacs Lisp indentation
(setq-default lisp-indent-offset 4)
;; Bash indentation
(setq-default sh-basic-offset 4)
;; C indentation
(setq-default c-basic-offset 4)

;; Flyspell mode
(global-set-key (kbd "<f8>") 'flyspell-buffer)
;; ]s next word
;; [s previous word
;; z= show suggestions

;; Source Code Pro font
(set-frame-font "Source Code Pro Light 12")
;; (set-frame-font "Jetbrains Mono 12")
;; Increase/declease font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Common extensions
(add-to-list 'load-path "~/.emacs.d/epl")
(add-to-list 'load-path "~/.emacs.d/pkg-info.el")
(add-to-list 'load-path "~/.emacs.d/pos-tip.el")
(add-to-list 'load-path "~/.emacs.d/paredit.el")
(add-to-list 'load-path "~/.emacs.d/faceup")
(add-to-list 'load-path "~/.emacs.d/s.el")
(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/emacs-async")

;; Add $PATH to exec-path
(add-to-list 'load-path "~/.emacs.d/exec-path-from-shell")
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Zenburn theme
(setq zenburn-override-colors-alist '(("zenburn-bg" . "#333333")))
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
(load-theme 'zenburn t)

;; Highlight current line
(set-face-attribute 'hl-line nil :foreground nil :background "#262626")
;; Highlight visual selection
(set-face-attribute 'region nil :foreground nil :background "#3C1414")

;; Rainbow Delimiters mode
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Powerline/Spaceline modeline
(add-to-list 'load-path "~/.emacs.d/powerline")
(add-to-list 'load-path "~/.emacs.d/spaceline")
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)
(spaceline-toggle-minor-modes-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'wave)
(spaceline-compile)

;; Smartparens mode
(add-to-list 'load-path "~/.emacs.d/smartparens")
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'text-mode-hook 'smartparens-mode)

;; Expand region mode
(add-to-list 'load-path "~/.emacs.d/expand-region.el")
(require 'expand-region)
(global-set-key (kbd "M-,") 'er/expand-region)
(global-set-key (kbd "M-m") 'er/contract-region)

;; Helm mode
;; Ctrl-y paste into mini buffer
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

;; Helm Ag mode
(add-to-list 'load-path "~/.emacs.d/emacs-helm-ag")
(require 'helm-ag)
(custom-set-variables
    '(helm-ag-base-command "ag --nocolor --nogroup")
    '(helm-ag-command-option "--hidden --follow"))
(global-set-key (kbd "M-s s") 'helm-do-ag-project-root)

(defun helm-do-ag-project-root-search-file-names ()
    "Searches file names in a project using ag -g <pattern> command line tool"
    (interactive)
    (let ((helm-ag-command-option (concat helm-ag-command-option " -g")))
        (helm-do-ag-project-root)))
(global-set-key (kbd "M-s f") 'helm-do-ag-project-root-search-file-names)

;; Company mode
(add-to-list 'load-path "~/.emacs.d/company-mode")
(require 'company)
(global-set-key (kbd "M-s SPC") 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)
(define-key company-active-map (kbd "M-j") #'company-select-next)
(define-key company-active-map (kbd "M-k") #'company-select-previous)

;; Avy mode
(add-to-list 'load-path "~/.emacs.d/avy")
(require 'avy)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
;; Smart-case search
(setq avy-case-fold-search nil)

;; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Ediff mode
(setq ediff-split-window-function 'split-window-horizontally)

;; JS2 mode
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(setq-default js2-basic-offset 4)
(setq-default js-indent-level 4)

;; TypeScript mode
(add-to-list 'load-path "~/.emacs.d/typescript.el")
(autoload 'typescript-mode "typescript-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; Scheme mode
(add-to-list 'load-path "~/.emacs.d/racket-mode")
(autoload 'racket-mode "racket-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))
;; Normal (not aligned) identation
(put 'normal-ident 'racket-indent-function 1)
;; Keywords, builtins, and type highlighting
(font-lock-add-keywords 'racket-mode
    `((,(regexp-opt '("my-keyword1" "my-keyword2") t) . font-lock-keyword-face)
      (,(regexp-opt '("my-builtin1" "my-builtin2") t) . font-lock-builtin-face)
      (,(regexp-opt '("MyType1" "MyType2") t) . font-lock-type-face)))
;; Scheme REPL mode
(add-to-list 'load-path "~/.emacs.d/geiser/elisp")
(autoload 'geiser "geiser" nil t)
(setq geiser-active-implementations '(guile))
(global-set-key (kbd "M-s >") 'geiser)

;; SML mode
(add-to-list 'load-path "~/.emacs.d/sml-mode.el")
(autoload 'sml-mode "sml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sml\\'" . sml-mode))
(add-to-list 'auto-mode-alist '("\\.sig\\'" . sml-mode))

;; R mode
(add-to-list 'load-path "~/.emacs.d/ESS/lisp")
(autoload 'ess-r-mode "ess-r-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
;; Comment with a single #
(defun ess-mode-hook-setup ()
  (setq comment-add 0))
(add-hook 'ess-mode-hook 'ess-mode-hook-setup)

;; Web mode
(add-to-list 'load-path "~/.emacs.d/web-mode")
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-hook 'web-mode-hook
    '(lambda ()
        (setq web-mode-markup-indent-offset 4)
        (setq web-mode-attr-indent-offset 4)
        (setq web-mode-css-indent-offset 4)
        (setq web-mode-code-indent-offset 4)))

;; Emmet mode
;; C-j => expand line
(add-to-list 'load-path "~/.emacs.d/emmet-mode")
(autoload 'emmet-mode "emmet-mode" nil t)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)

;; Pug mode
(add-to-list 'load-path "~/.emacs.d/emacs-pug-mode")
(autoload 'pug-mode "pug-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pug\\'" . pug-mode))

;; Prolog mode
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; SQL mode
(setq sql-product 'postgres)
(add-to-list 'auto-mode-alist '("\\.cql\\'" . sql-mode))

;; Dockerfile mode
(add-to-list 'load-path "~/.emacs.d/dockerfile-mode")
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; PlantUML mode
(add-to-list 'load-path "~/.emacs.d/plantuml-mode")
(autoload 'plantuml-mode "plantuml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")

;; Gnuplot mode
(add-to-list 'load-path "~/.emacs.d/gnuplot-mode")
(autoload 'gnuplot-mode "gnuplot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.gplot\\'" . gnuplot-mode))
(setq gnuplot-program "/bin/gnuplot")

;; Terraform mode
(add-to-list 'load-path "~/.emacs.d/emacs-hcl-mode")
(add-to-list 'load-path "~/.emacs.d/emacs-terraform-mode")
(autoload 'terraform-mode "terraform-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(custom-set-variables '(terraform-indent-level 4))

;; Markdown mode
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; YAML mode
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(setq-default yaml-indent-offset 4)

;; XSD mode
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))

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
(add-to-list 'load-path "~/.emacs.d/goto-chg.el")
(require 'goto-chg)

(add-to-list 'load-path "~/.emacs.d/key-chord.el")
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

(setq evil-want-Y-yank-to-eol t)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Exit insert/replace/visual mode
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)

;; Treat _ as part of the word on *, #, w, b
(add-hook 'emacs-lisp-mode-hook
    #'(lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))
(add-hook 'sh-mode-hook
    #'(lambda () (modify-syntax-entry ?_ "w" sh-mode-syntax-table)))
(add-hook 'sh-mode-hook
    #'(lambda () (modify-syntax-entry ?- "w" sh-mode-syntax-table)))
(add-hook 'sql-mode-hook
    #'(lambda () (modify-syntax-entry ?_ "w" sql-mode-syntax-table)))
(add-hook 'js2-mode-hook
    #'(lambda () (modify-syntax-entry ?_ "w" js2-mode-syntax-table)))
(add-hook 'typescript-mode-hook
    #'(lambda () (modify-syntax-entry ?_ "w" typescript-mode-syntax-table)))
(add-hook 'python-mode-hook
    #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'java-mode-hook
    #'(lambda () (modify-syntax-entry ?_ "w" java-mode-syntax-table)))
(add-hook 'ess-r-mode-hook
    #'(lambda () (modify-syntax-entry ?_ "w" ess-r-mode-syntax-table)))
(add-hook 'racket-mode-hook
    #'(lambda () (modify-syntax-entry ?- "w" racket-mode-syntax-table)))

;; Ibuffer mode
(evil-ex-define-cmd "ls" 'ibuffer)
(evil-set-initial-state 'ibuffer-mode 'normal)

;; Evil Surroune mode
;; Normal mode: ys, cs, ds
;; Visual mode: S
(add-to-list 'load-path "~/.emacs.d/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)
(evil-add-to-alist 'evil-surround-pairs-alist
    ?\( '("(" . ")")
    ?\[ '("[" . "]")
    ?\{ '("{" . "}")
    ?\) '("(" . ")")
    ?\] '("[" . "]")
    ?\} '("{" . "}"))

;; Evil Nerd Commenter
(add-to-list 'load-path "~/.emacs.d/evil-nerd-commenter")
(require 'evil-nerd-commenter)
(define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map "gc" 'evilnc-comment-operator)

;; Evil Goggles mode
(add-to-list 'load-path "~/.emacs.d/evil-goggles")
(require 'evil-goggles)
(evil-goggles-mode)
(setq evil-goggles-duration 0.500)
(custom-set-faces '(evil-goggles-default-face ((t (:inherit region)))))
