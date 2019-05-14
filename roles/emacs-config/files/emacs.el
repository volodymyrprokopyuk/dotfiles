; disable toolbar
(tool-bar-mode -1)
; disable menubar
(menu-bar-mode -1)
; disable scrollbar
(scroll-bar-mode -1)
; disable startup screen
(setq inhibit-startup-screen t)
; maximize emacs window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
; set calendar week start day to Monday
(add-hook 'calendar-mode-hook '(lambda () (setq calendar-week-start-day 1)))

; show line numbers
(global-linum-mode t)
(add-hook 'linum-mode-hook '(lambda () (setq linum-format "%d ")))
; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
; highlight matching parenthesis
(show-paren-mode 1)

; Recentf mode
(recentf-mode 1)
(setq recentf-max-menu-items 512)
(add-to-list 'recentf-exclude (expand-file-name "~/.emacs.d/recentf"))
(run-with-timer 3600 (* 60 60) 'recentf-save-list)

; terminal clipboard yank/paste
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

; WiteSpace mode
(require 'whitespace)
(global-whitespace-mode t)
(global-whitespace-toggle-options t)
; columns margin
(setq-default whitespace-line-column 88)
; M-q -> format selected text
(setq-default fill-column 88)
; show tab marks, trailing blanks, long lines
(setq-default whitespace-style '(face tab-mark trailing lines-tail))
; remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; put newline at the end of a file
(setq require-final-newline t)
(setq mode-require-final-newline t)
; automatically format text when typing
(add-hook 'text-mode-hook 'auto-fill-mode)

; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
; Emacs Lisp indentation
(setq-default lisp-indent-offset 4)
; Bash indentation
(setq-default sh-basic-offset 4)
; C indentation
(setq-default c-basic-offset 4)

; Source Code Pro font
(set-frame-font "Source Code Pro Light 12")
; increase/declease font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

; common extensions
(add-to-list 'load-path "~/.emacs.d/epl")
(add-to-list 'load-path "~/.emacs.d/pkg-info.el")
(add-to-list 'load-path "~/.emacs.d/s.el")
(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/emacs-async")

; add $PATH to exec-path
(add-to-list 'load-path "~/.emacs.d/exec-path-from-shell")
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

; Zenburn theme (done)
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
(load-theme 'zenburn t)

; Rainbow Delimiters mode (done)
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Powerline/Spaceline modeline (done)
(add-to-list 'load-path "~/.emacs.d/powerline")
(add-to-list 'load-path "~/.emacs.d/spaceline")
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)
(spaceline-toggle-minor-modes-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'wave)
(spaceline-compile)

; Smartparens mode
(add-to-list 'load-path "~/.emacs.d/smartparens")
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'text-mode-hook 'smartparens-mode)

; Helm mode
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-s ;") 'helm-mini)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "M-s r") 'helm-recentf)
(global-set-key (kbd "M-s l") 'helm-locate)
(helm-mode 1)

; Helm Ag mode (done)
(add-to-list 'load-path "~/.emacs.d/emacs-helm-ag")
(require 'helm-ag)
(custom-set-variables
    '(helm-ag-base-command "ag --nocolor --nogroup")
    '(helm-ag-command-option "--hidden --ignore *~ --ignore .git --ignore .idea --ignore __pycache__ --ignore pyvenv --ignore htmlcov"))
(global-set-key (kbd "M-s s") 'helm-do-ag-project-root)

(defun helm-do-ag-project-root-search-file-names ()
    "Searches file names in a project using ag -g <pattern> command line tool"
    (interactive)
    (let ((helm-ag-command-option (concat helm-ag-command-option " -g")))
        (helm-do-ag-project-root)))

(global-set-key (kbd "M-s f") 'helm-do-ag-project-root-search-file-names)

; Company mode
(add-to-list 'load-path "~/.emacs.d/company-mode")
(require 'company)
(global-set-key (kbd "M-s SPC") 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

; Avy mode
(add-to-list 'load-path "~/.emacs.d/avy")
(require 'avy)
(global-set-key (kbd "M-s j") 'avy-goto-char-timer)

; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

; Ediff mode
(setq ediff-split-window-function 'split-window-horizontally)

; Kotlin mode
(add-to-list 'load-path "~/.emacs.d/kotlin-mode")
(autoload 'kotlin-mode "kotlin-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))
(add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-mode))

; JS2 mode
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(setq-default js2-basic-offset 4)
(setq-default js-indent-level 4)

; CQL mode
(add-to-list 'auto-mode-alist '("\\.cql\\'" . sql-mode))

; JS format
(defun js-format ()
    "Format JS region using js-beautify"
    (interactive)
    (save-excursion
        (shell-command-on-region (mark) (point) "js-beautify --config ~/.jsbeautifyrc --type js -" (buffer-name) t)))

(eval-after-load 'js2-mode '(define-key js2-mode-map (kbd "M-s \\") 'js-format))

; JSON format
(defun json-format ()
    "Format JSON region using jq"
    (interactive)
    (save-excursion
        (shell-command-on-region (mark) (point) "jq --indent 4 ." (buffer-name) t)))

(eval-after-load 'js '(define-key js-mode-map (kbd "M-s \\") 'json-format))

; Web mode
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

; HTML format
(defun html-format ()
    "Format HTML region using js-beautify"
    (interactive)
    (save-excursion
        (shell-command-on-region (mark) (point) "js-beautify --config ~/.jsbeautifyrc --type html -" (buffer-name) t))
    (web-mode))

(eval-after-load 'web-mode '(define-key web-mode-map (kbd "M-s \\") 'html-format))

; CSS format
(defun css-format ()
    "Format CSS region using js-beautify"
    (interactive)
    (save-excursion
        (shell-command-on-region (mark) (point) "js-beautify --config ~/.jsbeautifyrc --type css -" (buffer-name) t))
    (web-mode))

; Emmet mode
; C-j => expand line
(add-to-list 'load-path "~/.emacs.d/emmet-mode")
(autoload 'emmet-mode "emmet-mode" nil t)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)

; Markdown mode
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; XML format
(defun xml-format ()
    "Format XML region with xmlstarlet"
    (interactive)
    (save-excursion
        (shell-command-on-region (mark) (point) "xmlstarlet format --indent-spaces 4" (buffer-name) t)))

(eval-after-load 'nxml-mode '(define-key nxml-mode-map (kbd "M-s \\") 'xml-format))

; XSD mode
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))

; YAML mode
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(setq-default yaml-indent-offset 4)

; Dockerfile mode
(add-to-list 'load-path "~/.emacs.d/dockerfile-mode")
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

; PlantUML mode
(add-to-list 'load-path "~/.emacs.d/plantuml-mode")
(autoload 'plantuml-mode "plantuml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")

; Evil mode
(add-to-list 'load-path "~/.emacs.d/goto-chg.el")
(require 'goto-chg)

(add-to-list 'load-path "~/.emacs.d/key-chord.el")
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; exit insert mode
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)

; move selected lines up (K) and down (J)
(define-key evil-visual-state-map "J"
    (concat ":m '>+1" (kbd "RET") "gv=gv"))
(define-key evil-visual-state-map "K"
    (concat ":m '<-2" (kbd "RET") "gv=gv"))

; treat _ as part of the word on *, #, w
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'sql-mode-hook #'(lambda () (modify-syntax-entry ?_ "w" sql-mode-syntax-table)))
(add-hook 'java-mode-hook #'(lambda () (modify-syntax-entry ?_ "w" java-mode-syntax-table)))

; Ibuffer mode
(evil-ex-define-cmd "ls" 'ibuffer)
(evil-set-initial-state 'ibuffer-mode 'normal)

; Evil Surround mode
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

; Evil Nerd Commenter
(add-to-list 'load-path "~/.emacs.d/evil-nerd-commenter")
(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)
(global-set-key (kbd "M-,") 'evilnc-comment-or-uncomment-lines)

; Evil Goggles mode
(add-to-list 'load-path "~/.emacs.d/evil-goggles")
(require 'evil-goggles)
(evil-goggles-mode)
(setq evil-goggles-duration 0.500)
(custom-set-faces
    '(evil-goggles-default-face ((t (:inherit isearch-fail)))))
