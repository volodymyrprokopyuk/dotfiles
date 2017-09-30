; disable Toolbar
(tool-bar-mode -1)
; disable Menubar
(menu-bar-mode -1)
; disable Scrollbar
(scroll-bar-mode -1)
; disable Startup Screen
(setq inhibit-startup-screen t)
(add-hook 'calendar-mode-hook
  ; set calendar week start day to Monday
  '(lambda () (setq calendar-week-start-day 1)))

; show line numbers
(global-linum-mode t)
(add-hook 'linum-mode-hook
  '(lambda () (setq linum-format "%d ")))
; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
; highlight matching parenthesis
(show-paren-mode 1)

; terminal clipboard yank/paste
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
          (unless (string= (car kill-ring) xsel-output) xsel-output )))
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)))

; XML format
(defun xml-format ()
  "Format XML region using xmllint."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -"
      (buffer-name) t)))

; WiteSpace mode
(require 'whitespace)
(global-whitespace-mode t)
(global-whitespace-toggle-options t)
; 120 columns margin
(setq-default whitespace-line-column 120)
; M-q -> format selected text
(setq-default fill-column 120)
; show tab marks, trailing blanks, long lines
(setq-default whitespace-style '(face tab-mark trailing lines-tail))
; remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; automatically format text when typing
(add-hook 'text-mode-hook 'auto-fill-mode)

; indentation
; C-c C-s => show offset variable
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
; Emacs Lisp indentation
(setq-default lisp-indent-offset 2)
; Bash indentation
(setq-default sh-basic-offset 2)
; C indentation
(setq-default c-basic-offset 2)

; Source Code Pro Font
(set-frame-font "Source Code Pro Light 14")

; Common Extensions
(add-to-list 'load-path "~/.emacs.d/epl")
(add-to-list 'load-path "~/.emacs.d/pkg-info.el")
(add-to-list 'load-path "~/.emacs.d/s.el")
(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/emacs-async")

; Add $PATH to exec-path
(add-to-list 'load-path "~/.emacs.d/exec-path-from-shell")
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

; Zenburn Theme (done)
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
(load-theme 'zenburn t)

; Rainbow Delimiters Mode (done)
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Powerline/Spaceline Modeline (done)
(add-to-list 'load-path "~/.emacs.d/powerline")
(add-to-list 'load-path "~/.emacs.d/spaceline")
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'wave)
(spaceline-compile)

; Smartparens Mode
(add-to-list 'load-path "~/.emacs.d/smartparens")
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'text-mode-hook 'smartparens-mode)

; Helm Mode
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-s ;") 'helm-mini)
(global-set-key (kbd "M-s o") 'helm-occur)
(helm-mode 1)

; Helm Ag Mode (done)
(add-to-list 'load-path "~/.emacs.d/emacs-helm-ag")
(require 'helm-ag)
(custom-set-variables
  '(helm-ag-base-command "ag --nocolor --nogroup")
  '(helm-ag-command-option "--hidden --ignore *~ --ignore .git"))
(global-set-key (kbd "M-s s") 'helm-do-ag-project-root)

; Company Mode
(add-to-list 'load-path "~/.emacs.d/company-mode")
(require 'company)
(global-set-key (kbd "M-s SPC") 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)

; YASnippet Mode
;(add-to-list 'load-path "~/.emacs.d/yasnippet")
;(require 'yasnippet)
;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;(yas-global-mode 1)

; Emacs Lisp Mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

; JS2 Mode
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)

; Web Mode
(add-to-list 'load-path "~/.emacs.d/web-mode")
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-hook 'web-mode-hook
  '(lambda ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))

; Emmet Mode
; C-j => expand line
(add-to-list 'load-path "~/.emacs.d/emmet-mode")
(autoload 'emmet-mode "emmet-mode" nil t)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)

; Markdown Mode
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; YAML Mode
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

; Elixir Mode
;(add-to-list 'load-path "~/.emacs.d/emacs-elixir")
;(autoload 'elixir-mode "elixir-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
;(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
;(add-to-list 'auto-mode-alist '("\\.gradle\\'" . elixir-mode))

; Erlang Mode
;(add-to-list 'load-path "~/.emacs.d/otp/lib/tools/emacs")
;(require 'erlang-start)
;(add-to-list 'exec-path "~/local/erlang/bin")
;(add-hook 'erlang-mode
;  '(lambda () (setq erlang-root-dir "~/local/erlang")))

; Evil Mode
(add-to-list 'load-path "~/.emacs.d/goto-chg.el")
(require 'goto-chg)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; Ibuffer Mode
(evil-ex-define-cmd "ls" 'ibuffer)
(evil-set-initial-state 'ibuffer-mode 'normal)
