; disable Toolbar
(tool-bar-mode -1)
; disable Menubar
(menu-bar-mode -1)
; disable Scrollbar
(scroll-bar-mode -1)
; disable StartupScreen
(setq inhibit-startup-screen t)

; show line numbers
(global-linum-mode t)
(setq linum-format "%d ")
; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
; highlight matching parenthesis
(show-paren-mode 1)

; terminal clipboard yank/paste
; $ sudo apt-get install xsel
(setq x-select-enable-clipboard t)
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
; $ sudo apt-get install libxml2-utils
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -"
      (buffer-name) t)))

; WiteSpace mode
(require 'whitespace)
(global-whitespace-mode t)
(global-whitespace-toggle-options t)
; 80 columns margin
(setq-default whitespace-line-column 80)
; M-q -> format selected text
(setq-default fill-column 80)
; show tab marks, trailing blanks, long lines
(setq-default whitespace-style '(face tab-mark trailing lines-tail))
; remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
; Emacs Lisp indentation
(setq-default lisp-indent-offset 2)
; Bash indentation
(setq-default sh-basic-offset 2)
; C indentation
(setq-default c-basic-offset 2)

; Source Code Pro font
(set-frame-font "Source Code Pro Light 14")

; Zenburn color
; $ git clone https://github.com/bbatsov/zenburn-emacs.git
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
(load-theme 'zenburn t)

; Rainbow delimiters mode
; $ git clone https://github.com/Fanael/rainbow-delimiters.git
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Helm mode
; $ git clone https://github.com/jwiegley/emacs-async.git
; $ git clone https://github.com/emacs-helm/helm.git
(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

; YASnippet mode
; $ git clone https://github.com/capitaomorte/yasnippet.git
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

; JS2 mode
; $ git clone https://github.com/mooz/js2-mode.git
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(setq-default js2-basic-offset 2)

; Web mode
; $ git clone https://github.com/fxbois/web-mode.git
(add-to-list 'load-path "~/.emacs.d/web-mode")
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

; Emmet mode
; $ git clone https://github.com/smihica/emmet-mode.git
; C-j -> expand line
(add-to-list 'load-path "~/.emacs.d/emmet-mode")
(autoload 'emmet-mode "emmet-mode" nil t)
(add-hook 'web-mode-hook 'emmet-mode)

; Markdown mode
; $ git clone https://github.com/jrblevin/markdown-mode.git
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; Jade mode
; $ git clone https://github.com/brianc/jade-mode.git
(add-to-list 'load-path "~/.emacs.d/jade-mode")
(autoload 'jade-mode "jade-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

; Clojure mode
; $ git clone https://github.com/clojure-emacs/clojure-mode.git
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

; Cucumber mode
; $ git clone https://github.com/michaelklishin/cucumber.el.git
(add-to-list 'load-path "~/.emacs.d/cucumber.el")
(autoload 'feature-mode "feature-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))

; Ninja mode
; $ git clone https://github.com/martine/ninja.git
(add-to-list 'load-path "~/.emacs.d/ninja")
(autoload 'ninja-mode "ninja-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ninja\\'" . ninja-mode))

; Evil mode
; $ git clone https://gitorious.org/evil/evil.git
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; Ibuffer mode
(evil-ex-define-cmd "ls" 'ibuffer)
(evil-set-initial-state 'ibuffer-mode 'normal)
