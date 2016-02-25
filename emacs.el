; remove Toolbar
(tool-bar-mode -1)
; remove Menubar
(menu-bar-mode -1)
; remove Scrollbar
(scroll-bar-mode -1)
; remove StartupScreen
(setq inhibit-startup-screen t)

; line numbers
(line-number-mode t)
; column number in Modeline
(column-number-mode t)
; line number on the left
(global-linum-mode t)
; line numbers separation
(setq linum-format "%d ")
; highlight current line
(global-hl-line-mode 1)
; show matching parenthesis
(show-paren-mode 1)

; font
(set-default-font "Source Code Pro Light 12")
; set correct font in GUI with emacs server
(setq default-frame-alist '((font . "Source Code Pro Light 12")))

; color
; Zenburn
; $ git clone https://github.com/bbatsov/zenburn-emacs.git
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
(load-theme 'zenburn t)

; WiteSpace mode
(global-whitespace-mode t)
(global-whitespace-toggle-options t)
; 80 columns
(setq-default whitespace-line-column 80)
; tabs, trailing white spaces
(setq-default whitespace-style '(face tab-mark trailing lines-tail empty))
; delete trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
; JS indentation
(setq-default js2-basic-offset 2)
; C/C++ indentation
(setq c-basic-offset 2)
; CSS indentation
(setq css-indent-offset 2)
; bash indentation
(setq sh-basic-offset 2)
; elisp indentation
(setq lisp-indent-offset 2)

; Rainbow delimiters mode
; $ git clone https://github.com/Fanael/rainbow-delimiters.git
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; terminal copy/paste to/from clipboard
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
  (setq interprogram-paste-function 'xsel-paste-function))
)

; XML format
; $ sudo apt-get install libxml2-utils
; M-x xml-format RET (format XML)
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -"
      (buffer-name) t)))

; RecentFiles mode
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

; Smex mode
; $ git clone https://github.com/nonsequitur/smex.git
(add-to-list 'load-path "~/.emacs.d/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; PostgreSQL mode
(add-to-list 'auto-mode-alist '("\\.sql\\'" . (lambda () (sql-mode)
  (sql-highlight-postgres-keywords))))

; TeX mode
; M-q (format selection)
(setq-default fill-column 80)

; JS2 mode
; $ git clone https://github.com/mooz/js2-mode.git
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

; Clojure mode
; $ git clone https://github.com/clojure-emacs/clojure-mode.git
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)

; Ninja mode
; $ git clone https://github.com/martine/ninja.git
(add-to-list 'load-path "~/.emacs.d/ninja")
(require 'ninja-mode)

; Jade mode
; $ git clone https://github.com/brianc/jade-mode.git
(add-to-list 'load-path "~/.emacs.d/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

; Markdown mode
; $ git clone https://github.com/defunkt/markdown-mode.git
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; Gnuplot mode
; $ git clone https://github.com/mkmcc/gnuplot-mode.git
(add-to-list 'load-path "~/.emacs.d/gnuplot-mode")
(require 'gnuplot-mode)
(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.gnuplot\\'" . gnuplot-mode))

; YASnippet mode
; $ git clone https://github.com/capitaomorte/yasnippet.git
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

; Org mode
; $ git clone https://orgmode.org/org-mode.git
;
; * headline folding -> S-Tab
; [[link][title]] -> C-c C-o
; * TODO -> C-c C-t
;   - [X] -> C-c C-c
; <timestamp> -> C-c . / C-c C-c
; agenda -> C-c a a
;   d/w
;   f/b/.
;   l/r
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(require 'org)
(global-set-key "\C-ca" 'org-agenda)
; org-mode-ident conflict resolution
(add-hook 'org-mode-hook 'visual-line-mode)
; org-ident-mode on
(setq org-startup-indented t)
; TODO DONE timestamp
(setq org-log-done t)
; agenda files directory
(setq org-agenda-files '("~/Dropbox/orgfiles"))

; Evil mode
; $ git clone https://gitorious.org/evil/evil.git
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; Spell mode
; M-x ispell
; M-x ispell-change-dictionary
; M-x ispell-kill-ispell

; C-x RET f unix/utf-8
