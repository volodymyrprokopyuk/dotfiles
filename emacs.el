; emacs configuration
; in ~/.emacs:
;   ; volodymir's configuration
;   (load "~/.emacs.d/emacs.el")

; terminal copy/paste to/from clipboard
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
; Solarized dark
; $ git clone https://github.com/sellout/emacs-color-theme-solarized.git
;(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
;(load-theme 'solarized-dark t)
; Solarized light
; $ git clone git://github.com/bbatsov/solarized-emacs.git
;(add-to-list 'load-path "~/.emacs.d/solarized-emacs")
;(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs")
;(load-theme 'solarized-light t)
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

; RecentFiles mode
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

; JS2 mode
; $ git clone git://github.com/mooz/js2-mode.git
; $ make
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

; Tern mode
; $ git clone https://github.com/marijnh/tern.git
; $ npm install/update
(add-to-list 'load-path "~/.emacs.d/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
; Tern completion
(global-set-key (kbd "M-/") 'complete-symbol)

; Haskell mode
; $ git clone https://github.com/haskell/haskell-mode.git
; $ make
(add-to-list 'load-path "~/.emacs.d/haskell-mode")
(require 'haskell-mode-autoloads)
; haskell indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq haskell-indent-offset 2)
; Shake mode
(add-to-list 'auto-mode-alist '("build\\'" . haskell-mode))

; PostgreSQL mode
(add-to-list 'auto-mode-alist '("\\.sql\\'" . (lambda () (sql-mode)
  (sql-highlight-postgres-keywords))))

; Julia mode
(add-to-list 'load-path "~/.emacs.d/julia")
(require 'julia-mode)

; TeX mode
; M-q (format selection)
(setq-default fill-column 80)
; TeX completion
(add-hook 'LaTeX-mode-hook (lambda()
  (local-set-key (kbd "M-/") 'TeX-complete-symbol)))

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

; XML format
; $ sudo apt-get install libxml2-utils
; M-x xml-format RET
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -"
      (buffer-name) t)))

; Org mode
; $ git clone git://orgmode.org/org-mode.git
; $ make autoloads
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
; $ make
; u (undo)
; 0 u (redo)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; Spell mode
; M-x ispell
; M-x ispell-change-dictionary
; M-x ispell-kill-ispell

; C-x RET f unix/utf-8
; C-x 8 RET 20AC RET
; M-x toggle-read-only
