;; -*- lexical-binding: t; -*-

(doom! :completion
       company
       vertico

       :ui
       doom
       modeline
       ophints
       (popup +defaults)
       vc-gutter
       treemacs

       :editor
       (evil +everywhere)

       :emacs
       undo
       vc

       :tools
       lookup
       docker

       :lang
       json
       yaml
       markdown
       sh
       nim
       javascript
       web
       emacs-lisp
       lua

       :config
       (default +bindings +smartparens))
