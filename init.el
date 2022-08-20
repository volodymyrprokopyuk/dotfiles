;; -*- lexical-binding: t; -*-

(doom! :completion
       company
       vertico

       :ui
       doom
       ligatures
       modeline
       ophints
       (popup +defaults)
       vc-gutter

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

       :config
       (default +bindings +smartparens))
