;; -*- lexical-binding: t; -*-

(doom! :os
       tty

       :completion
       company
       vertico

       :ui
       doom
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
       javascript
       web
       solidity
       emacs-lisp
       lua

       :config
       (default +bindings +smartparens))
