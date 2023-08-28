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
       (vc-gutter +pretty)

       :editor
       (evil +everywhere)
       fold
       objed

       :emacs
       undo
       vc

       :tools
       lookup
       lsp

       :lang
       json
       yaml
       markdown
       web
       (javascript +lsp)
       solidity
       emacs-lisp
       sh
       lua

       :config
       (default +bindings +smartparens))
