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
       (sh +fish)
       emacs-lisp
       lua

       :config
       (default +bindings +smartparens))
