;; -*- lexical-binding: t; -*-

(doom! :os
       tty

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

       :completion
       company
       vertico

       :checkers
       syntax
       (spell +aspell +everywhere)

       :lang
       json
       yaml
       markdown
       web
       (javascript +lsp)
       (go +lsp)
       solidity
       (sh +fish)
       emacs-lisp
       lua

       :config
       (default +bindings +smartparens))
