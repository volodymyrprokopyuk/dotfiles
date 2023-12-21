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
       tree-sitter

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
       (javascript +lsp +tree-sitter)
       (go +lsp +tree-sitter)
       solidity
       (sh +fish)
       emacs-lisp
       lua

       :config
       (default +bindings +smartparens))
