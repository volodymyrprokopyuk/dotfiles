;; -*- lexical-binding: t; -*-

(doom!
  :completion
  company
  vertico

  :ui
  doom
  ;; ligatures TODO
  modeline
  ophints
  (popup +defaults)
  (vc-gutter +pretty)

  :editor
  (evil +everywhere)
  fold
  ;; objed TODO

  :emacs
  dired
  undo
  vc

  :checkers
  syntax
  (spell +aspell +everywhere)

  :tools
  lookup
  lsp
  ;; magit TODO
  docker

  :os
  tty

  :lang
  json
  yaml
  markdown
  org
  (go +lsp)
  solidity
  web
  (javascript +lsp)
  (sh +fish)
  emacs-lisp
  lua

  :config
  (default +bindings +smartparens))
