;; -*- lexical-binding: t; -*-

(doom!
  :completion
  company
  ;; (corfu +orderless)
  vertico

  :ui
  doom
  ligatures
  modeline
  ophints
  (popup +defaults)
  (vc-gutter +pretty)

  :editor
  (evil +everywhere)
  fold
  snippets
  ;; (whitespace +guess +trim)

  :emacs
  ;; dired
  undo
  vc

  :checkers
  syntax

  :tools
  lookup
  lsp
  docker

  :os
  tty

  :lang
  json
  yaml
  markdown
  org
  graphviz
  web
  (sh +fish)
  (go +lsp)
  (javascript +lsp)
  solidity
  emacs-lisp
  lua

  :config
  (default +bindings +smartparens))
