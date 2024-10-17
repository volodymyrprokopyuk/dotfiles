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
  ;; fold
  ;; objed TODO
  snippets

  :emacs
  ;; dired
  undo
  vc

  :checkers
  syntax

  :tools
  lookup
  lsp
  magit
  docker

  :os
  tty

  :lang
  json
  yaml
  markdown
  org
  web
  (sh +fish)
  (go +lsp)
  (javascript +lsp)
  solidity
  emacs-lisp
  lua

  :config
  (default +bindings +smartparens))
