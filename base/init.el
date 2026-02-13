;; -*- lexical-binding: t; -*-

(doom!
  :completion
  (corfu +orderless)
  (vertico +icons)

  :ui
  doom
  (ligatures +extra)
  (smartparens +overlay)
  (indent-guides)
  modeline
  ophints
  (popup +defaults)
  (vc-gutter +pretty)

  :editor
  (evil +everywhere)
  fold
  snippets
  (whitespace +trim)

  :emacs
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
  (javascript)
  solidity
  emacs-lisp
  lua

  :config
  (default +bindings +snippets))
