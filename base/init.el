;; -*- lexical-binding: t; -*-

(doom!
  :completion
  (corfu +orderless +icons)
  (vertico +icons)

  :ui
  doom
  (ligatures +extra)
  (smartparens +overlay)
  indent-guides
  (vc-gutter +pretty)
  modeline
  ophints
  (popup +defaults)

  :editor
  (evil +everywhere)
  (whitespace +trim)
  fold
  snippets

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
  web
  (sh +fish)
  (go +lsp)
  (javascript)
  emacs-lisp
  lua

  :config
  (default +bindings +snippets))
