;; -*- lexical-binding: t; -*-

(doom!
  :completion
  (corfu +orderless +icons)
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
