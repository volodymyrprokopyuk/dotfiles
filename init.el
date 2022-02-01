;; -*- lexical-binding: t; -*-

(doom! :input

       :completion
       company
       vertico

       :ui
       doom
       modeline
       ophints
       (popup +defaults)
       vc-gutter

       :editor
       (evil +everywhere)

       :emacs
       undo
       vc

       :term

       :checkers

       :tools
       lookup
       docker

       :os

       :lang
       json
       yaml
       markdown
       emacs-lisp
       sh
       nim

       :email

       :app

       :config
       (default +bindings +smartparens))
