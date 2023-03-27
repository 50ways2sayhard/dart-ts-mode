;;; dart-ts-mode.el --- tree sitter support for Dart  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : John Gong <gjtzone@hotmail.com>
;; Maintainer : John Gong <gjtzone@hotmail.com>
;; Created    : March 2023
;; Keywords   : dart languages tree-sitter
;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))
(require 'c-ts-common) ; For comment indent and filling.

(declare-function treesit-parser-create "treesit.c")

(defcustom dart-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `dart-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'dart)

(defvar dart-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `dart-ts-mode'.")

(defvar dart-ts-mode--indent-rules
  `((dart
     ((parent-is "program") column-0 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((parent-is "(declaration (initializers))") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "class_body") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "formal_parameter_list") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "formal_parameter") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "function_body") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "function_expression_body") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "switch_block") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "list_literal") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "return_statement") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol dart-ts-mode-indent-offset)
     ;; FIXME Don't know why the "function_body" rule doesn't work.
     ((parent-is "block") parent-bol dart-ts-mode-indent-offset)

     (no-node parent-bol 0))))

(defvar dart-ts-mode--keywords
  '("async" "async*" "yield" "sync*"
    "await" "get" "interface" "show"
    "hide" "on" "class" "enum" "extends"
    "in" "is" "new" "return"
    "super" "with" "if" "else"
    "try" "catch" "default" "switch")
  "Dart keywords for tree-sitter font-locking.")

(defvar dart-ts-mode--builtins
  '("abstract" "as" "covariant" "deferred"
    "dynamic" "export" "extension" "external"
    "factory" "Function" "get" "implements"
    "import" "interface" "late" "library"
    "mixin" "operator" "part" "required"
    "set" "static" "typedef")
  "Dart builtins for tree-sitter font locking.")

(defvar dart-ts-mode--operators
  '("=>" ".." "??" "==" "?"
    ":" "&&" "%" "<" ">" "="
    ">=" "<=" "||")
  "Dart operators for tree-sitter font-locking.")

;; TODO font-lock for template_substitution
(defun dart-ts-mode--font-lock-settings (language)
  "Tree-sitter font-lock settings.
Argument LANGUAGE is 'dart'."
  (treesit-font-lock-rules
   :language language
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face
     (documentation_comment) @font-lock-comment-face)

   :language language
   :feature 'constant
   `([(const_builtin)
      (final_builtin)
      (null_literal) (true) (false)] @font-lock-keyword-face)

   :language language
   :feature 'keyword
   `([,@dart-ts-mode--keywords] @font-lock-keyword-face
     [,@dart-ts-mode--builtins] @font-lock-builtin-face
     (break_statement) @font-lock-keyword-face
     [(this) (super) (inferred_type)] @font-lock-keyword-face
     (case_builtin) @font-lock-keyword-face
     ((identifier) @font-lock-type-face
      (:match "^_?[A-Z].*[a-z]" @font-lock-type-face))
     "Function" @font-lock-type-face)

   :language language
   :override t
   :feature 'operator
   `([,@dart-ts-mode--operators
      (multiplicative_operator)
      (increment_operator)
      (is_operator)
      (prefix_operator)
      (equality_operator)
      (additive_operator)] @font-lock-operator-face
      (ternary_expression ["?" ":"] @font-lock-operator-face)
      ;; ((template_substitution
      ;;   "$" @font-lock-punctuation-face
      ;;   "{" @font-lock-punctuation-face
      ;;   "}" @font-lock-punctuation-face))
      ;; (template_substitution
      ;;  "$" @font-lock-punctuation-misc-face
      ;;  (identifier) @font-lock-variable-face)
      ([";" "." ","]) @font-lock-delimiter-face)

   :language language
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language language
   :feature 'string
   `((string_literal) @font-lock-string-face
     (dotted_identifier_list) @font-lock-string-face)

   :language language
   :override t
   :feature 'literal
   `([(hex_integer_literal) (decimal_integer_literal) (decimal_floating_point_literal)] @font-lock-number-face
     (symbol_literal) @font-lock-constant-face
     (string_literal) @font-lock-string-face
     [(true) (false) (null_literal)] @font-lock-constant-face)

   :language language
   :override t
   :feature 'type
   `((class_definition
      name: (identifier) @font-lock-type-face)
     (constructor_signature
      name: (identifier) @font-lock-type-face)
     (scoped_identifier
      scope: (identifier) @font-lock-type-face)
     (function_signature
      name: (identifier) @font-lock-function-name-face)
     (getter_signature
      (identifier) @font-lock-function-name-face)
     (setter_signature
      name: (identifier) @font-lock-function-name-face)
     (enum_declaration
      name: (identifier) @font-lock-type-face)
     (type_identifier) @font-lock-type-face
     (type_alias
      (type_identifier) @font-lock-type-face)
     (void_type) @font-lock-type-face
     ((scoped_identifier
       scope: (identifier) @font-lock-type-face
       name: (identifier) @font-lock-type-face)
      (:match "^[a-zA-Z]" @font-lock-type-face)))

   :language language
   :override t
   :feature 'annotation
   `((annotation
      name: (identifier) @font-lock-constant-face)
     ["@"] @font-lock-constant-face
     (marker_annotation
      name: (identifier) @font-lock-constant-face))

   :language language
   :feature 'method
   `((function_signature
      name: (identifier) @font-lock-function-name-face)
     (setter_signature
      name: (identifier) @font-lock-function-name-face))

   :language language
   :feature 'identifier
   `((type_identifier) @font-lock-type-face)

   :language language
   :feature 'property
   `((unconditional_assignable_selector (identifier) @font-lock-property-face)
     (conditional_assignable_selector
      (identifier) @font-lock-property-face))

   :language language
   :feature 'function
   `((super) @font-lock-function-call-face)

   :language language
   :feature 'number
   `([(hex_integer_literal)
      (decimal_integer_literal)
      (decimal_floating_point_literal)] @font-lock-number-face)

   :language language
   :feature 'delimiter
   `((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language language
   :feature 'escape-sequence
   :override t
   `((escape_sequence) @font-lock-escape-face)))


(defvar dart-ts-mode--sentence-nodes
  '(
    "import_statement"
    "debugger_statement"
    "expression_statement"
    "if_statement"
    "switch_statement"
    )
  "Nodes that designate sentences in Dart.
See `treesit-sentence-type-regexp' for more information.")

(defvar dart-ts-mode--sexp-nodes
  '("expression"
    "pattern"
    "array"
    "function"
    "string"
    "escape"
    "template"
    "regex"
    "number"
    "identifier"
    "this"
    "super"
    "true"
    "false"
    "null"
    "arguments"
    "pair")
  "Nodes that designate sexps in Dart.
See `treesit-sexp-type-regexp' for more information.")

;; TODO function name
(defun dart-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "method_signature"
         "function_signature"
         "class_definition"
         "enum_declaration"
         "import_specification")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name")
      t))))


;;;###autoload
(define-derived-mode dart-ts-mode prog-mode "Dart"
  "Major mode for editing Dart."
  :group 'dart
  :syntax-table dart-ts-mode--syntax-table

  ;; Comments.
  (c-ts-common-comment-setup)
  (setq-local treesit-defun-prefer-top-level t)

  (setq-local treesit-text-type-regexp
              (regexp-opt '("comment"
                            "template_string")))

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))
  (setq-local electric-layout-rules
	            '((?\; . after) (?\{ . after) (?\} . before)))
  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("class_definition"
                            "function_signature"
                            "enum_declaration")))

  (setq-local treesit-sentence-type-regexp
              (regexp-opt dart-ts-mode--sentence-nodes))

  ;; TODO sexp
  (setq-local treesit-sexp-type-regexp
              (regexp-opt dart-ts-mode--sexp-nodes))

  (setq-local treesit-simple-imenu-settings
              '(("Class" "\\`class_definition\\'" nil nil)
                ("Enum" "\\`enum_declaration\\'" nil nil)
                ("Method" "\\`function_signature\\'" nil nil)))
  (setq-local treesit-defun-name-function #'dart-ts-mode--defun-name)
  (when (treesit-ready-p 'dart)
    (treesit-parser-create 'dart)

    ;; Indent.
    (setq-local treesit-simple-indent-rules dart-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                (dart-ts-mode--font-lock-settings 'dart))
    ;; FIXME More reasonable feature list.
    (setq-local treesit-font-lock-feature-list
                '((comment escape-sequence)
                  (constant keyword string type assignment definition)
                  (annotation expression literal property)
                  (bracket delimiter operator number)))

    (treesit-major-mode-setup))
  )

(if (treesit-ready-p 'dart)
    (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-ts-mode)))

(provide 'dart-ts-mode)

;;; dart-ts-mode.el ends here
