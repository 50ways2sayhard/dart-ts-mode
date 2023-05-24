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

(defgroup dart-ts nil
  "Major mode for editing Dart code."
  :prefix "dart-ts-"
  :group 'languages)

(defcustom dart-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `dart-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'dart-ts)

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
     ((match "}" "class_body") column-0 0)
     ((match "}" "optional_formal_parameters") standalone-parent 0)
     ((n-p-gp "}" "block" "if_statement") dart-ts-mode--if-statement-indent-rule 0)
     ((match "}" "block") dart-ts-mode--function-body-indent-rule 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((parent-is "(declaration (initializers))") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "class_body") column-0 dart-ts-mode-indent-offset)
     ((parent-is "enum_body") column-0 dart-ts-mode-indent-offset)
     ((parent-is "extension_body") column-0 dart-ts-mode-indent-offset)
     ((parent-is "formal_parameter_list") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "optional_formal_parameters") dart-ts-mode--optional-formal-parameters-indent-rule 0)
     ;; ((parent-is "formal_parameter") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "function_expression_body") parent-bol dart-ts-mode-indent-offset)
     ((n-p-gp nil "switch_block" "switch_statement") dart-ts-mode--switch-case-indent-rule dart-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "list_literal") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "set_or_map_literal") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "return_statement") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "arguments") dart-ts-mode--arguments-indent-rule 0)
     ((n-p-gp nil "block" "function_body") dart-ts-mode--function-body-indent-rule dart-ts-mode-indent-offset)
     ((n-p-gp nil "block" "if_statement") dart-ts-mode--if-statement-indent-rule dart-ts-mode-indent-offset)
     ((parent-is "block") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol dart-ts-mode-indent-offset)

     (no-node parent-bol 0))))

(defun dart-ts-mode--node-start (node)
  "Return the NODE's start position."
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))

(defun dart-ts-mode--parent-start (node)
  "Return the position of the first character of NODE's parent."
  (dart-ts-mode--node-start (treesit-node-parent node)))

(defun dart-ts-mode--if-statement-indent-rule (_ parent &rest __)
  "Indent rule for if_statement.
If parent of PARENT (a.k.a grandparent) is if_statement,
returns parent-bol of grandparent.Otherwise returns bol of grandparent."
  (let ((gp (treesit-node-parent parent)))
    (if (and (treesit-node-p gp)
             (string= "if_statement" (treesit-node-string gp)))
        (dart-ts-mode--parent-start gp)
      (dart-ts-mode--node-start gp))))

(defun dart-ts-mode--switch-case-indent-rule (node parent &rest __)
  "Indent rule for a NODE under switch_block.
If NODE is switch's label, returns PARENT's start position.
Otherwise returns PARENT's start position plus
`dart-ts-mode-indent-offset'."
  (let ((parent-start (dart-ts-mode--node-start parent))
        (node-name (treesit-node-type node)))
    (if (or (string= "switch_label" node-name)
            (string= "switch_statement_case" node-name)
            (string= "switch_statement_default" node-name))
        parent-start
      (+ parent-start dart-ts-mode-indent-offset))))

(defun dart-ts-mode--arguments-indent-rule (node parent &rest _)
  "Return indentation of argument list.
If NODE is the first sibling of PARENT, returns bol of parent, or else returns
starting point of first sibling."
  (let ((first-sibling (treesit-node-child parent 0 t)))
    (if (and first-sibling (not (treesit-node-eq first-sibling node)))
        (treesit-node-start first-sibling)
      (+ (dart-ts-mode--node-start parent) dart-ts-mode-indent-offset))))

(defun dart-ts-mode--function-body-indent-rule (_ parent &rest _)
  "Return indentation of function_body.PARENT is always a block node."
  (let ((maybe-signature (treesit-node-prev-sibling (treesit-node-parent parent))))
    (if (and maybe-signature
             (or (string-match-p "function_signature" (treesit-node-string maybe-signature))
                 (string-match-p "method_signature" (treesit-node-string maybe-signature))))
        (dart-ts-mode--node-start maybe-signature)
      (dart-ts-mode--node-start parent))))

(defun dart-ts-mode--optional-formal-parameters-indent-rule (_ parent &rest __)
  "Return indentation of children of optional_formal_parameters.
PARENT is always optional_formal_parameters."
  (let ((parent-sibling (treesit-node-prev-sibling parent)))
    (if (and (treesit-node-p parent-sibling)
             (string= (treesit-node-string parent-sibling) "formal_parameter"))
        (treesit-node-start parent-sibling)
      (+ (dart-ts-mode--parent-start parent) dart-ts-mode-indent-offset))))

(defvar dart-ts-mode--keywords
  '("async" "async*" "await" "catch" "class"
    "default" "else" "enum" "extends" "get" "hide"
    "if" "in" "interface" "is" "new" "on" "return"
    "show" "super" "switch" "sync*" "this"
    "try" "when" "with" "yield")
  "Dart keywords for tree-sitter font-locking.")

(defvar dart-ts-mode--builtins
  '("abstract" "as" "base" "covariant" "deferred"
    "dynamic" "export" "extension" "external"
    "factory" "Function" "get" "implements"
    "import" "interface" "late" "library"
    "mixin" "operator" "part" "required"
    "sealed" "set" "static" "typedef")
  "Dart builtins for tree-sitter font locking.")

(defvar dart-ts-mode--operators
  '("=>" ".." "??" "==" "?"
    ":" "&&" "%" "<" ">" "="
    ">=" "<=" "||")
  "Dart operators for tree-sitter font-locking.")

(defvar dart-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'dart
   :feature 'comment
   '((comment) @font-lock-comment-face
     (documentation_comment) @font-lock-comment-face)

   :language 'dart
   :feature 'definition
   :override t
   '((class_definition
      name: (identifier) @font-lock-type-face)
     (object_pattern
      ((identifier) @font-lock-variable-name-face))
     (constant_pattern
      (identifier) @font-lock-variable-name-face)
     (variable_pattern
      (identifier) @font-lock-variable-name-face)
     (record_field
      (label (identifier) @font-lock-variable-name-face))
     (relational_expression
      (identifier) @font-lock-variable-name-face)
     (switch_statement_case
      (identifier) @font-lock-variable-name-face)
     (initialized_identifier
      (identifier) @font-lock-variable-name-face)
     (initialized_variable_definition
      name: (identifier) @font-lock-variable-name-face)
     (static_final_declaration
      (identifier) @font-lock-variable-name-face)
     (constant_constructor_signature
      (identifier) @font-lock-function-name-face)
     (constructor_signature
      name: (identifier) @font-lock-function-name-face)
     (function_signature
      name: (identifier) @font-lock-function-name-face)
     (getter_signature
      name: (identifier) @font-lock-function-name-face)
     (setter_signature
      name: (identifier) @font-lock-function-name-face))

   :language 'dart
   :feature 'keyword
   `([,@dart-ts-mode--keywords] @font-lock-keyword-face
     [(break_statement) (continue_statement)] @font-lock-keyword-face
     [,@dart-ts-mode--builtins] @font-lock-builtin-face
     [(const_builtin) (final_builtin) (case_builtin)] @font-lock-builtin-face
     ((identifier) @font-lock-type-face
      (:match "^_?[A-Z].*[a-z]" @font-lock-type-face))
     ((identifier) @font-lock-keyword-face
      (:match "^rethrow" @font-lock-keyword-face))
     (yield_each_statement
      "yield" @font-lock-keyword-face
      "*" @font-lock-keyword-face)
     (throw_expression "throw" @font-lock-keyword-face)
     (while_statement "while" @font-lock-keyword-face)
     (for_statement "for" @font-lock-keyword-face))

   :language 'dart
   :feature 'string
   :override t
   '((string_literal) @font-lock-string-face
     (template_substitution) @font-lock-variable-name-face
     (dotted_identifier_list) @font-lock-string-face)

   :language 'dart
   :feature 'type
   :override t
   '((type_identifier) @font-lock-type-face
     [(inferred_type)
      (void_type)
      (nullable_type)
      (function_type)] @font-lock-type-face
     (enum_declaration
      name: (identifier) @font-lock-type-face)
     (scoped_identifier
      scope: (identifier) @font-lock-type-face)
     (type_alias
      (type_identifier) @font-lock-type-face)
     ((scoped_identifier
       scope: (identifier) @font-lock-type-face
       name: (identifier) @font-lock-type-face)
      (:match "^[a-zA-Z]" @font-lock-type-face)))

   :language 'dart
   :feature 'assignment
   `((assignment_expression
      left: (assignable_expression
             (identifier) @font-lock-variable-use-face)))

   :language 'dart
   :feature 'constant
   '([(true) (false)] @font-lock-constant-face)

   :language 'dart
   :feature 'number
   '([(hex_integer_literal)
      (decimal_integer_literal)
      (decimal_floating_point_literal)] @font-lock-number-face)

   :language 'dart
   :feature 'literal
   :override t
   '([(null_literal) (symbol_literal)] @font-lock-constant-face)

   :language 'dart
   :feature 'annotation
   :override t
   '((annotation
      "@" @font-lock-constant-face
      name: (identifier) @font-lock-constant-face)
     (marker_annotation) @font-lock-constant-face)

   :language 'dart
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'dart
   :feature 'property
   `((cascade_selector
      (identifier) @font-lock-property-name-face)
     (conditional_assignable_selector
      (identifier) @font-lock-property-name-face)
     (qualified
      (identifier) @font-lock-property-name-face)
     (unconditional_assignable_selector
      (identifier) @font-lock-property-name-face))

   :language 'dart
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language 'dart
   :feature 'operator
   `([,@dart-ts-mode--operators] @font-lock-operator-face
     [(multiplicative_operator)
      (increment_operator)
      (is_operator)
      (prefix_operator)
      (equality_operator)
      (additive_operator)] @font-lock-operator-face
     (ternary_expression ["?" ":"] @font-lock-operator-face))

   :language 'dart
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'dart
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `dart-ts-mode'.")

(defvar dart-ts-mode--sentence-nodes
  '(
    "assert_statement"
    "debugger_statement"
    "expression_statement"
    "formal_parameter"
    "if_statement"
    "import_statement"
    "optional_formal_parameters"
    "switch_statement"
    "variable_declaration"
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
    ((or "function_signature"
         "method_signature"
         "setter_signature"
         "getter_signature"
         "class_definition"
         "enum_declaration")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name")
      t))))

;;;###autoload
(define-derived-mode dart-ts-mode prog-mode "Dart"
  "Major mode for editing Dart, powered by tree-sitter."
  :group 'dart-ts
  :syntax-table dart-ts-mode--syntax-table

  ;; Comments.
  (c-ts-common-comment-setup)

  ;; Compile.
  (setq-local compile-command "dart")

  ;; Electric pair.
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  (setq-local electric-layout-rules
              '((?\; . after) (?\{ . after) (?\} . before)))

  (when (treesit-ready-p 'dart)
    (treesit-parser-create 'dart)

    (setq-local treesit-defun-prefer-top-level t)

    (setq-local treesit-defun-type-regexp
                (regexp-opt '("class_definition"
                              "function_signature"
                              "getter_signature"
                              "setter_signature"
                              "constructor_signature"
                              "constant_constructor_signature"
                              "enum_declaration")))

    (setq-local treesit-text-type-regexp
                (regexp-opt '("comment"
                              "template_string")))

    (setq-local treesit-sentence-type-regexp
                (regexp-opt dart-ts-mode--sentence-nodes))

    (setq-local treesit-sexp-type-regexp
                (regexp-opt dart-ts-mode--sexp-nodes))

    (setq-local treesit-simple-imenu-settings
                '(("Class" "\\`class_definition\\'" nil nil)
                  ("Enum" "\\`enum_declaration\\'" nil nil)
                  ("Method" "\\`function_signature\\'" nil nil)))

    (setq-local treesit-defun-name-function #'dart-ts-mode--defun-name)

    ;; Indent.
    (setq-local treesit-simple-indent-rules dart-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings dart-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( assignment constant number literal
                    annotation escape-sequence property)
                  ( delimiter operator bracket error)))

    (treesit-major-mode-setup))
  )

(if (treesit-ready-p 'dart)
    (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-ts-mode)))

(provide 'dart-ts-mode)

;;; dart-ts-mode.el ends here
