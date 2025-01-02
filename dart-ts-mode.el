;;; dart-ts-mode.el --- tree sitter support for Dart  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : John Gong <gjtzone@hotmail.com>
;; Maintainer : John Gong <gjtzone@hotmail.com>
;; Created    : March 2023
;; Keywords   : dart languages tree-sitter

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

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
     ((parent-is "record_literal") dart-ts-mode--arguments-indent-rule 0)
     ;; ((parent-is "formal_parameter") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "function_expression_body") parent-bol dart-ts-mode-indent-offset)
     ((n-p-gp nil "switch_block" "switch_statement") dart-ts-mode--switch-case-indent-rule dart-ts-mode-indent-offset)
     ((parent-is "switch_expression") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "if_element") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "for_element") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "list_literal") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "set_or_map_literal") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "return_statement") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "arguments") dart-ts-mode--arguments-indent-rule 0)
     ((n-p-gp nil "block" "function_body") dart-ts-mode--function-body-indent-rule dart-ts-mode-indent-offset)
     ((n-p-gp nil "block" "function_expression_body") dart-ts-mode--function-expression-body-indent-rule dart-ts-mode-indent-offset)
     ((n-p-gp nil "block" "if_statement") dart-ts-mode--if-statement-indent-rule dart-ts-mode-indent-offset)
     ((parent-is "block") parent-bol dart-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol dart-ts-mode-indent-offset)
     ((node-is "'''") parent-bol 0)
     ((node-is "cascade_section") parent-bol dart-ts-mode-indent-offset)

     (no-node parent-bol 0))))

(defun dart-ts-mode--node-bol (node)
  "Return NODE's bol position."
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))

(defun dart-ts-mode--parent-start (node)
  "Return the position of NODE's parent."
  (treesit-node-start (treesit-node-parent node)))

(defun dart-ts-mode--if-statement-indent-rule (_node parent &rest _)
  "Indent rule for if_statement.
If parent of PARENT (a.k.a grandparent) is if_statement, returns
parent of grandparent.  Otherwise returns bol of grandparent."
  (let ((gp (treesit-node-parent parent)))
    (if (string= "if_statement" (treesit-node-string gp))
        (dart-ts-mode--parent-start gp)
      (dart-ts-mode--node-bol gp))))

(defun dart-ts-mode--switch-case-indent-rule (node parent &rest _)
  "Indent rule for a NODE under switch_block.
If NODE is switch's label, returns NODE's parent-bol.  Otherwise
returns parent-bol plus `dart-ts-mode-indent-offset'."
  (let ((node-type (treesit-node-type node))
        (parent-bol (dart-ts-mode--node-bol parent)))
    (if (or (string= "switch_statement_case" node-type)
            (string= "switch_statement_default" node-type)
            (string= "switch_label" node-type))
        parent-bol
      (+ parent-bol dart-ts-mode-indent-offset))))

(defun dart-ts-mode--arguments-indent-rule (node parent &rest _)
  "Return indentation of argument list.
If NODE is the first sibling of PARENT, returns bol of parent, or else returns
starting point of first sibling."
  (let ((first-sibling (treesit-node-child parent 0 t)))
    (if (and first-sibling (not (treesit-node-eq first-sibling node)))
        (treesit-node-start first-sibling)
      (+ (dart-ts-mode--node-bol parent) dart-ts-mode-indent-offset))))

(defun dart-ts-mode--function-body-indent-rule (_node parent &rest _)
  "Indent rule for NODE inside a named function body.
PARENT is alway block here.  If the previous sibling of NODE's
grandparent is a signature, return signature's start position.
Otherwise return the indentation of NODE's PARENT, which is always a block
node."
  (let* ((gp (treesit-node-parent parent))
         (gp-ps (treesit-node-prev-sibling gp))
         (gp-ps-name (treesit-node-type gp-ps)))
    (cond
     ((string-match-p "\\(function\\|method\\)_signature" gp-ps-name)
      (treesit-node-start gp-ps))
     (t (dart-ts-mode--node-bol parent)))))

(defun dart-ts-mode--function-expression-body-indent-rule (_node parent &rest _)
  "Indent rule for NODE inside the body of an anonymous function expression.
PARENT is alway block here.It first gets the grandparent (gp)
and great-grandparent (ggp) nodes of NODE.

It then checks if ggp matches one of the following node types:

    argument
    parenthesized_expression
    return_statement
    record_field
    named_argument

If there is a match, it returns the beginning of line indentation of PARENT.
This indents the function body to the same column as the argument/expression
it is inside of.

If there is no match, it returns the start position of ggp."
  (let* ((gp (treesit-node-parent parent))
         (ggp (treesit-node-parent gp)))
    (if (treesit-node-match-p
         (treesit-node-parent ggp)
         (rx (or "argument" "parenthesized_expression"
                 "return_statement" "record_field" "named_argument" "assignment_expression")))
        (dart-ts-mode--node-bol (treesit-node-parent ggp))
      (treesit-node-start ggp))))

(defun dart-ts-mode--optional-formal-parameters-indent-rule (_node parent &rest _)
  "Return indentation of children of optional_formal_parameters.
PARENT is always optional_formal_parameters."
  (if-let ((formal-sib (treesit-node-prev-sibling parent "formal_parameter")))
      (treesit-node-start formal-sib)
    (+ (dart-ts-mode--node-bol parent) dart-ts-mode-indent-offset)))

(defvar dart-ts-mode--keywords
  '("async" "async*" "await" "catch" "class"
    "default" "else" "enum" "extends" "get" "hide"
    "if" "in" "is" "new" "on" "return"
    "show" "super" "switch" "sync*" "this"
    "try" "when" "with" "yield")
  "Dart keywords for tree-sitter font-locking.")

(defvar dart-ts-mode--builtins
  '("abstract" "as" "base" "covariant" "deferred"
    "dynamic" "export" "extension" "external"
    "factory" "get" "implements"
    "import" "interface" "late" "library"
    "mixin" "operator" "part" "required"
    "sealed" "set" "static" "typedef")
  "Dart builtins for tree-sitter font locking.")

(defvar dart-ts-mode--operators
  '("=>" ".." "??" "==" "?"
    ":" "&&" "%" "<" ">" "="
    ">=" "<=" "||" ">>>="
    ">>=" "<<=" "&=" "|=" "??="
    "%=" "+=" "-=" "*=" "/="
    "^=" "~/=")
  "Dart operators for tree-sitter font-locking.")

(defvar dart-ts-mode--imenu-settings
  '(("Class" "\\`class_definition\\'")
    ("Enum" "\\`enum_declaration\\'")
    ("Method" "\\`function_signature\\'")
    ("Mixin" "\\`mixin_declaration\\'" nil dart-ts-mode--mixin-name))
  "The value for `treesit-simple-imenu-settings'.
By default `treesit-defun-name-function' is used to extract
definition names.")

;;;; Things.

(defvar dart-ts-mode--sentence-nodes
  '("assert_statement"
    "debugger_statement"
    "expression_statement"
    "formal_parameter"
    "if_statement"
    "import_statement"
    "optional_formal_parameters"
    "switch_statement"
    "variable_declaration")
  "Nodes that designate sentences in Dart.")

(defvar dart-ts-mode--text-nodes
  '("comment" "string_literal")
  "Nodes that designate texts in Dart.")

;;;; Font-lock.

(defvar dart-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'dart
   :feature 'comment
   '((comment) @font-lock-comment-face
     (documentation_comment) @font-lock-comment-face)

   :language 'dart
   :feature 'keyword
   :override t
   `([,@dart-ts-mode--keywords] @font-lock-keyword-face
     [,@dart-ts-mode--builtins] @font-lock-builtin-face
     [(break_statement) (continue_statement)] @font-lock-keyword-face
     [(const_builtin) (final_builtin) (case_builtin)] @font-lock-keyword-face
     [(super) (this)] @font-lock-keyword-face
     ((identifier) @font-lock-keyword-face
      (:match "^rethrow" @font-lock-keyword-face))
     (for_statement "for" @font-lock-keyword-face)
     (finally_clause "finally" @font-lock-keyword-face)
     (part_of_directive (part_of_builtin) @font-lock-builtin-face)
     (function_type "Function" @font-lock-type-face)
     (throw_expression "throw" @font-lock-keyword-face)
     (while_statement "while" @font-lock-keyword-face)
     (yield_each_statement
      "yield" @font-lock-keyword-face
      "*" @font-lock-keyword-face))

   :language 'dart
   :feature 'definition
   '((class_definition
      name: (identifier) @font-lock-type-face)
     (class_definition
      "final" @font-lock-keyword-face)
     ((identifier) @font-lock-type-face
      (:match "\\`_?[A-Z]" @font-lock-type-face))
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
     (combinator
      (identifier) @font-lock-variable-use-face)
     (import_specification
      (identifier) @font-lock-variable-use-face)
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
      name: (identifier) @font-lock-function-name-face)
     (formal_parameter
      name: (identifier) @font-lock-variable-name-face))

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
      (nullable_type)] @font-lock-type-face
     (enum_declaration
      name: (identifier) @font-lock-type-face)
     (scoped_identifier
      scope: (identifier) @font-lock-type-face
      name: (identifier) @font-lock-type-face)
     (type_alias
      (type_identifier) @font-lock-type-face)
     ((scoped_identifier
       scope: (identifier) @font-lock-type-face
       name: (identifier) @font-lock-type-face)
      (:match "^[a-zA-Z]" @font-lock-type-face)))

   :language 'dart
   :feature 'assignment
   '((assignment_expression
      left: (assignable_expression
             (identifier) @font-lock-variable-use-face)))

   :language 'dart
   :feature 'constant
   '([(true) (false)] @font-lock-constant-face)

   :language 'dart
   :feature 'number
   '([(hex_integer_literal)
      (decimal_integer_literal)
      (decimal_floating_point_literal)] @font-lock-constant-face)

   :language 'dart
   :feature 'literal
   :override t
   '([(null_literal) (symbol_literal)] @font-lock-constant-face)

   :language 'dart
   :feature 'annotation
   :override t
   '((annotation
      "@" @font-lock-constant-face
      name: (identifier) @font-lock-constant-face))

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
      (identifier) @font-lock-property-name-face)
     )

   :language 'dart
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language 'dart
   :feature 'operator
   `([,@dart-ts-mode--operators] @font-lock-operator-face
     [(multiplicative_operator)
      (shift_operator)
      (increment_operator)
      (is_operator)
      (prefix_operator)
      (equality_operator)
      (additive_operator)] @font-lock-operator-face)

   :language 'dart
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'dart
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `dart-ts-mode'.")

(defun dart-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or NODE is not a defun node."
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

(defun dart-ts-mode--mixin-name (node)
  "Return the name of mixin NODE.
Return nil if there is no name or NODE."
  (when (string-equal "mixin_declaration" (treesit-node-type node))
    (treesit-node-text (treesit-node-child node 1) t)))

(defun dart-ts-mode--electric-pair-string-delimiter ()
  "Insert corresponding multi-line string for `electric-pair-mode'."
  (when (and electric-pair-mode
             (memq last-command-event '(?\" ?\'))
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion (insert (make-string 2 last-command-event)))))

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

  ;; Add """ ... """ pairing to `electric-pair-mode'.
  (add-hook 'post-self-insert-hook
            #'dart-ts-mode--electric-pair-string-delimiter 'append t)

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

    (setq-local treesit-defun-name-function #'dart-ts-mode--defun-name)

    (if (boundp 'treesit-thing-settings)
        ;; Emacs 30+.
        (setq-local treesit-thing-settings
                    `((dart
                       ;; It's more useful to include semicolons as sexp so
                       ;; that users can move to the end of a statement.
                       (sexp (not ,(rx (or "{" "}" "[" "]" "(" ")" ","))))
                       (sentence ,(regexp-opt dart-ts-mode--sentence-nodes))
                       (text ,(regexp-opt dart-ts-mode--text-nodes)))))
      ;; (setq-local treesit-sexp-type-regexp
      ;;             (rx bol
      ;;                 (or "block" "body" "identifier" "annotation"
      ;;                     "_expression" "expression_statement"
      ;;                     "true" "false" "this" "super" "null")
      ;;                 eol))
      (setq-local treesit-sentence-type-regexp
                  (regexp-opt dart-ts-mode--sentence-nodes))
      (setq-local treesit-text-type-regexp
                  (regexp-opt dart-ts-mode--text-nodes)))

    (setq-local treesit-simple-imenu-settings dart-ts-mode--imenu-settings)

    ;; Indent.
    (setq-local treesit-simple-indent-rules dart-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings dart-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment keyword)
                  ( definition string type)
                  ( assignment constant number literal
                    annotation escape-sequence property)
                  ( delimiter operator bracket error)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'dart)
    (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-ts-mode)))

(provide 'dart-ts-mode)

;;; dart-ts-mode.el ends here
