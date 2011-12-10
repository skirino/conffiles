;;; genie.el --- silly walks for Genie  -*- coding: iso-8859-1 -*-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Genie, with support for inferior processes.

(eval-when-compile
  (require 'comint)
  (autoload 'info-lookup-maybe-add-help "info-look"))
(eval-and-compile (require 'compile))	; avoid warning

(require 'sym-comp)
(autoload 'comint-mode "comint")

(defgroup genie nil
  "Silly walks in the Genie language."
  :group 'languages
  :link '(emacs-commentary-link "genie"))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("genie" . genie-mode))
(add-to-list 'auto-mode-alist '("\\.gs\\'" . genie-mode))
(add-to-list 'same-window-buffer-names "*Genie*")

;;;; Font lock

(defvar genie-font-lock-keywords
  `(,(rx symbol-start
	 (or "void" "char" "int" "float" "double" "string"
	     "class" "interface" "struct" "enum" "signal"
	     "public" "partial" "private" "const" "abstract"
	     "protected" "ref" "in" "out" "static" "virtual"
	     "override" "params" "internal" "weak" "owned"
	     "unowned" "is" "as"
	     "delegate" "event" "set" "get" "add" "remove"
	     "callback" "var" "default"
	     "using" "namespace" "construct"
	     "for" "if" "switch" "while" "catch" "foreach" "lock"
	     "do" "return" "continue" "break" "throw"
	     "true" "false" "null" "this" "base"
	     "in" "sizeof" "typeof" "init" "prop")
	 symbol-end)
    (,(rx symbol-start "None" symbol-end)	; see § Keywords in 2.5 manual
     . font-lock-constant-face)
    ;; Definitions
    (,(rx symbol-start (group "class") (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))
    (,(rx symbol-start (group "def") (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ;; Top-level assignments are worth highlighting.
    (,(rx line-start (group (1+ (or word ?_))) (0+ space)
          ;; `augmented'
          (opt (or "+" "-" "*" "/" "//" "%" "**" ">>" "<<" "&" "^" "|"))
          "=")
     (1 font-lock-variable-name-face))
    ;; decorators
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_ ?.))))
     (1 font-lock-type-face))
    ;; Built-ins.  (The next three blocks are from
    ;; `__builtin__.__dict__.keys()' in Genie 2.5.1.  Now modified
    ;; for Genie 2/3 compatibility.)  These patterns are debateable,
    ;; but they at least help to spot possible shadowing of builtins.
    (,(rx symbol-start (or
	  ;; exceptions
	  "ArithmeticError" "AssertionError" "AttributeError"
	  "BaseException" "DeprecationWarning" "EOFError"
	  "EnvironmentError" "Exception" "FloatingPointError"
	  "FutureWarning" "GeneratorExit" "IOError" "ImportError"
	  "ImportWarning" "IndentationError" "IndexError" "KeyError"
	  "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
	  "NotImplemented" "NotImplementedError" "OSError"
	  "OverflowError" "PendingDeprecationWarning" "ReferenceError"
	  "RuntimeError" "RuntimeWarning" "StandardError"
	  "StopIteration" "SyntaxError" "SyntaxWarning" "SystemError"
	  "SystemExit" "TabError" "TypeError" "UnboundLocalError"
	  "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeError"
	  "UnicodeTranslateError" "UnicodeWarning" "UserWarning"
	  "ValueError" "Warning" "ZeroDivisionError") symbol-end)
     . font-lock-type-face)
    (,(rx (or line-start (not (any ". \t"))) (* (any " \t")) symbol-start
	  (group (or
	  ;; callable built-ins, fontified when not appearing as
	  ;; object attributes
	  "abs" "all" "any" "bool"
	  "chr" "classmethod" "cmp" "compile" "complex"
	  "copyright" "credits" "delattr" "dict" "dir" "divmod"
	  "enumerate" "eval" "exit" "filter" "float"
	  "frozenset" "getattr" "globals" "hasattr" "hash" "help"
	  "hex" "id" "input" "int" "isinstance" "issubclass"
	  "iter" "len" "license" "list" "locals" "map" "max"
	  "min" "object" "oct" "open" "ord" "pow" "property" "quit"
	  "range" "repr" "reversed"
	  "round" "set" "setattr" "slice" "sorted" "staticmethod"
	  "str" "sum" "super" "tuple" "type" "vars"
	  "xrange" "zip")) symbol-end)
     (1 font-lock-builtin-face))
    (,(rx symbol-start (or
	  ;; other built-ins
	  "True" "False" "Ellipsis"
	  "_" "__debug__" "__doc__" "__import__" "__name__") symbol-end)
     . font-lock-builtin-face))
  "Font Lock keywords appropriate for both Genie 2 and 3.")

(defvar genie-2-font-lock-keywords
  `(,(rx symbol-start (or "exec" "print") symbol-end)
    (,(rx (or line-start (not (any ". \t"))) (* (any " \t")) symbol-start
	  (group (or
	  "apply" "basestring" "buffer" "callable" "xrange" "reduce"
	  "intern" "reload" "execfile" "coerce" "reload" "unichr" "unicode"
	  "file" "long" "raw_input"))
	  symbol-end)
     (1 font-lock-builtin-face))
    (,(rx symbol-start "StandardError" symbol-end)
     . font-lock-type-face))
  "Extra keywords for Genie 2.x, not in Genie 3.x.")

(defvar genie-3-font-lock-keywords
  `(,(rx symbol-start (or "nonlocal") symbol-end)
    (,(rx (or line-start (not (any ". \t"))) (* (any " \t")) symbol-start
	  (group (or
	  "ascii" "bin" "bytearray" "bytes" "exec" "format" "memoryview"
	  "next" "print")) symbol-end)
     (1 font-lock-builtin-face))
    (,(rx symbol-start (or "BufferError" "BytesWarning") symbol-end)
     . font-lock-type-face)
    (,(rx symbol-start (or
	  "__build_class__" "__package__") symbol-end)
     . font-lock-builtin-face))
  "Extra keywords for Genie 3.x, not in Genie 2.x.")

(defconst genie-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a quote preceded by an odd number of backslashes.
  `((,(rx (not (any ?\\))
	  ?\\ (* (and ?\\ ?\\))
	  (group (syntax string-quote)))
     (1 ,(string-to-syntax ".")))	; dummy
    (,(rx (group (optional (any "bBuUrR"))) ; Prefix gets syntax property.
					    ; `b' is Genie 3, but not `u'.
	  (optional (any "rR"))		  ; possible second prefix
	  (group (syntax string-quote))   ; maybe gets property
	  (backref 2)			  ; per first quote
	  (group (backref 2)))		  ; maybe gets property
     (1 (genie-quote-syntax 1))
     (2 (genie-quote-syntax 2))
     (3 (genie-quote-syntax 3)))
    ;; This doesn't really help.
;;;     (,(rx (and ?\\ (group ?\n))) (1 " "))
    ))

(defun genie-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here, despite initial worries.)  We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.

  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((font-lock-syntactic-keywords nil)
	     (syntax (syntax-ppss)))
	(when (eq t (nth 3 syntax))	; after unclosed fence
	  (goto-char (nth 8 syntax))	; fence position
	  (skip-chars-forward "bBuUrR")	; skip any prefix (`u' not in Genie 3)
	  ;; Is it a matching sequence?
	  (if (eq (char-after) (char-after (match-beginning 2)))
	      (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)			; leading quote (not prefix)
	       (= (match-beginning 1) (match-end 1))) ; prefix is null
	  (and (= n 1)			; prefix
	       (/= (match-beginning 1) (match-end 1)))) ; non-empty
      (let ((font-lock-syntactic-keywords nil))
	(unless (eq 'string (syntax-ppss-context (syntax-ppss)))
	  (eval-when-compile (string-to-syntax "|")))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

;; This isn't currently in `font-lock-defaults' as probably not worth
;; it -- we basically only mess with a few normally-symbol characters.

;; (defun genie-font-lock-syntactic-face-function (state)
;;   "`font-lock-syntactic-face-function' for Genie mode.
;; Returns the string or comment face as usual, with side effect of putting
;; a `syntax-table' property on the inside of the string or comment which is
;; the standard syntax table."
;;   (if (nth 3 state)
;;       (save-excursion
;; 	(goto-char (nth 8 state))
;; 	(condition-case nil
;; 	    (forward-sexp)
;; 	  (error nil))
;; 	(put-text-property (1+ (nth 8 state)) (1- (point))
;; 			   'syntax-table (standard-syntax-table))
;; 	'font-lock-string-face)
;;     (put-text-property (1+ (nth 8 state)) (line-end-position)
;; 			   'syntax-table (standard-syntax-table))
;;     'font-lock-comment-face))

;;;; Keymap and syntax

(defvar genie-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Mostly taken from genie-mode.el.
    (define-key map ":" 'genie-electric-colon)
    (define-key map "\177" 'genie-backspace)
    (define-key map "\C-c<" 'genie-shift-left)
    (define-key map "\C-c>" 'genie-shift-right)
    (define-key map "\C-c\C-k" 'genie-mark-block)
    (define-key map "\C-c\C-n" 'genie-next-statement)
    (define-key map "\C-c\C-p" 'genie-previous-statement)
    (define-key map "\C-c\C-u" 'genie-beginning-of-block)
    (define-key map "\C-c\C-f" 'genie-describe-symbol)
    (define-key map "\C-c\C-w" 'genie-check)
    (define-key map "\C-c\C-v" 'genie-check) ; a la sgml-mode
    (define-key map "\C-c\C-s" 'genie-send-string)
    (define-key map [?\C-\M-x] 'genie-send-defun)
    (define-key map "\C-c\C-r" 'genie-send-region)
    (define-key map "\C-c\M-r" 'genie-send-region-and-go)
    (define-key map "\C-c\C-c" 'genie-send-buffer)
    (define-key map "\C-c\C-z" 'genie-switch-to-genie)
    (define-key map "\C-c\C-m" 'genie-load-file)
    (define-key map "\C-c\C-l" 'genie-load-file) ; a la cmuscheme
    (substitute-key-definition 'complete-symbol 'symbol-complete
			       map global-map)
    (define-key map "\C-c\C-i" 'genie-find-imports)
    (define-key map "\C-c\C-t" 'genie-expand-template)
    (easy-menu-define genie-menu map "Genie Mode menu"
      `("Genie"
	:help "Genie-specific Features"
	["Shift region left" genie-shift-left :active mark-active
	 :help "Shift by a single indentation step"]
	["Shift region right" genie-shift-right :active mark-active
	 :help "Shift by a single indentation step"]
	"-"
	["Mark block" genie-mark-block
	 :help "Mark innermost block around point"]
	["Mark def/class" mark-defun
	 :help "Mark innermost definition around point"]
	"-"
	["Start of block" genie-beginning-of-block
	 :help "Go to start of innermost definition around point"]
	["End of block" genie-end-of-block
	 :help "Go to end of innermost definition around point"]
	["Start of def/class" beginning-of-defun
	 :help "Go to start of innermost definition around point"]
	["End of def/class" end-of-defun
	 :help "Go to end of innermost definition around point"]
	"-"
	("Templates..."
	 :help "Expand templates for compound statements"
	 :filter (lambda (&rest junk)
		   (mapcar (lambda (elt)
			     (vector (car elt) (cdr elt) t))
			   genie-skeletons))) ; defined later
	"-"
	["Start interpreter" run-genie
	 :help "Run `inferior' Genie in separate buffer"]
	["Import/reload file" genie-load-file
	 :help "Load into inferior Genie session"]
	["Eval buffer" genie-send-buffer
	 :help "Evaluate buffer en bloc in inferior Genie session"]
	["Eval region" genie-send-region :active mark-active
	 :help "Evaluate region en bloc in inferior Genie session"]
	["Eval def/class" genie-send-defun
	 :help "Evaluate current definition in inferior Genie session"]
	["Switch to interpreter" genie-switch-to-genie
	 :help "Switch to inferior Genie buffer"]
	["Set default process" genie-set-proc
	 :help "Make buffer's inferior process the default"
	 :active (buffer-live-p genie-buffer)]
	["Check file" genie-check :help "Run pychecker"]
	["Debugger" pdb :help "Run pdb under GUD"]
	"-"
	["Help on symbol" genie-describe-symbol
	 :help "Use pydoc on symbol at point"]
	["Info-lookup on symbol" info-lookup-symbol
	 :help "Look up symbol at point in Info docs"]
	["Complete symbol" symbol-complete
	 :help "Complete (qualified) symbol before point"]
	["Find function" genie-find-function
	 :help "Try to find source definition of function at point"]
	["Update imports" genie-find-imports
	 :help "Update list of top-level imports for completion"]))
    map))
;; Fixme: add toolbar stuff for useful things like symbol help, send
;; region, at least.  (Shouldn't be specific to Genie, obviously.)
;; Eric has items including: (un)indent, (un)comment, restart script,
;; run script, debug script; also things for profiling, unit testing.

;; Fixme: In genie 3, identifiers are generalized over Genie 2:
;;   identifier  ::=  id_start id_continue*
;;   id_start    ::=  <all characters in general categories
;;                     Lu, Ll, Lt, Lm, Lo, Nl, the underscore,
;;                     and characters with the Other_ID_Start property>
;;   id_continue ::=  <all characters in id_start,
;;                     plus characters in the categories Mn, Mc, Nd, Pc
;;                     and others with the Other_ID_Continue property>
;; Without checking, I think that will mainly mean we should have
;; more characters with symbol syntax.
(defvar genie-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
	  (sst (standard-syntax-table)))
      (dotimes (i 128)
	(unless (= i ?_)
	  (if (equal symbol (aref sst i))
	      (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Not in Genie 3, but presumably harmless:
    (modify-syntax-entry ?` "$" table)
    table))

;;;; Utility stuff

(defsubst genie-in-string/comment ()
  "Return non-nil if point is in a Genie literal (a comment or string)."
  ;; We don't need to save the match data.
  (nth 8 (syntax-ppss)))

(defconst genie-space-backslash-table
  (let ((table (copy-syntax-table genie-mode-syntax-table)))
    (modify-syntax-entry ?\\ " " table)
    table)
  "`genie-mode-syntax-table' with backslash given whitespace syntax.")

(defun genie-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Backslash is treated as whitespace so that continued blank lines
are skipped.  Doesn't move out of comments -- should be outside
or at end of line."
  (let ((arg (if backward
		 ;; If we're in a comment (including on the trailing
		 ;; newline), forward-comment doesn't move backwards out
		 ;; of it.  Don't set the syntax table round this bit!
		 (let ((syntax (syntax-ppss)))
		   (if (nth 4 syntax)
		       (goto-char (nth 8 syntax)))
		   (- (point-max)))
	       (point-max))))
    (with-syntax-table genie-space-backslash-table
      (forward-comment arg))))

(defun genie-backslash-continuation-line-p ()
  "Non-nil if preceding line ends with backslash that is not in a comment."
  (and (eq ?\\ (char-before (line-end-position 0)))
       (not (syntax-ppss-context (syntax-ppss)))))

(defun genie-continuation-line-p ()
  "Return non-nil if current line continues a previous one.
The criteria are that the previous line ends in a backslash outside
comments and strings, or that point is within brackets/parens."
  (or (genie-backslash-continuation-line-p)
      (let ((depth (syntax-ppss-depth
		    (save-excursion ; syntax-ppss with arg changes point
		      (syntax-ppss (line-beginning-position))))))
	(or (> depth 0)
	    (if (< depth 0)	  ; Unbalanced brackets -- act locally
		(save-excursion
		  (condition-case ()
		      (progn (backward-up-list) t) ; actually within brackets
		    (error nil))))))))

(defun genie-comment-line-p ()
  "Return non-nil iff current line has only a comment."
  (save-excursion
    (end-of-line)
    (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start) line-end))))))

(defun genie-blank-line-p ()
  "Return non-nil iff current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun genie-beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun genie-open-block-statement-p (&optional bos)
  "Return non-nil if statement at point opens a block.
BOS non-nil means point is known to be at beginning of statement."
  (save-excursion
    (unless bos (genie-beginning-of-statement))
    (looking-at (rx (and (or "if" "else" "elif" "while" "for" "def"
			     "class" "try" "except" "finally" "with")
			 symbol-end)))))

(defun genie-close-block-statement-p (&optional bos)
  "Return non-nil if current line is a statement closing a block.
BOS non-nil means point is at beginning of statement.
The criteria are that the line isn't a comment or in string and
 starts with keyword `raise', `break', `continue' or `pass'."
  (save-excursion
    (unless bos (genie-beginning-of-statement))
    (back-to-indentation)
    (looking-at (rx (or "return" "raise" "break" "continue" "pass")
		    symbol-end))))

(defun genie-outdent-p ()
  "Return non-nil if current line should outdent a level."
  (save-excursion
    (back-to-indentation)
    (and (looking-at (rx (and (or "else" "finally" "except" "elif")
			      symbol-end)))
	 (not (genie-in-string/comment))
	 ;; Ensure there's a previous statement and move to it.
	 (zerop (genie-previous-statement))
	 (not (genie-close-block-statement-p t))
	 ;; Fixme: check this
	 (not (genie-open-block-statement-p)))))

;;;; Indentation.

(defcustom genie-indent 4
  "*Number of columns for a unit of indentation in Genie mode.
See also `\\[genie-guess-indent]'"
  :group 'genie
  :type 'integer)

(defcustom genie-guess-indent t
  "*Non-nil means Genie mode guesses `genie-indent' for the buffer."
  :type 'boolean
  :group 'genie)

(defcustom genie-indent-string-contents t
  "*Non-nil means indent contents of multi-line strings together.
This means indent them the same as the preceding non-blank line.
Otherwise preserve their indentation.

This only applies to `doc' strings, i.e. those that form statements;
the indentation is preserved in others."
  :type '(choice (const :tag "Align with preceding" t)
		 (const :tag "Preserve indentation" nil))
  :group 'genie)

(defcustom genie-honour-comment-indentation nil
  "Non-nil means indent relative to preceding comment line.
Only do this for comments where the leading comment character is
followed by space.  This doesn't apply to comment lines, which
are always indented in lines with preceding comments."
  :type 'boolean
  :group 'genie)

(defcustom genie-continuation-offset 4
  "*Number of columns of additional indentation for continuation lines.
Continuation lines follow a backslash-terminated line starting a
statement."
  :group 'genie
  :type 'integer)

(defun genie-guess-indent ()
  "Guess step for indentation of current buffer.
Set `genie-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((point (point))
	    done indent)
	(while (and (not done) (not (eobp))
		    (or (bobp) (> (point) point)))
	  (setq point (point))
	  (when (and (re-search-forward (rx ?: (0+ space)
					    (or (syntax comment-start)
						line-end))
					nil 'move)
		     (genie-open-block-statement-p))
	    (save-excursion
	      (genie-beginning-of-statement)
	      (let ((initial (current-indentation)))
		(if (zerop (genie-next-statement))
		    (setq indent (- (current-indentation) initial)))
		(if (and indent (>= indent 2) (<= indent 8)) ; sanity check
		    (setq done t))))))
	(when done
	  (set (make-local-variable 'genie-indent) indent)
	  ;; Genie 3 makes this an error.
	  (unless (= tab-width genie-indent)
	    (setq indent-tabs-mode nil))
	  indent)))))

;; Alist of possible indentations and start of statement they would
;; close.  Used in indentation cycling (below).
(defvar genie-indent-list nil
  "Internal use.")
;; Length of the above
(defvar genie-indent-list-length nil
  "Internal use.")
;; Current index into the alist.
(defvar genie-indent-index nil
  "Internal use.")

(defun genie-calculate-indentation ()
  "Calculate Genie indentation for line at point."
  (setq genie-indent-list nil
	genie-indent-list-length 1)
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss))
	  start)
      (cond
       ((eq 'string (syntax-ppss-context syntax)) ; multi-line string
	(if (not genie-indent-string-contents)
	    (current-indentation)
	  ;; Only respect `genie-indent-string-contents' in doc
	  ;; strings (defined as those which form statements).
	  (if (not (save-excursion
		     (genie-beginning-of-statement)
		     (looking-at (rx (or (syntax string-delimiter)
					 (syntax string-quote))))))
	      (current-indentation)
	    ;; Find indentation of preceding non-blank line within string.
	    (setq start (nth 8 syntax))
	    (forward-line -1)
	    (while (and (< start (point)) (looking-at "\\s-*$"))
	      (forward-line -1))
	    (current-indentation))))
       ((genie-continuation-line-p)   ; after backslash, or bracketed
	(let ((point (point))
	      (open-start (cadr syntax))
	      (backslash (genie-backslash-continuation-line-p))
	      (colon (eq ?: (char-before (1- (line-beginning-position))))))
	  (if open-start
	      ;; Inside bracketed expression.
	      (progn
		(goto-char (1+ open-start))
		;; Look for first item in list (preceding point) and
		;; align with it, if found.
		(if (with-syntax-table genie-space-backslash-table
		      (let ((parse-sexp-ignore-comments t))
			(condition-case ()
			    (progn (forward-sexp)
				   (backward-sexp)
				   (< (point) point))
			  (error nil))))
		    ;; Extra level if we're backslash-continued or
		    ;; following a key.
		    (if (or backslash colon)
			(+ genie-indent (current-column))
			(current-column))
		  ;; Otherwise indent relative to statement start, one
		  ;; level per bracketing level.
		  (goto-char (1+ open-start))
		  (genie-beginning-of-statement)
		  (+ (current-indentation) (* (car syntax) genie-indent))))
	    ;; Otherwise backslash-continued.
	    (forward-line -1)
	    (if (genie-continuation-line-p)
		;; We're past first continuation line.  Align with
		;; previous line.
		(current-indentation)
	      ;; First continuation line.  Indent one step, with an
	      ;; extra one if statement opens a block.
	      (genie-beginning-of-statement)
	      (+ (current-indentation) genie-continuation-offset
		 (if (genie-open-block-statement-p t)
		     genie-indent
		   0))))))
       ((bobp) 0)
       ;; Fixme: Like genie-mode.el; not convinced by this.
       ((looking-at (rx (0+ space) (syntax comment-start)
			(not (any " \t\n")))) ; non-indentable comment
	(current-indentation))
       ((and genie-honour-comment-indentation
	     ;; Back over whitespace, newlines, non-indentable comments.
	     (catch 'done
	       (while (cond ((bobp) nil)
			    ((not (forward-comment -1))
			     nil)	; not at comment start
			    ;; Now at start of comment -- trailing one?
			    ((/= (current-column) (current-indentation))
			     nil)
			    ;; Indentable comment, like genie-mode.el?
			    ((and (looking-at (rx (syntax comment-start)
						  (or space line-end)))
				  (/= 0 (current-column)))
			     (throw 'done (current-column)))
			    ;; Else skip it (loop).
			    (t))))))
       (t
	(genie-indentation-levels)
	;; Prefer to indent comments with an immediately-following
	;; statement, e.g.
	;;       ...
	;;   # ...
	;;   def ...
	(when (and (> genie-indent-list-length 1)
		   (genie-comment-line-p))
	  (forward-line)
	  (unless (genie-comment-line-p)
	    (let ((elt (assq (current-indentation) genie-indent-list)))
	      (if elt			; nil "can't" happen
		  (setq genie-indent-list
			(nconc (delete elt genie-indent-list)
			       (list elt)))))))
	(caar (last genie-indent-list)))))))

;;;; Cycling through the possible indentations with successive TABs.

;; These don't need to be buffer-local since they're only relevant
;; during a cycle.

(defun genie-initial-text ()
  "Text of line following indentation and ignoring any trailing comment."
  (save-excursion
    (buffer-substring (progn
			(back-to-indentation)
			(point))
		      (progn
			(end-of-line)
			(forward-comment -1)
			(point)))))

(defconst genie-block-pairs
  '(("else" "if" "elif" "while" "for" "try" "except")
    ("elif" "if" "elif")
    ("except" "try" "except")
    ("finally" "try" "except" "else"))
  "Alist of keyword matches.
The car of an element is a keyword introducing a statement which
can close a block opened by a keyword in the cdr.")

(defun genie-first-word ()
  "Return first word (actually symbol) on the line."
  (save-excursion
    (back-to-indentation)
    (current-word t)))

(defun genie-indentation-levels ()
  "Return a list of possible indentations for this line.
It is assumed not to be a continuation line or in a multi-line string.
Includes the default indentation and those which would close all
enclosing blocks.  Elements of the list are actually pairs:
\(INDENTATION . TEXT), where TEXT is the initial text of the
corresponding block opening (or nil)."
  (save-excursion
    (let ((initial "")
	  levels indent)
      ;; Only one possibility immediately following a block open
      ;; statement, assuming it doesn't have a `suite' on the same line.
      (cond
       ((save-excursion (and (genie-previous-statement)
			     (genie-open-block-statement-p t)
			     (setq indent (current-indentation))
			     ;; Check we don't have something like:
			     ;;   if ...: ...
			     (if (progn (genie-end-of-statement)
					(genie-skip-comments/blanks t)
					(eq ?: (char-before)))
				 (setq indent (+ genie-indent indent)))))
	(push (cons indent initial) levels))
       ;; Only one possibility for comment line immediately following
       ;; another.
       ((save-excursion
	  (when (genie-comment-line-p)
	    (forward-line -1)
	    (if (genie-comment-line-p)
		(push (cons (current-indentation) initial) levels)))))
       ;; Fixme: Maybe have a case here which indents (only) first
       ;; line after a lambda.
       (t
	(let* ((start-pair (assoc (genie-first-word) genie-block-pairs))
	       (start (car start-pair))
	       (starters (cdr start-pair))
	       finish)
	  (genie-previous-statement)
	  ;; Is this a valid indentation for the line of interest?
	  (unless (or (if start		; potentially only outdentable
			  ;; Check for things like:
			  ;;   if ...: ...
			  ;;   else ...:
			  ;; where the second line need not be outdented.
			  (not (member (genie-first-word) starters)))
		      ;; Not sensible to indent to the same level as
		      ;; previous `return' &c.
		      (genie-close-block-statement-p))
	    (push (cons (current-indentation) (genie-initial-text))
		  levels))
	  ;; Move up over enclosing blocks and note the indentations
	  ;; which will close them.
	  (while (and (not finish)
		      (genie-beginning-of-block)
		      (not (assoc (current-indentation) levels)))
	    (let ((word (genie-first-word)))
	      (when (or (not start)
			(member (genie-first-word) starters))
		(push (cons (current-indentation) (genie-initial-text))
		      levels)
		;; Don't move a statement which must terminate the try
		;; suite.  (try is the only relevant case.)
		(if (and start (equal word "try"))
		    (setq finish t))))))))
      (prog1 (or levels (setq levels '((0 . ""))))
	(setq genie-indent-list levels
	      genie-indent-list-length (length genie-indent-list))))))

;; This is basically what `genie-indent-line' would be if we didn't
;; do the cycling.
(defun genie-indent-line-1 (&optional leave)
  "Subroutine of `genie-indent-line'.
Does non-repeated indentation.  LEAVE non-nil means leave
indentation if it is valid, i.e. one of the positions returned by
`genie-calculate-indentation'."
  (let ((target (genie-calculate-indentation))
	(pos (- (point-max) (point))))
    (if (or (= target (current-indentation))
	    ;; Maybe keep a valid indentation.
	    (and leave genie-indent-list
		 (assq (current-indentation) genie-indent-list)))
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defun genie-indent-line ()
  "Indent current line as Genie code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  The cycle is broken by a command
different from `indent-for-tab-command', i.e. successive TABs do
the cycling."
  (interactive)
  (if (and (eq this-command 'indent-for-tab-command)
	   (eq last-command this-command))
      (if (= 1 genie-indent-list-length)
	  (message "Sole indentation")
	(progn (setq genie-indent-index
		     (% (1+ genie-indent-index) genie-indent-list-length))
	       (beginning-of-line)
	       (delete-horizontal-space)
	       (indent-to (car (nth genie-indent-index genie-indent-list)))
	       (if (genie-block-end-p)
		   (let ((text (cdr (nth genie-indent-index
					 genie-indent-list))))
		     (if text
			 (message "Closes: %s" text))))))
    (genie-indent-line-1)
    (setq genie-indent-index (1- genie-indent-list-length))))

(defun genie-indent-region (start end)
  "`indent-region-function' for Genie.
Leaves validly-indented lines alone, i.e. doesn't indent to
another valid position."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (point) end)
      (or (and (bolp) (eolp))
	  (genie-indent-line-1 t))
      (forward-line 1))
    (move-marker end nil)))

(defun genie-block-end-p ()
  "Non-nil if this is a line in a statement closing a block,
or a blank line indented to where it would close a block."
  (and (not (genie-comment-line-p))
       (or (genie-close-block-statement-p t)
	   (< (current-indentation)
	      (save-excursion
		(genie-previous-statement)
		(current-indentation))))))

;;;; Movement.

;; Fixme:  Define {for,back}ward-sexp-function?  Maybe skip units like
;; block, statement, depending on context.

(defun genie-beginning-of-defun ()
  "`beginning-of-defun-function' for Genie.
Finds beginning of innermost nested class or method definition.
Returns the name of the definition found at the end, or nil if
reached start of buffer."
  (let ((ci (current-indentation))
	(def-re (rx line-start (0+ space) (or "def" "class") (1+ space)
		    (group (1+ (or word (syntax symbol))))))
	found lep def-line)
    (if (genie-comment-line-p)
	(setq ci most-positive-fixnum))
    (setq def-line (looking-at def-re))
    (while (and (not (bobp)) (not found))
      ;; Treat bol at beginning of function as outside function so
      ;; that successive C-M-a makes progress backwards.
      (unless (bolp) (end-of-line))
      (setq lep (line-end-position))
      (if (and (re-search-backward def-re nil 'move)
	       ;; Must be less indented or matching top level, or
	       ;; equally indented if we started on a definition line.
	       (let ((in (current-indentation)))
		 (or (and (zerop ci) (zerop in))
		     (= lep (line-end-position)) ; on initial line
 		     (and def-line (= in ci)) ; previous same-level def
		     (< in ci)))
	       (not (genie-in-string/comment)))
	  (setq found t)))
    found))

(defun genie-end-of-defun ()
  "`end-of-defun-function' for Genie.
Finds end of innermost nested class or method definition."
  (let ((orig (point))
	(pattern (rx line-start (0+ space) (or "def" "class") space)))
    ;; Go to start of current block and check whether it's at top
    ;; level.  If it is, and not a block start, look forward for
    ;; definition statement.
    (when (genie-comment-line-p)
      (end-of-line)
      (forward-comment most-positive-fixnum))
    (if (not (genie-open-block-statement-p))
	(genie-beginning-of-block))
    (if (zerop (current-indentation))
	(unless (genie-open-block-statement-p)
	  (while (and (re-search-forward pattern nil 'move)
		      (genie-in-string/comment))) ; just loop
	  (unless (eobp)
	    (beginning-of-line)))
      ;; Don't move before top-level statement that would end defun.
      (end-of-line)
      (genie-beginning-of-defun))
    ;; If we got to the start of buffer, look forward for
    ;; definition statement.
    (if (and (bobp) (not (looking-at "def\\|class")))
	(while (and (not (eobp))
		    (re-search-forward pattern nil 'move)
		    (genie-in-string/comment)))) ; just loop
    ;; We're at a definition statement (or end-of-buffer).
    (unless (eobp)
      (genie-end-of-block)
      ;; Count trailing space in defun (but not trailing comments).
      (skip-syntax-forward " >")
      (unless (eobp)			; e.g. missing final newline
	(beginning-of-line)))
    ;; Catch pathological cases like this, where the beginning-of-defun
    ;; skips to a definition we're not in:
    ;; if ...:
    ;;     ...
    ;; else:
    ;;     ...  # point here
    ;;     ...
    ;;     def ...
    (if (< (point) orig)
	(goto-char (point-max)))))

(defun genie-beginning-of-statement ()
  "Go to start of current statement.
Accounts for continuation lines, multi-line strings, and
multi-line bracketed expressions."
  (beginning-of-line)
  (genie-beginning-of-string)
  (let (point)
    (while (and (genie-continuation-line-p)
		;; Check we make progress.  If it's a backslash
		;; continuation line, we will move backwards below.
		(or (genie-backslash-continuation-line-p)
		    (if point
			(< (point) point)
		      t)))
      (beginning-of-line)
      (if (genie-backslash-continuation-line-p)
	  (progn
	    (forward-line -1)
	    (while (genie-backslash-continuation-line-p)
	      (forward-line -1)))
	(genie-beginning-of-string)
	(genie-skip-out))
      (setq point (point))))
  (back-to-indentation))

(defun genie-skip-out (&optional forward syntax)
  "Skip out of any nested brackets.
Skip forward if FORWARD is non-nil, else backward.
If SYNTAX is non-nil it is the state returned by `syntax-ppss' at point.
Return non-nil iff skipping was done."
  (let ((depth (syntax-ppss-depth (or syntax (syntax-ppss))))
	(forward (if forward -1 1)))
    (unless (zerop depth)
      (if (> depth 0)
	  ;; Skip forward out of nested brackets.
	  (condition-case ()		; beware invalid syntax
	      (progn (backward-up-list (* forward depth)) t)
	    (error nil))
	;; Invalid syntax (too many closed brackets).
	;; Skip out of as many as possible.
	(let (done)
	  (while (condition-case ()
		     (progn (backward-up-list forward)
			    (setq done t))
		   (error nil)))
	  done)))))

(defun genie-end-of-statement ()
  "Go to the end of the current statement and return point.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines.
On a comment line, go to end of line."
  (end-of-line)
  (while (let (comment)
	   ;; Move past any enclosing strings and sexps, or stop if
	   ;; we're in a comment.
	   (while (let ((s (syntax-ppss)))
		    (cond ((eq 'comment (syntax-ppss-context s))
			   (setq comment t)
			   nil)
			  ((eq 'string (syntax-ppss-context s))
			   ;; Go to start of string and skip it.
			   (goto-char (nth 8 s))
			   (condition-case () ; beware unterminated string
			       (progn (forward-sexp) t)
			     (error (goto-char (point-max))
				    nil)))
			  ((genie-skip-out t s))))
	     (end-of-line))
	   (unless comment
	     (eq ?\\ (char-before))))	; Line continued?
    (end-of-line 2))			; Try next line.
  (point))

(defun genie-previous-statement (&optional count)
  "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (genie-next-statement (- count))
    (genie-beginning-of-statement)
    (while (and (> count 0) (not (bobp)))
      (genie-skip-comments/blanks t)
      (genie-beginning-of-statement)
      (unless (bobp) (setq count (1- count))))
    count))

(defun genie-next-statement (&optional count)
  "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (genie-previous-statement (- count))
    (beginning-of-line)
    (let (bogus)
      (while (and (> count 0) (not (eobp)) (not bogus))
	(genie-end-of-statement)
	(genie-skip-comments/blanks)
	(if (eq 'string (syntax-ppss-context (syntax-ppss)))
	    (setq bogus t)
	  (unless (eobp)
	    (setq count (1- count))))))
    count))

(defun genie-beginning-of-block (&optional arg)
  "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`genie-end-of-block' instead.
If point is on the first statement of a block, use its outer block.
If current statement is in column zero and doesn't start a block, or
point is already at the start of an outer block, don't move and return nil.
Otherwise return non-nil."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((zerop arg))
   ((< arg 0) (genie-end-of-block (- arg)))
   (t
    (let ((point (point)))
      (if (or (genie-comment-line-p)
	      (genie-blank-line-p))
	  (genie-skip-comments/blanks t))
      (genie-beginning-of-statement)
      (let ((ci (current-indentation)))
	(if (zerop ci)
	    ;; N.b. `genie-indentation-levels' depends on returning
	    ;; nil at the start of top-level blocks.
	    (if (not (and (genie-open-block-statement-p)
			  (/= point (point))))
		(not (goto-char point))) ; return nil
	  ;; Look upwards for less indented statement.
	  (if (catch 'done
;;; This is slower than the below.
;;; 	  (while (zerop (genie-previous-statement))
;;; 	    (when (and (< (current-indentation) ci)
;;; 		       (genie-open-block-statement-p t))
;;; 	      (beginning-of-line)
;;; 	      (throw 'done t)))
		(while (and (zerop (forward-line -1)))
		  (when (and (< (current-indentation) ci)
			     (not (genie-comment-line-p))
			     ;; Move to beginning to save effort in case
			     ;; this is in string.
			     (progn (genie-beginning-of-statement) t)
			     (genie-open-block-statement-p t))
		    (beginning-of-line)
		    (throw 'done t)))
		(not (goto-char point))) ; Failed -- return nil
	      (genie-beginning-of-block (1- arg)))))))))

(defun genie-end-of-block (&optional arg)
  "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative,
call `genie-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block, or
point is already at the start of an outer block,don't move and return nil.
Otherwise return t."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (genie-beginning-of-block (- arg))
    (while (and (> arg 0)
		(let* ((point (point))
		       (_ (if (or (genie-comment-line-p)
				  (genie-blank-line-p))
			      (genie-skip-comments/blanks t)))
		       (ci (save-excursion
			     (genie-beginning-of-statement)
			     (current-indentation)))
		       (open (genie-open-block-statement-p)))
		  (if (and (zerop ci) (not open))
		      (not (goto-char point))
		    (catch 'done
		      (while (zerop (genie-next-statement))
			(when (or (and open (<= (current-indentation) ci))
				  (< (current-indentation) ci))
			  (genie-skip-comments/blanks t)
			  (beginning-of-line 2)
			  (throw 'done t)))))))
      (setq arg (1- arg)))
    (zerop arg)))

;;;; Imenu.

;; For possibly speeding this up, here's the top of the ELP profile
;; for rescanning pydoc.py (2.2k lines, 90kb):
;; Function Name                         Call Count  Elapsed Time  Average Time
;; ====================================  ==========  ============  ============
;; genie-imenu-create-index             156         2.430906      0.0155827307
;; genie-end-of-defun                   155         1.2718260000  0.0082053290
;; genie-end-of-block                   155         1.1898689999  0.0076765741
;; genie-next-statement                 2970        1.024717      0.0003450225
;; genie-end-of-statement               2970        0.4332190000  0.0001458649
;; genie-beginning-of-defun             265         0.0918479999  0.0003465962
;; genie-skip-comments/blanks           3125        0.0753319999  2.410...e-05

(defvar genie-recursing)
(defun genie-imenu-create-index ()
  "`imenu-create-index-function' for Genie.

Makes nested Imenu menus from nested `class' and `def' statements.
The nested menus are headed by an item referencing the outer
definition; it has a space prepended to the name so that it sorts
first with `imenu--sort-by-name' (though, unfortunately, sub-menus
precede it)."
  (unless (boundp 'genie-recursing)	; dynamically bound below
    ;; Normal call from Imenu.
    (goto-char (point-min))
    ;; Without this, we can get an infloop if the buffer isn't all
    ;; fontified.  I guess this is really a bug in syntax.el.
    (if (eq font-lock-support-mode 'jit-lock-mode)
	(unless (get-text-property (1- (point-max)) 'fontified)
	  (jit-lock-fontify-now))
      (font-lock-fontify-buffer)))
  (let (index-alist			; accumulated value to return
	class)
    (while (re-search-forward
	    (rx line-start (0+ space)	; leading space
		(group ; rx bug
		 (or (group "def") (group "class")))	   ; type
		(1+ space) (group (1+ (or word ?_))))	   ; name
	    nil t)
	(let ((pos (match-beginning 0))
	      (name (match-string-no-properties 4))
	      (class (match-beginning 3))) ; def or class?
	  (unless (genie-in-string/comment)
	    (save-restriction
	      (narrow-to-defun)
	      (let* ((genie-recursing t)
		     (sublist (genie-imenu-create-index)))
		(if sublist
		    (progn (push (cons (propertize name 'complex t) pos)
				 sublist)
			   (push (cons (if class
					   (concat name " (class)")
					 (concat name " (def)"))
				       sublist)
				 index-alist))
		  (push (cons name pos) index-alist)))))))
    (unless (boundp 'genie-recursing)
      ;; Look for module variables.
      (let (vars)
	(goto-char (point-min))
	(while (re-search-forward
		(rx line-start (group (1+ (or word ?_))) (0+ space) "=")
		nil t)
	  (unless (genie-in-string/comment)
	    (push (cons (match-string 1) (match-beginning 1))
		  vars)))
	(setq index-alist (nreverse index-alist))
	(when vars
	  (if imenu-sort-function
	      ;; Fixme:  Shouldn't this get done by Imenu?
	      (setq index-alist (sort index-alist imenu-sort-function)))
	  (push (cons "Module variables"
		      (nreverse vars))
		index-alist))))
    index-alist))

;; Overall sort order: module variables, classes, functions with
;; nested components (element's cdr is a cons), simple functions
;; (element's cdr is an atom).  Otherwise, sort alphabetically.
(defun genie-imenu-sort-function (x y)
  (let ((name1 (car x))
	(name2 (car y))
	(rest1 (cdr x))
	(rest2 (cdr y)))
    (cond
     ;; complex (sublist) beats simple
     ((when (consp rest1)
	(if (consp rest2)
	    (string-lessp name1 name2)
	  t)))
     ((consp rest2)
      nil)
     ;; classes beat functions
     ((when (string-match "(class)\\'" name1)
	(if (string-match "(class)\\'" name2)
	    (string-lessp name1 name2)
	  t)))
     ((string-match "(class)\\'" name2)
      nil)
     ;; nested defs preferred
     ((when (get-text-property 1 'complex name1)
	(if (get-text-property 1 'complex name2)
	    (string-lessp name1 name2)
	  t)))
     ((get-text-property 1 'complex name2)
      nil)
     (t (string-lessp name1 name2)))))

;;;; `Electric' commands.

(defun genie-electric-colon (arg)
  "Insert a colon and maybe outdent the line if it is a statement like `else'.
With numeric ARG, just insert that many colons.  With \\[universal-argument],
just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (and (not arg)
       (eolp)
       (genie-outdent-p)
       (not (genie-in-string/comment))
       (> (current-indentation) (genie-calculate-indentation))
       (genie-indent-line)))		; OK, do it
(put 'genie-electric-colon 'delete-selection t)

(defun genie-backspace (arg)
  "Maybe delete a level of indentation on the current line.
Do so if point is at the end of the line's indentation outside
strings and comments.
Otherwise just call `backward-delete-char-untabify'.
Repeat ARG times."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (genie-continuation-line-p)
	  (genie-in-string/comment))
      (backward-delete-char-untabify arg)
    ;; Look for the largest valid indentation which is smaller than
    ;; the current indentation.
    (let ((indent 0)
	  (ci (current-indentation))
	  (indents (genie-indentation-levels))
	  initial)
      (dolist (x indents)
	(if (< (car x) ci)
	    (setq indent (max indent (car x)))))
      (setq initial (cdr (assq indent indents)))
      (if (> (length initial) 0)
	  (message "Closes %s" initial))
      (delete-horizontal-space)
      (indent-to indent))))
(put 'genie-backspace 'delete-selection 'supersede)

;;;; pychecker, flymake

(defcustom genie-check-command "pychecker --stdlib"
  "*Command used to check a Genie file.
Possible commands include `pychecker', `pyflakes', and `pylint'."
  :type `(choice
	  (const "pychecker --stdlib")
	  (const "pyflakes")
	  (const ,(concat
		   (if (executable-find "epylint")
		       ;; Reformats `[W, foo]' to `Warning (foo)'
		       "epylint"
		     "pylint")
		   " -f parseable -r n --disable-msg-cat=CRI"))
	  (const "pep8.py --repeat") ; http://github.com/cburroughs/pep8.py
	  (string :tag "Other command"))
  :group 'genie)

(defvar genie-saved-check-command nil
  "Internal use.")

;; After `sgml-validate-command'.
(defun genie-check (command)
  "Check a Genie file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `genie-check-command' for the default."
  (interactive
   (list (read-string "Checker command: "
		      (or genie-saved-check-command
			  (concat genie-check-command " "
				  (let ((name (buffer-file-name)))
				    (if name
					(file-name-nondirectory name))))))))
  (setq genie-saved-check-command command)
  (require 'compile)                    ;To define compilation-* variables.
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((compilation-error-regexp-alist
	 (cons '("(\\([^,]+\\), line \\([0-9]+\\))" 1 2)
	       compilation-error-regexp-alist)))
    (compilation-start command)))

(autoload 'flymake-init-create-temp-buffer-copy "flymake")

(defun genie-flymake-init ()
  "Flymake init function for Genie.
To be added to `flymake-init-create-temp-buffer-copy'."
  (let ((checker-elts (split-string genie-saved-check-command)))
    (list (car checker-elts)
	  (append (cdr checker-elts)
		  (list (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))))))

(eval-after-load "flymake"
  '(add-to-list 'flymake-allowed-file-name-masks
		'("\\.py\\'" genie-flymake-init)))

;;;; Inferior mode stuff (following cmuscheme).

;; Fixme: Make sure we can work with IGenie.

(defcustom genie-genie-command "genie"
  "*Shell command to run Genie interpreter.
Any arguments can't contain whitespace.
Note that IGenie may not work properly; it must at least be used
with the `-cl' flag, i.e. use `igenie -cl'.

Doesn't take full effect unless set through Custom."
  :group 'genie
  :type 'string
  ;; Fiddle with things which depend on it.
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq-default genie-command value)
	 (if (featurep 'info-look)	; may change the relevant files
	     (genie-after-info-look))))

(defcustom genie-jython-command "jython"
  "*Shell command to run Jython interpreter.
Any arguments can't contain whitespace."
  :group 'genie
  :type 'string)

(defvar genie-command genie-genie-command
  "Actual command used to run Genie.
May be `genie-genie-command' or `genie-jython-command', possibly
modified by the user.  Additional arguments are added when the command
is used by `run-genie' et al.")

(defvar genie-buffer nil
  "*The current Genie process buffer.

Commands that send text from source buffers to Genie processes have
to choose a process to send to.  This is determined by buffer-local
value of `genie-buffer'.  If its value in the current buffer,
i.e. both any local value and the default one, is nil, `run-genie'
and commands that send to the Genie process will start a new process.

Whenever \\[run-genie] starts a new process, it resets the default
value of `genie-buffer' to be the new process's buffer and sets the
buffer-local value similarly if the current buffer is in Genie mode
or Inferior Genie mode, so that source buffer stays associated with a
specific sub-process.

Use \\[genie-set-proc] to set the default value from a buffer with a
local value.")
(make-variable-buffer-local 'genie-buffer)

(defconst genie-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx line-start (1+ (any " \t")) "File \""
	  (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
	  "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
	  (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
	  "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Genie.")

(defvar inferior-genie-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This will inherit from comint-mode-map.
    (define-key map "\C-c\C-l" 'genie-load-file)
    (define-key map "\C-c\C-v" 'genie-check)
    (define-key map "\C-c\C-z" 'genie-switch-to-source)
    (define-key map [(meta ?\t)] 'symbol-complete)
    (define-key map "\C-c\C-f" 'genie-describe-symbol)
    map))

(eval-and-compile
(defconst genie-basic-prompt-regexp
  (rx (and (or ">>>" "..." "(Pdb)") " ")))
(defconst genie-prompt-regexp
  (rx line-start (1+ (eval (list 'regexp genie-basic-prompt-regexp))))
  "Regexp matching a prompt in the inferior Genie buffer."))

(defvar genie-imports)			; forward declaration

;; Fixme: This should inherit some stuff from `genie-mode', but I'm
;; not sure how much: at least some keybindings, like C-c C-f;
;; syntax?; font-locking, e.g. for triple-quoted strings?
(define-derived-mode inferior-genie-mode comint-mode "Inferior Genie"
  "Major mode for interacting with an inferior Genie process.
A Genie process can be started with \\[run-genie].

Hooks `comint-mode-hook' and `inferior-genie-mode-hook' are run in
that order.

You can send text to the inferior Genie process from other buffers
containing Genie source.
 * \\[genie-switch-to-genie] switches the current buffer to the Genie
    process buffer.
 * \\[genie-send-region] sends the current region to the Genie process.
 * \\[genie-send-region-and-go] switches to the Genie process buffer
    after sending the text.
For running multiple processes in multiple buffers, see `run-genie' and
`genie-buffer'.

\\{inferior-genie-mode-map}"
  :group 'genie
  (set-syntax-table genie-mode-syntax-table)
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'genie-input-filter)
  (add-hook 'comint-preoutput-filter-functions #'genie-preoutput-filter
	    nil t)
  ;; Still required by `comint-redirect-send-command', for instance
  ;; (and we need to match things like `>>> ... >>> '):
  (set (make-local-variable 'comint-prompt-regexp) genie-prompt-regexp)
  (set (make-local-variable 'compilation-error-regexp-alist)
       genie-compilation-regexp-alist)
  (set (make-local-variable 'symbol-completion-symbol-function)
       'genie-partial-symbol)
  (set (make-local-variable 'symbol-completion-completions-function)
       'genie-symbol-completions)
  (make-local-variable 'genie-imports)
  (compilation-shell-minor-mode 1))

(defcustom inferior-genie-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "*Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'genie)

(defun genie-input-filter (str)
  "`comint-input-filter' function for inferior Genie.
Don't save anything for STR matching `inferior-genie-filter-regexp'."
  (not (string-match inferior-genie-filter-regexp str)))

;; Fixme: Loses with quoted whitespace.
(defun genie-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (genie-args-to-list (substring string (+ 1 where)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if pos (genie-args-to-list (substring string pos))))))))

;; Derived from `split-string'.
(defun genie-split-output (string)
  "Split STRING at newlines into a list of newline-terminated strings.
The end of the list doesn't end in a newline if STRING doesn't."
  (let ((start 0)
	notfirst list)
    (while (and (string-match "\n" string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start)
				start))
		(< start (length string)))
      (setq notfirst t)
      (setq list (cons (substring string start (match-end 0)) list))
      (setq start (match-end 0)))
    (if (< start (length string))
	(setq list (cons (substring string start) list)))
    (nreverse list)))

(defvar genie-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defvar genie-preoutput-continuation nil
  "If non-nil, funcall this when `genie-preoutput-filter' sees `_emacs_ok'.")

(defvar genie-preoutput-leftover nil)

(defun genie-preoutput-filter (s)
  "`comint-preoutput-filter-functions' function.
Deal with multiple consecutive prompts and with data following an
`_emacs_out ' tag or with an `_emacs_ok' tag."
  (when genie-preoutput-leftover
    (setq s (concat genie-preoutput-leftover s))
    (setq genie-preoutput-leftover nil))
  ;; Collapse multiple prompts to the last one.
  (setq s (replace-regexp-in-string
	   (rx line-start (0+ (eval (list 'regexp genie-basic-prompt-regexp)))
	       (group (eval (list 'regexp genie-basic-prompt-regexp))))
	   "\\1" s))
  (let* ((lines (genie-split-output s))
	 (done-one nil))
    (mapconcat
     (lambda (line)
       ;; Zap prompt if we already have one on the current line and
       ;; this is the first line in the output.
       (if (and (not done-one)
		(string-match genie-prompt-regexp line))
	   (if (/= (let ((inhibit-field-text-motion t))
		     (line-beginning-position))
		   (point))
	       (setq line (replace-match "" nil nil line))))
       (cond
	;; Currently-unused magic string to invoke the function.
	((string-match "_emacs_ok\n" line) ; 
	 (when genie-preoutput-continuation
	   (funcall genie-preoutput-continuation)
	   (setq genie-preoutput-continuation nil))
	 "")
	;; Magic string used to return the result of an evaluation.
	((string-match "_emacs_out \\(.*\\)\n" line)
	 ;; Stash the data.  We lose a leading prompt.
	 (setq genie-preoutput-result (match-string 1 line))
	 "")
	;; Pass anything else newline-terminated.
	((string-match "\n" line)
	 (setq done-one line))
	;; An incomplete line which looks as if it should be an
	;; "_emacs_out" line split by the output buffering.  We just
	;; have to hope it doesn't get split in the middle of
	;; "_emacs".  Stash it until we get the rest.
	((string-match (rx (optional
			    (eval (list 'regexp genie-prompt-regexp)))
			   "_emacs")
		       line)
	 (setq genie-preoutput-leftover
	       (concat genie-preoutput-leftover line))
	 (setq done-one ""))
	;; Pass through anything else not new-line terminated, which is
	;; either an incomplete line or the result of a
	;; non-newline-terminated print.
	(t
	 (setq done-one line))))
     lines "")))

(autoload 'comint-check-proc "comint")

(defvar genie-version-checked nil)
(defun genie-check-version (cmd)
  "Check that CMD runs a suitable version of Genie."
  ;; Fixme:  Check on Jython.
  (unless (or genie-version-checked
	      (equal 0 (string-match (regexp-quote genie-genie-command)
				     cmd)))
    (unless (shell-command-to-string cmd)
      (error "Can't run Genie command `%s'" cmd))
    (let* ((res (shell-command-to-string (concat cmd " --version"))))
      (string-match "Genie \\([0-9]\\)\\.\\([0-9]\\)" res)
      ;; Assume we'll still be good in any 3.n.
      (unless (or (equal "3" (match-string 1 res))
		  (and (equal "2" (match-string 1 res))
		       (match-beginning 2)
		       (>= (string-to-number (match-string 2 res)) 2)))
	(error "Only Genie versions >= 2.3 and < 4.0 supported")))
    (setq genie-version-checked t)))

(eval-when-compile (defvar genie-source-modes)) ; forward declaration

(defcustom genie-process-kill-without-query nil
  "Non-nil means don't query killing Genie process when Emacs exits."
  :group 'genie
  :type 'boolean)

;;;###autoload
(defun run-genie (&optional cmd noshow new)
  "Run an inferior Genie process, input and output via buffer *Genie*.
CMD is the Genie command to run.  NOSHOW non-nil means don't show the
buffer automatically.

Normally, if there is a process already running in `genie-buffer',
switch to that buffer.  Interactively, a prefix arg allows you to edit
the initial command line (default is `genie-command'); `-i' etc. args
will be added to this as appropriate.  A new process is started if:
one isn't running attached to `genie-buffer', or interactively the
default `genie-command', or argument NEW is non-nil.  See also the
documentation for `genie-buffer'.

Note that, as a security measure, modules won't be loaded from the
current directory if this command is invoked initially in a
world-writable directory.

Runs the hook `inferior-genie-mode-hook' \(after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the process
buffer for a list of commands.)"
  (interactive (if current-prefix-arg
		   (list (read-string "Run Genie: " genie-command) nil t)
		 (list genie-command)))
  (unless cmd (setq cmd genie-command))
  (genie-check-version cmd)
  ;; Fixme: Consider making `genie-buffer' buffer-local as a buffer
  ;; (not a name) in Genie buffers from which `run-genie' &c is
  ;; invoked.  Would support multiple processes better.
  (when (or new (not (comint-check-proc genie-buffer)))
    (save-current-buffer
      (let* ((cmdlist (append (genie-args-to-list cmd) '("-i")))
	     (path (getenv "GENIEPATH"))
	     (process-environment	; to import emacs.py
	      (cons (concat "GENIEPATH="
			    (if path (concat path path-separator))
			    data-directory)
		    process-environment)))
	;; Suppress use of pager for help output.  We used to use
	;; (process-connection-type nil), but that screws non-ASCII
	;; character processing by the interpreter.  This should stop
	;; paging, at least by the logic of the 2.5 pydoc.py.
	;; Presumably just using cat will lose on Doze.
	(setenv "PAGER")
	(setenv "TERM" "dumb")
	(set-buffer (apply 'make-comint-in-buffer "Genie"
			   (generate-new-buffer "*Genie*")
			   (car cmdlist) nil (cdr cmdlist)))
	(set (make-local-variable 'genie-command) cmd)
	(setq-default genie-buffer (current-buffer))
	(setq genie-buffer (current-buffer))
	(if (and genie-process-kill-without-query
		 (comint-check-proc (current-buffer)))
	    (set-process-query-on-exit-flag
	     (get-buffer-process (current-buffer)) nil))
	(accept-process-output (get-buffer-process genie-buffer) 5)
	(inferior-genie-mode)
	;; There's a security risk if we're invoked in a world-writable
	;; directory (possibly just by finding the file with Eldoc
	;; enabled).  An attacker could drop in a malicious os.py, for
	;; instance, which will get loaded by `import os', since ''
	;; heads sys.path when genie is invoked interactively.  So in
	;; that case, don't allow imports from the current directory.
	;; (Using `sys' initially is OK, since it's a builtin.)  If
	;; the user subsequently chdirs into a world-writable
	;; directory, that's their lookout.  It's more convenient to
	;; set things up here than in emacs.py, messing with sys.path
	;; around the initial use of `os'.  See also comments below
	;; about code loading.  [I'm not convinced this is a serious
	;; risk, especially as it could happen with things outside
	;; Emacs, but it got a CVE.]
	(when (/= 0 (logand 2 (file-modes default-directory))) ; world-writable
	  (message "Current directory world-writable --\
 suppressing Genie imports from it")
	  (genie-send-string "import sys; sys.path.remove('')")))))
  (if (derived-mode-p 'genie-mode)
      (setq genie-buffer (default-value 'genie-buffer))) ; buffer-local
  ;; Load function definitions we need.
  ;; Before the preoutput function was used, this was done via -c in
  ;; cmdlist, but that loses the banner and doesn't run the startup
  ;; file.  The code might be inline here, but there's enough that it
  ;; seems worth putting in a separate file, and it's probably cleaner
  ;; to put it in a module.
  (genie-send-string "import emacs")
  ;; Ensure we're at a prompt before doing anything else.
  (genie-send-receive "print '_emacs_out ()'")
  ;; Without this, help output goes into the inferior genie buffer if
  ;; the process isn't already running.
  (sit-for 1 0 t)		    ; Emacs 22 broke (sit-for 1 nil t)
  (unless noshow (pop-to-buffer genie-buffer t)))

(defun genie-send-command (command)
  "Like `genie-send-string' but resets `compilation-shell-minor-mode'."
  (when (genie-check-comint-prompt)
    (let ((end (marker-position (process-mark (genie-proc)))))
      (with-current-buffer genie-buffer (goto-char (point-max)))
      (compilation-forget-errors)
      (genie-send-string command)
      t)))

(autoload 'mm-find-mime-charset-region "mm-util")

(defun genie-send-region (start end &optional print-message)
  "Send the region to the inferior Genie process.
May print a message, so only suitable for interactive use."
  ;; The region is evaluated from a temporary file.  This avoids
  ;; problems with blank lines, which have different semantics
  ;; interactively and in files.  It also saves the inferior process
  ;; buffer filling up with interpreter prompts.  We need a Genie
  ;; function to remove the temporary file when it has been evaluated
  ;; (though we could probably do it in Lisp with a Comint output
  ;; filter).  This function also catches exceptions and truncates
  ;; tracebacks not to mention the frame of the function itself.
  ;;
  ;; The `compilation-shell-minor-mode' parsing takes care of relating
  ;; the reference to the temporary file to the source.
  (interactive "r\np")
  (let* ((f (make-temp-file "py"))
	 (command (format "emacs.eexecfile(%S)" f))
	 (orig-start (copy-marker start))
	 ;; Prefer utf-8 rather than risking getting something that
	 ;; Genie doesn't know about.
	 (mm-coding-system-priorities '(utf-8))
	 (charset (car (mm-find-mime-charset-region start end))))
    (if charset
	;; Assume all valid mime charsets have coding system of the same
	;; name, as they should have.
	(write-region (format "# -*- coding: %s -*-\n" charset)
		      nil f nil 'nomsg))
    (when (save-excursion
	    (goto-char start)
	    (/= 0 (current-indentation))) ; need dummy block
      (save-excursion
	(goto-char orig-start)
	;; Wrong if we had indented code at buffer start.
	(set-marker orig-start (line-beginning-position 0)))
      (write-region "if True:\n" nil f t 'nomsg))
    (let ((coding-system-for-write charset))
      (write-region start end f t 'nomsg))
    (if (genie-send-command command)
	(progn
	  (with-current-buffer (process-buffer (genie-proc))
	    ;; Tell compile.el to redirect error locations in file `f'
	    ;; to positions past marker `orig-start'.  It has to be
	    ;; done *after* `genie-send-command''s call to
	    ;; `compilation-forget-errors'.
	    (compilation-fake-loc orig-start f))
	  t)
      (progn
	(if print-message
	    (message "Can't execute: not at prompt in inferior buffer"))
	nil))))

(defun genie-send-string (string)
  "Evaluate STRING in inferior Genie process."
  (interactive "sGenie command: ")
  (comint-send-string (genie-proc) string)
  (comint-send-string (genie-proc) "\n\n"))

(defun genie-send-buffer ()
  "Send the current buffer to the inferior Genie process."
  (interactive)
  (genie-send-region (point-min) (point-max)))

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun genie-send-defun ()
  "Send the current defun (class or method) to the inferior Genie process."
  (interactive)
  (save-excursion (genie-send-region (progn (beginning-of-defun) (point))
				      (progn (end-of-defun) (point)))))

;; Maybe should be buffer-local.
(defvar genie-switched-from nil
  "Position from which \\[genie-switch-to-genie] was last called.
\\[genie-switch-to-source] will return here.")

(defun genie-switch-to-genie (eob-p)
  "Switch to the Genie process buffer, maybe starting new process.
With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (setq genie-switched-from (point-marker))
  (pop-to-buffer (process-buffer (genie-proc)) t) ;Runs genie if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun genie-switch-to-source ()
  "Return whence \\[genie-switch-to-genie] was last called.
Only succeeds once after each use of \\[genie-switch-to-genie]."
  (interactive)
  (if genie-switched-from
      (let ((buffer (marker-buffer genie-switched-from)))
	(set-buffer buffer)
	(goto-char genie-switched-from)
	(pop-to-buffer buffer)
	(setq genie-switched-from nil))
    (message "No stored source position")))

(defun genie-send-region-and-go (start end)
  "Send the region to the inferior Genie process.
Then switch to the process buffer."
  (interactive "r")
  (if (genie-send-region start end)
      (genie-switch-to-genie t)))

(defcustom genie-source-modes '(genie-mode jython-mode)
  "*Used to determine if a buffer contains Genie source code.
If a file is loaded into a buffer that is in one of these major modes,
it is considered Genie source by `genie-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'genie)

(defvar genie-prev-dir/file nil
  "Caches (directory . file) pair used in the last `genie-load-file' command.
Used for determining the default in the next one.")

(autoload 'comint-get-source "comint")

(defun genie-load-file (file-name)
  "Load a Genie file FILE-NAME into the inferior Genie process.
If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive (comint-get-source "Load Genie file: " genie-prev-dir/file
				  genie-source-modes
				  t))	; because execfile needs exact name
  (comint-check-source file-name)     ; Check to see if buffer needs saving.
  (setq genie-prev-dir/file (cons (file-name-directory file-name)
				   (file-name-nondirectory file-name)))
  (with-current-buffer (process-buffer (genie-proc)) ;Runs genie if needed.
    (let ((world-writable
	   ;; Directory is world-writable (see `run-genie').
	   (/= 0 (logand 2 (file-modes default-directory)))))
      (if (genie-send-command
	   (if (and (string-match "\\.py\\'" file-name)
		    (not world-writable))
	       (let ((module (file-name-sans-extension
			      (file-name-nondirectory file-name))))
		 (format "emacs.eimport(%S,%S)"
			 module (file-name-directory file-name)))
	     (format "execfile(%S)" file-name)))
	  (if world-writable
	      (message "Not loading as module from world-writable directory")
	    (message "%s loaded" file-name))
	(message "Can't execute: not at prompt in inferior buffer")))))

(defun genie-proc ()
  "Return the current Genie process.
See variable `genie-buffer'.  Starts a new process if necessary."
  ;; Fixme: Maybe should look for another active process if there
  ;; isn't one for `genie-buffer'.
  (unless (comint-check-proc genie-buffer)
    (run-genie nil t))
  (get-buffer-process (or (if (eq major-mode 'inferior-genie-mode)
				  (current-buffer)
				genie-buffer))))

(defun genie-set-proc ()
  "Set the default value of `genie-buffer' to correspond to this buffer.
If the current buffer has a local value of `genie-buffer', set the
default (global) value to that.  The associated Genie process is
the one that gets input from \\[genie-send-region] et al when used
in a buffer that doesn't have a local value of `genie-buffer'."
  (interactive)
  (if (local-variable-p 'genie-buffer)
      (setq-default genie-buffer genie-buffer)
    (error "No local value of `genie-buffer'")))

;;;; Context-sensitive help.

(defconst genie-dotty-syntax-table
  (let ((table (copy-syntax-table genie-mode-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table giving `.' symbol syntax.
Otherwise inherits from `genie-mode-syntax-table'.")

(defvar view-return-to-alist)
(autoload 'help-buffer "help-mode")

;; Fixme: Should this actually be used instead of info-look, i.e. be
;; bound to C-h S?  [Probably not, since info-look may work in cases
;; where this doesn't.]
(defun genie-describe-symbol (symbol)
  "Get help on SYMBOL using `help'.
Interactively, prompt for symbol.

Symbol may be anything recognized by the interpreter's `help'
command -- e.g. `CALLS' -- not just variables in scope in the
interpreter.  This only works for Genie version 2.2 or newer
since earlier interpreters don't support `help'.

In some cases where this doesn't find documentation, \\[info-lookup-symbol]
will."
  ;; Note that we do this in the inferior process, not a separate one, to
  ;; ensure the environment is appropriate.
  (interactive
   (let ((symbol (with-syntax-table genie-dotty-syntax-table
		   (current-word)))
	 (enable-recursive-minibuffers t))
     (list (read-string (if symbol
			    (format "Describe symbol (default %s): " symbol)
			  "Describe symbol: ")
			nil nil symbol))))
  (if (equal symbol "") (error "No symbol"))
  ;; Ensure we have a suitable help buffer.
  ;; Fixme: Maybe process `Related help topics' a la help xrefs and
  ;; allow C-c C-f in help buffer.
  (let ((temp-buffer-show-hook		; avoid xref stuff
	 (lambda ()
	   (toggle-read-only 1)
	   (setq view-return-to-alist
		 (list (cons (selected-window) help-return-method))))))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
 	;; Fixme: Is this actually useful?
	(help-setup-xref (list 'genie-describe-symbol symbol) (interactive-p))
	(set (make-local-variable 'comint-redirect-subvert-readonly) t)
	(print-help-return-message))))
  (comint-redirect-send-command-to-process (format "emacs.ehelp(%S, %s)"
						   symbol (or genie-imports
							      "None"))
   "*Help*" (genie-proc) nil nil))

(add-to-list 'debug-ignored-errors "^No symbol")

(defun genie-send-receive (string)
  "Send STRING to inferior Genie (if any) and return result.
The result is what follows `_emacs_out' in the output (or nil).
This is a no-op if `genie-check-comint-prompt' returns nil."
  (let ((proc (genie-proc)))
    (when (genie-check-comint-prompt proc)
      ;; We typically lose if the inferior isn't in the normal REPL,
      ;; e.g. prompt is `help> ' or `(Pdb)'.  It actually needs to be
      ;; `>>> ', not `... ', i.e. we're not inputting a block &c.
      ;; This may not be the place to check it, e.g. we might actually
      ;; want to send commands having set up such a state, but
      ;; currently it's OK for all uses.
      (genie-send-string string)
      (setq genie-preoutput-result nil)
      (while (progn
	       (accept-process-output proc 5)
	       genie-preoutput-leftover))
      genie-preoutput-result)))

(defun genie-check-comint-prompt (&optional proc)
  "Return non-nil iff there's a normal prompt in the inferior buffer.
If there isn't, it's probably not appropriate to send input to return
Eldoc information etc.  If PROC is non-nil, check the buffer for that
process."
  (setq proc (or proc (genie-proc)))
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      ;; The prompt really should be `>>>', not `...', or `(pdb)',
      ;; say, else we can't sensibly send stuff to evaluate.
      (save-match-data (re-search-backward ">>> \\=" nil t)))))

;; Fixme:  Is there anything reasonable we can do with random methods?
;; (Currently only works with functions.)
(defun genie-eldoc-function ()
  "`eldoc-print-current-symbol-info' for Genie.
Only works when point is in a function name, not its arg list, for
instance.  Assumes an inferior Genie is running."
  (with-syntax-table genie-dotty-syntax-table
    (let ((symbol (current-word))
	  result)
      ;; First try the symbol we're on.
      (or (and symbol
	       (setq result
		     (genie-send-receive (format "emacs.eargs(%S, %s)"
						  symbol genie-imports))))
	  ;; Try moving to symbol before enclosing parens.
	  (let ((s (syntax-ppss)))
	    (unless (zerop (car s))
	      (when (eq ?\( (char-after (nth 1 s)))
		(save-excursion
		  (goto-char (nth 1 s))
		  (skip-syntax-backward "-")
		  (let ((point (point)))
		    (skip-syntax-backward "w_")
		    (if (< (point) point)
			(setq result
			      (genie-send-receive
			       (format "emacs.eargs(%S, %s)"
				       (buffer-substring-no-properties
					(point) point)
				       genie-imports))))))))))
      (if (equal result "")
	  nil
	result))))

;;;; Info-look functionality.

(eval-when-compile (require 'info))

(defun genie-after-info-look ()
  "Set up info-look for Genie.
Tries to take account of versioned Genie Info files, e.g. Debian's
genie2.5-ref.info.gz.
Used with `eval-after-load'."
  (let* ((version (let ((s (shell-command-to-string (concat genie-command
							    " -V"))))
		    (string-match "^Genie \\([0-9]+\\.[0-9]+\\>\\)" s)
		    (match-string 1 s)))
	 ;; Whether info files have a Genie version suffix, e.g. in Debian.
	 (versioned
	  (with-temp-buffer
	    (Info-mode)
	    ;; First look for Info files corresponding to the version
	    ;; of the interpreter we're running.
	    (condition-case ()
		;; Don't use `info' because it would pop-up a *info* buffer.
		(progn
		  (Info-goto-node (format "(genie%s-lib)Miscellaneous Index"
					  version))
		  t)
	      (error
	       ;; Otherwise see if we actually have an un-versioned one.
	       (condition-case ()
		   (progn
		     (Info-goto-node
		      (format "(genie-lib)Miscellaneous Index" version))
		     nil)
		 (error
		  ;; Otherwise look for any versioned Info file.
		  (condition-case ()
		      (let (found)
			(dolist (dir (or Info-directory-list
					 Info-default-directory-list))
			  (unless found
			    (let ((file (car (file-expand-wildcards
					      (expand-file-name "genie*-lib*"
								dir)))))
			      (if (and file
				       (string-match
					"\\<genie\\([0-9]+\\.[0-9]+\\>\\)-"
					file))
				  (setq version (match-string 1 file)
					found t)))))
			found)
		    (error)))))))))
    (info-lookup-maybe-add-help
     :mode 'genie-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
	 ;; The empty prefix just gets us highlighted terms.
	 `((,(concat "(genie" version "-ref)Miscellaneous Index"))
	   (,(concat "(genie" version "-ref)Module Index"))
	   (,(concat "(genie" version "-ref)Function-Method-Variable Index"))
	   (,(concat "(genie" version "-ref)Class-Exception-Object Index"))
	   (,(concat "(genie" version "-lib)Module Index"))
	   (,(concat "(genie" version "-lib)Class-Exception-Object Index"))
	   (,(concat "(genie" version "-lib)Function-Method-Variable Index"))
	   (,(concat "(genie" version "-lib)Miscellaneous Index")))
       '(("(genie-ref)Miscellaneous Index")
	 ("(genie-ref)Module Index")
	 ("(genie-ref)Function-Method-Variable Index")
	 ("(genie-ref)Class-Exception-Object Index")
	 ("(genie-lib)Module Index")
	 ("(genie-lib)Class-Exception-Object Index")
	 ("(genie-lib)Function-Method-Variable Index")
	 ("(genie-lib)Miscellaneous Index"))))))
(eval-after-load "info-look" '(genie-after-info-look))

;;;; Miscellany.

(defcustom genie-jython-packages '("java" "javax" "org" "com")
  "Packages implying `jython-mode'.
If these are imported near the beginning of the buffer, `genie-mode'
actually punts to `jython-mode'."
  :type '(repeat string)
  :group 'genie)

;; Called from `genie-mode', this causes a recursive call of the
;; mode.  See logic there to break out of the recursion.
(defun genie-maybe-jython ()
  "Invoke `jython-mode' if the buffer appears to contain Jython code.
The criterion is either a match for `jython-mode' via
`interpreter-mode-alist' or an import of a module from the list
`genie-jython-packages'."
  ;; The logic is taken from genie-mode.el.
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((interpreter (if (looking-at auto-mode-interpreter-regexp)
			     (match-string 2))))
	(if (and interpreter (eq 'jython-mode
				 (cdr (assoc (file-name-nondirectory
					      interpreter)
					     interpreter-mode-alist))))
	    (jython-mode)
	  (if (catch 'done
		(while (re-search-forward
			(rx line-start (or "import" "from") (1+ space)
			    (group (1+ (not (any " \t\n.")))))
			(+ (point-min) 10000) ; Probably not worth customizing.
			t)
		  (if (member (match-string 1) genie-jython-packages)
		      (throw 'done t))))
	      (jython-mode)))))))

(defun genie-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation."
  (interactive "P")
  (or (fill-comment-paragraph justify)
      (save-excursion
	(end-of-line)
	(let* ((syntax (syntax-ppss))
	       (orig (point))
	       start end)
	  (cond ((nth 4 syntax)	; comment.   Fixme: loses with trailing one
		 (let (fill-paragraph-function)
		   (fill-paragraph justify)))
		;; The `paragraph-start' and `paragraph-separate'
		;; variables don't allow us to delimit the last
		;; paragraph in a multi-line string properly, so narrow
		;; to the string and then fill around (the end of) the
		;; current line.
		((eq t (nth 3 syntax))	; in fenced string
		 (goto-char (nth 8 syntax)) ; string start
		 (setq start (line-beginning-position))
		 (condition-case ()	; for unbalanced quotes
		     (progn (forward-sexp)
			    (setq end (- (point) 3)))
		   (error (setq end (point-max)))))
		((re-search-backward "\\s|\\s-*\\=" nil t) ; end of fenced string
		 (forward-char)
		 (setq end (point))
		 (condition-case ()
		     (progn (backward-sexp)
			    (setq start (line-beginning-position)))
		   (error nil))))
	  (when end
	    (save-restriction
	      (narrow-to-region start end)
	      (goto-char orig)
	      ;; Avoid losing leading and trailing newlines in doc
	      ;; strings written like:
	      ;;   """
	      ;;   ...
	      ;;   """
	      (let ((paragraph-separate
		     ;; Note that the string could be part of an
		     ;; expression, so it can have preceding and
		     ;; trailing non-whitespace.
		     (concat
		      (rx (or
			   ;; Opening triple quote without following text.
			   (and (* nonl)
				(group (syntax string-delimiter))
				(repeat 2 (backref 1))
				;; Fixme:  Not sure about including
				;; trailing whitespace.
				(* (any " \t"))
				eol)
			   ;; Closing trailing quote without preceding text.
			   (and (group (any ?\" ?')) (backref 2)
				(syntax string-delimiter))))
		      "\\(?:" paragraph-separate "\\)"))
		    fill-paragraph-function)
		(fill-paragraph justify))))))) t)

(defun genie-shift-left (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the left.
COUNT defaults to `genie-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie.  It is an error if any lines in the region are indented less than
COUNT columns."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) current-prefix-arg)
		 (list (line-beginning-position) (line-end-position 2)
		       current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count genie-indent))
  (when (> count 0)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(if (and (< (current-indentation) count)
		 (not (looking-at "[ \t]*$")))
	    (error "Can't shift all lines enough"))
	(forward-line))
      (indent-rigidly start end (- count)))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun genie-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `genie-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) current-prefix-arg)
		 (list (line-beginning-position) (line-end-position 2)
		       current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count genie-indent))
  (indent-rigidly start end count))

(defun genie-outline-level ()
  "`outline-level' function for Genie mode.
The level is the number of `genie-indent' steps of indentation
of current line."
  (1+ (/ (current-indentation) genie-indent)))

;; Fixme: Consider top-level assignments, imports, &c.
(defun genie-current-defun ()
  "`add-log-current-defun-function' for Genie."
  (save-excursion
    ;; Move up the tree of nested `class' and `def' blocks until we
    ;; get to zero indentation, accumulating the defined names.
    (let ((progress t)
	  accum beg)
      (while (and progress (or (not beg) (> (current-indentation) 0)))
	(genie-beginning-of-block)
	(end-of-line)
	(beginning-of-defun)
	(if beg
	    (setq progress (< (point) beg)))
	(when progress
	  (if (looking-at (rx (0+ space) (or "def" "class") (1+ space)
			      (group (1+ (or word (syntax symbol))))))
	      ;; -no-properties not correct (e.g. composition), but
	      ;; consistent with `add-log-current-defun'
	      (push (match-string-no-properties 1) accum)))
	(setq beg (point)))
      (if accum (mapconcat 'identity accum ".")))))

(defun genie-mark-block ()
  "Mark the block around point and go to the beginning of it.
Do nothing if not in a block.
Uses `genie-beginning-of-block', `genie-end-of-block'."
  (interactive)
  (let ((point (point))
	(active mark-active))
    (genie-beginning-of-block)
    (if  (and (= point (point))
	      (not (genie-open-block-statement-p)))
	;; Didn't move, and not at very start of a top-level block.
	(progn (pop-mark)
	       (setq mark-active active))
      (push-mark point)
      (push-mark (point) nil t)
      (genie-end-of-block)
      (exchange-point-and-mark))))


;;;; Completion.

(defvar genie-imports nil
  "String of top-level import statements updated by `genie-find-imports'.")
(make-variable-buffer-local 'genie-imports)

;; font-lock could try to run this when it deals with an import, but
;; that's probably not a good idea if it gets run multiple times when
;; the statement is being edited, and is more likely to end up with
;; something syntactically incorrect.
;; Fixme: currently doesn't deal with constructs which are effectively
;; at top-level:
;; if ...:
;;     import ...
(defun genie-find-imports ()
  "Find top-level import statements, updating `genie-imports'."
  (interactive)
  (save-excursion
    (let (lines)
      (goto-char (point-min))
      (while (re-search-forward "^import\\>\\|^from\\>" nil t)
	(unless (syntax-ppss-context (syntax-ppss)) ; avoid comment/string
	  (let ((start (line-beginning-position)))
	    ;; Skip over continued lines.
	    (while (and (eq ?\\ (char-before (line-end-position)))
			(= 0 (forward-line 1)))
	      t)
	    (push (buffer-substring-no-properties
		   start (line-beginning-position 2))
		  lines))))
      (setq genie-imports
	    (if lines
		(apply #'concat
;; This is probably best left out since you're unlikely to need the
;; doc for a function in the buffer and the import will lose if the
;; Genie sub-process' working directory isn't the same as the
;; buffer's.
;; 			 (if buffer-file-name
;; 			     (concat
;; 			      "import "
;; 			      (file-name-sans-extension
;; 			       (file-name-nondirectory buffer-file-name))))
		       (nreverse lines))
	      "None"))
      (when lines
	;; The output ends up in the wrong place if the string we
	;; send contains newlines (from the imports).
	(setq genie-imports
	      (replace-regexp-in-string "\n" "\\n"
					;; format does quoting
					(format "%S" genie-imports) t t))))))

;; Fixme:  This assumes imports precede the use of the relevant names,
;; which isn't necessary with the strange Genie scoping.  Rather than
;; simply declare that we should have import before use, we could also look
;; forward in the enclosing blocks for imports.
(defun genie-find-local-imports ()
  "Find import statements apparently in scope and return string of them.
The criterion is that import statements found looking backwards
in surrounding blocks are indented at the top level of the block."
  (if (eq major-mode 'inferior-genie-mode)
      "None"
    (save-excursion
      (if (or (genie-blank-line-p) (genie-comment-line-p))
	  (genie-previous-statement))
      (let ((to (point))	 ; search to here for each block level
	    (indent (current-indentation))
	    bob			   ; beginning-of-block for each level
	    lines)			; accumulated import lines
	(while (genie-beginning-of-block)
	  (setq bob (point))
	  (save-excursion
	    (while (re-search-forward "^\\s-+\\(?:import\\|from\\)\\>" to t)
	      (unless (or (syntax-ppss-context (syntax-ppss)) ; avoid comment/string
			  (> (current-indentation) indent)) ; right scope
		(let ((start (save-excursion
			       (back-to-indentation)
			       (point))))
		  ;; Skip over continued lines.
		  (while (and (eq ?\\ (char-before (line-end-position)))
			      (= 0 (forward-line 1)))
		    t)
		  (push (buffer-substring-no-properties
			 start (line-beginning-position 2))
			lines)))))
	  (setq to bob
		indent (current-indentation)))
	(if lines			; Per genie-find-imports
	    (let* ((lines (format "%S" (apply #'concat lines)))
		   (lines (replace-regexp-in-string "\n" "\\n" lines t t)))
	      (if (equal genie-imports "None")
		  lines
		(concat genie-imports lines)))
	  genie-imports)))))

;; Fixme: This fails the first time if the sub-process isn't already
;; running.  Presumably a timing issue with i/o to the process.
(defun genie-symbol-completions (symbol)
  "Return a list of completions of the string SYMBOL from Genie process.
The list is sorted.
Uses `genie-imports' to load modules against which to complete."
  (when symbol
    (let ((completions
	   (condition-case ()
	       (car (read-from-string
		     (genie-send-receive
		      (format "emacs.complete(%S,%s)" symbol
			      (genie-find-local-imports)))))
	     (error nil))))
      (sort
       ;; We can get duplicates from the above -- don't know why.
       (delete-dups completions)
       #'string<))))

(defun genie-partial-symbol ()
  "Return the partial symbol before point (for completion)."
  (let ((end (point))
	(start (save-excursion
		 (and (re-search-backward
		       (rx (or buffer-start (regexp "[^[:alnum:]._]"))
			   (group (1+ (regexp "[[:alnum:]._]"))) point)
		       nil t)
		      (match-beginning 1)))))
    (if start (buffer-substring-no-properties start end))))

;;;; FFAP support

(defun genie-module-path (module)
  "Function for `ffap-alist' to return path to MODULE."
  (genie-send-receive (format "emacs.modpath (%S)" module)))

(eval-after-load "ffap"
  '(push '(genie-mode . genie-module-path) ffap-alist))

;;;; Find-function support

;; Fixme: key binding?

(defun genie-find-function (name)
  "Find source of definition of function NAME.
Interactively, prompt for name."
  (interactive
   (let ((symbol (with-syntax-table genie-dotty-syntax-table
		   (current-word)))
	 (enable-recursive-minibuffers t))
     (list (read-string (if symbol
			    (format "Find location of (default %s): " symbol)
			  "Find location of: ")
			nil nil symbol))))
  (unless genie-imports
    (error "Not called from buffer visiting Genie file"))
  (let* ((loc (genie-send-receive (format "emacs.location_of (%S, %s)"
					   name genie-imports)))
	 (loc (if loc (car (read-from-string loc))))
	 (file (car loc))
	 (line (cdr loc)))
    (unless file (error "Don't know where `%s' is defined" name))
    (pop-to-buffer (find-file-noselect file))
    (when (integerp line)
      (goto-line line))))

;;;; Skeletons

(defvar genie-skeletons nil
  "Alist of named skeletons for Genie mode.
Elements are of the form (NAME . EXPANDER-FUNCTION).")

(defvar genie-mode-abbrev-table nil
  "Abbrev table for Genie mode.
The default contents correspond to the elements of `genie-skeletons'.")
(define-abbrev-table 'genie-mode-abbrev-table ())

(eval-when-compile
  ;; Define a user-level skeleton and add it to `genie-skeletons' and
  ;; the abbrev table.
(defmacro def-genie-skeleton (name &rest elements)
  (let* ((name (symbol-name name))
	 (function (intern (concat "genie-insert-" name))))
    `(progn
       (add-to-list 'genie-skeletons ',(cons name function))
       ;; Usual technique for inserting a skeleton, but expand
       ;; to the original abbrev instead if in a comment or string.
       (define-abbrev genie-mode-abbrev-table ,name ""
	 ;; Quote this to give a readable abbrev table.
	 '(lambda ()
	    (if (or (genie-in-string/comment)
		    ;; Only expand at beginning of statement,
		    ;; avoiding both instances like `foo_class'
		    ;; and Genie 2.5's `if' expressions.
		    (/= (current-column) (current-indentation)))
		(insert ,name)
	      (,function)))
	 nil t)				; system abbrev
       (define-skeleton ,function
	 ,(format "Insert Genie \"%s\" template." name)
	 ,@elements)))))
(put 'def-genie-skeleton 'lisp-indent-function 2)

;; From `skeleton-further-elements' set below:
;;  `<': outdent a level;
;;  `^': delete indentation on current line and also previous newline.
;;       Not quite like `delete-indentation'.  Assumes point is at
;;       beginning of indentation.

(def-genie-skeleton if
  "Condition: "
  "if " str ":" \n
  > -1	   ; Fixme: I don't understand the spurious space this removes.
  _ \n
  ("other condition, %s: "
   <			; Avoid wrong indentation after block opening.
   "elif " str ":" \n
   > _ \n nil)
  '(genie-else) | ^)

(define-skeleton genie-else
  "Auxiliary skeleton."
  nil
  (unless (eq ?y (read-char "Add `else' clause? (y for yes or RET for no) "))
    (signal 'quit t))
  < "else:" \n
  > _ \n)

(def-genie-skeleton while
  "Condition: "
  "while " str ":" \n
  > -1 _ \n
  '(genie-else) | ^)

(def-genie-skeleton for
  "Target, %s: "
  "for " str " in " (skeleton-read "Expression, %s: ") ":" \n
  > -1 _ \n
  '(genie-else) | ^)

(def-genie-skeleton try/except
  nil
  "try:" \n
  > -1 _ \n
  ("Exception, %s: "
   < "except " str '(genie-target) ":" \n
   > _ \n nil)
  < "except:" \n
  > _ \n
  '(genie-else) | ^)

(define-skeleton genie-target
  "Auxiliary skeleton."
  "Target, %s: " ", " str | -2)

(def-genie-skeleton try/finally
  nil
  "try:" \n
  > -1 _ \n
  < "finally:" \n
  > _ \n)

(def-genie-skeleton def
  "Name: "
  "def " str " (" ("Parameter, %s: " (unless (equal ?\( (char-before)) ", ")
		     str) "):" \n
  "\"\"\"" - "\"\"\"" \n     ; Fixme:  extra space inserted -- why?).
  > _ \n)

(def-genie-skeleton class
  "Name: "
  "class " str " (" ("Inheritance, %s: "
		     (unless (equal ?\( (char-before)) ", ")
		     str)
  & ")" | -2				; close list or remove opening
  ":" \n
  "\"\"\"" - "\"\"\"" \n
  > _ \n)

(defvar genie-default-template "if"
  "Default template to expand by `genie-expand-template'.
Updated on each expansion.")

(defun genie-expand-template (name)
  "Expand template named NAME.
Interactively, prompt for the name with completion."
  (interactive
   (list (completing-read (format "Template to expand (default %s): "
				  genie-default-template)
			  genie-skeletons nil t)))
  (if (equal "" name)
      (setq name genie-default-template)
    (setq genie-default-template name))
  (let ((func (cdr (assoc name genie-skeletons))))
    (if func
	(funcall func)
      (error "Undefined template: %s" name))))

;;;; Bicycle Repair Man support

(autoload 'pymacs-load "pymacs" nil t)
(autoload 'brm-init "bikemacs")

;; I'm not sure how useful BRM really is, and it's certainly dangerous
;; the way it modifies files outside Emacs...  Also note that the
;; current BRM loses with tabs used for indentation -- I submitted a
;; fix <URL:http://www.loveshack.ukfsn.org/emacs/bikeemacs.py.diff>.
(defun genie-setup-brm ()
  "Set up Bicycle Repair Man refactoring tool (if available).

Note that the `refactoring' features change files independently of
Emacs and may modify and save the contents of the current buffer
without confirmation."
  (interactive)
  (condition-case data
      (unless (fboundp 'brm-rename)
	(pymacs-load "bikeemacs" "brm-") ; first line of normal recipe
	(let ((py-mode-map (make-sparse-keymap)) ; it assumes this
	      (features (cons 'genie-mode features))) ; and requires this
	  (brm-init)			; second line of normal recipe
	  (remove-hook 'genie-mode-hook ; undo this from `brm-init'
		       '(lambda () (easy-menu-add brm-menu)))
	  (easy-menu-define
	    genie-brm-menu genie-mode-map
	    "Bicycle Repair Man"
	    '("BicycleRepairMan"
	      :help "Interface to navigation and refactoring tool"
	      "Queries"
	      ["Find References" brm-find-references
	       :help "Find references to name at point in compilation buffer"]
	      ["Find Definition" brm-find-definition
	       :help "Find definition of name at point"]
	      "-"
	      "Refactoring"
	      ["Rename" brm-rename
	       :help "Replace name at point with a new name everywhere"]
	      ["Extract Method" brm-extract-method
	       :active (and mark-active (not buffer-read-only))
	       :help "Replace statements in region with a method"]
	      ["Extract Local Variable" brm-extract-local-variable
	       :active (and mark-active (not buffer-read-only))
	       :help "Replace expression in region with an assignment"]
	      ["Inline Local Variable" brm-inline-local-variable
	       :help
	       "Substitute uses of variable at point with its definition"]
	      ;; Fixme:  Should check for anything to revert.
	      ["Undo Last Refactoring" brm-undo :help ""]))))
    (error (error "BicycleRepairMan setup failed: %s" data))))

;;;; Modes.

(defvar outline-heading-end-regexp)
(defvar eldoc-documentation-function)

;; Stuff to allow expanding abbrevs with non-word constituents.
(defun genie-abbrev-pc-hook ()
  "Reset the syntax table after possibly expanding abbrevs."
  (remove-hook 'post-command-hook 'genie-abbrev-pc-hook t)
  (set-syntax-table genie-mode-syntax-table))

(defvar genie-abbrev-syntax-table
  (copy-syntax-table genie-mode-syntax-table)
  "Syntax table used when expanding abbrevs.")

(defun genie-pea-hook ()
  "Set the syntax table before possibly expanding abbrevs."
  (set-syntax-table genie-abbrev-syntax-table)
  (add-hook 'post-command-hook 'genie-abbrev-pc-hook nil t))
(modify-syntax-entry ?/ "w" genie-abbrev-syntax-table)

(defvar genie-outline-regexp
  (rx (* space) (or "class" "def" "elif" "else" "except" "finally"
		    "for" "if" "try" "while" "with")
      symbol-end))

(defcustom genie-default-version 2
  "Which version of the language is supported by \\[genie-mode].
Either `2' (for the Genie 2.6 language) or `3' (for the Genie 3.0
language)."
  :group 'genie
  :type '(choice (const 2) (const 3)))

(defvar genie-version nil
  "The Genie language version set for the current Genie mode buffer.
It stores the value of `genie-default-version' when `genie-mode'
was invoked.")

;;;###autoload
(define-derived-mode genie-mode fundamental-mode "Genie"
  "Major mode for editing Genie files.

The version of the language supported depends on
`genie-default-version' (q.v.).  `genie-2-mode' and `genie-3-mode'
invoke `genie-mode' with the appropriate version set, and are
particularly useful in file variables to specify the language in the
file.

`genie-genie-command' determines which interpreter is used by the
Genie sub-shell, and you may want to ensure it corresponds to the
language version you edit if version 2 and 3 variants are both
available.  `jython-mode', which is invoked if the buffer appears to
contain Jython code, differs from `genie-mode' only in setting
`genie-genie-command'.

Turns on Font Lock mode unconditionally since it is currently required
for correct parsing of the source.
See also `run-genie' and associated Genie mode
commands for running Genie under Emacs.

The Emacs commands which work with `defun's, e.g. \\[beginning-of-defun], deal
with nested `def' and `class' blocks.  They take the innermost one as
current without distinguishing method and class definitions.  Used multiple
times, they move over others at the same indentation level until they reach
the end of definitions at that level, when they move up a level.
\\<genie-mode-map>
Colon is electric: it outdents the line if appropriate, e.g. for
an else statement.  \\[genie-backspace] at the beginning of an indented statement
deletes a level of indentation to close the current block; otherwise it
deletes a character backward.  TAB indents the current line relative to
the preceding code.  Successive TABs, with no intervening command, cycle
through the possibilities for indentation on the basis of enclosing blocks.

\\[fill-paragraph] fills comments and multi-line strings appropriately, but has no
effect outside them.

Supports Eldoc mode (only for functions, using a Genie process),
Info-Look and Imenu.  (Unfortunately the Info docs required for
info-look aren't currently available for Genie versions after 2.5.)
In Outline minor mode, `class' and `def' lines count as headers.

Symbol completion is available similarly to the `rlcompleter' module
in the Genie shell.  This completion is added to the Hippie Expand
functions locally if Hippie Expand mode is turned on.  Completion of
symbols of the form x.y only works if the components are literal
module/attribute names, not variables.  An abbrev table is set up with
skeleton expansions for compound statement templates.  Turn on Abbrev
mode to use it.

\\{genie-mode-map}"
  :group 'genie
  (set (make-local-variable 'genie-font-lock-keywords)
       (append genie-font-lock-keywords
	       (if (equal 2 genie-default-version)
		   genie-2-font-lock-keywords
		 genie-3-font-lock-keywords)))
  (set (make-local-variable 'font-lock-defaults)
       '(genie-font-lock-keywords nil nil nil nil
				   (font-lock-syntactic-keywords
				    . genie-font-lock-syntactic-keywords)
				   ;; This probably isn't worth it.
				   ;; (font-lock-syntactic-face-function
				   ;;  . genie-font-lock-syntactic-face-function)
				   ))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *")
  (set (make-local-variable 'comment-end-skip)
       "[ \t]*\\(\\s>\\|\n\\)")
  (set (make-local-variable 'indent-line-function) #'genie-indent-line)
  (set (make-local-variable 'indent-region-function) #'genie-indent-region)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'genie-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'add-log-current-defun-function)
       #'genie-current-defun)
  (set (make-local-variable 'outline-regexp) genie-outline-regexp)
  (set (make-local-variable 'outline-heading-end-regexp) ":\\s-*\\(\n\\|#\\)")
  (set (make-local-variable 'outline-level) #'genie-outline-level)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (make-local-variable 'genie-saved-check-command)
  (set (make-local-variable 'beginning-of-defun-function)
       'genie-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'genie-end-of-defun)
  (setq imenu-create-index-function #'genie-imenu-create-index)
  (set (make-local-variable 'imenu-sort-function) 'genie-imenu-sort-function)
  (set (make-local-variable 'eldoc-documentation-function)
       #'genie-eldoc-function)
  (add-hook 'eldoc-mode-hook
	    '(lambda () (run-genie nil t)) ; need it running
	    nil t)
  (set (make-local-variable 'symbol-completion-symbol-function)
       'genie-partial-symbol)
  (set (make-local-variable 'symbol-completion-completions-function)
       'genie-symbol-completions)
  (set (make-local-variable 'skeleton-further-elements)
       '((< '(backward-delete-char-untabify (min genie-indent
						 (current-column))))
	 (^ '(- (1+ (current-indentation))))))
  ;; Assumed by some parsing, I think.
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (setq mode-name
	(if (equal genie-default-version 2) "Genie2" "Genie3"))
  (set (make-local-variable 'genie-version) genie-default-version)
  (add-hook 'pre-abbrev-expand-hook 'genie-pea-hook nil t)
  (if (featurep 'hippie-exp)
      (set (make-local-variable 'hippie-expand-try-functions-list)
	   (cons 'symbol-completion-try-complete
		 hippie-expand-try-functions-list)))
  (unless font-lock-mode (font-lock-mode 1))
  (when genie-guess-indent
    (unless (local-variable-p 'genie-indent) ; file local variable
      (genie-guess-indent)))
  (set (make-local-variable 'genie-command) genie-genie-command)
  (genie-find-imports)
  (unless (boundp 'genie-mode-running)	; kill the recursion from jython-mode
    (let ((genie-mode-running t))
      (genie-maybe-jython))))

(defun genie-2-mode ()
  "Turn on Genie mode with Genie 2 keywords."
  (interactive)
  (let ((genie-default-version 2))
    (genie-mode)))

(defun genie-3-mode ()
  "Turn on Genie mode with Genie 3 keywords."
  (interactive)
  (let ((genie-default-version 3))
    (genie-mode)))

;; Not done automatically in Emacs 21 or 22.
(defcustom genie-mode-hook nil
  "Hook run when entering Genie mode."
  :group 'genie
  :type 'hook)
(custom-add-option 'genie-mode-hook 'imenu-add-menubar-index)
(custom-add-option 'genie-mode-hook
		   '(lambda ()
		      "Turn off Indent Tabs mode."
		      (setq indent-tabs-mode nil)))
(custom-add-option 'genie-mode-hook 'turn-on-eldoc-mode)
(custom-add-option 'genie-mode-hook 'abbrev-mode)
(custom-add-option 'genie-mode-hook 'genie-setup-brm)
(custom-add-option 'genie-mode-hook 'flyspell-prog-mode)
(custom-add-option 'genie-mode-hook 'flymake-mode)
(when (fboundp 'capitalized-words-mode)
  (custom-add-option 'genie-mode-hook 'capitalized-words-mode))

;; This seems to be of limited use since it isn't (can't be)
;; indentation-based.  Also hide-level doesn't seem to work properly.
(dolist (mode '(genie-mode jython-mode))
  (add-to-list 'hs-special-modes-alist
	       (list mode
		     "^\\s-*\\(?:def\\|class\\|if\\|else\\|elif\\|while\\|\
for\\|try\\|except\\|finally\\|with\\)\\>"
	       nil "#"
	       (lambda (arg)
		 (genie-end-of-block)
		 (skip-chars-backward " \t\n"))
	       nil)))

;;;###autoload
(define-derived-mode jython-mode genie-mode  "Jython"
  "Major mode for editing Jython files.
Like `genie-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `genie-mode-hook'."
  :group 'genie
  (set (make-local-variable 'genie-command) genie-jython-command))

(provide 'genie)
(provide 'genie-21)

;;; genie.el ends here
