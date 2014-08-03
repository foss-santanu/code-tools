
## Rudimentary Documentation for XREF.LISP

*Containing Directory /host/santanu/programming/Lisp/code-tools/xref/*

### API Documentation


#### LOOKUP (symbol environment)                                     [FUNCTION]

---

#### CAR-EQ (list item)                                              [FUNCTION]

---

#### \*FILE-CALLERS-DATABASE\* ((make-hash-table :test #'equal))     [VARIABLE]
>      
>      Contains name and list of file callers (files which call) for that
>      name. 

---

#### \*CALLERS-DATABASE\* ((make-hash-table :test #'equal))          [VARIABLE]
>      
>      Contains name and list of callers (function invocation) for that
>      name. 

---

#### \*READERS-DATABASE\* ((make-hash-table :test #'equal))          [VARIABLE]
>      
>      Contains name and list of readers (variable use) for that name.

---

#### \*SETTERS-DATABASE\* ((make-hash-table :test #'equal))          [VARIABLE]
>      
>      Contains name and list of setters (variable mutation) for that name.

---

#### \*CALLEES-DATABASE\* ((make-hash-table :test #'equal))          [VARIABLE]
>      
>      Contains name and list of functions and variables it calls.

---

#### CALLERS-LIST (name &optional (database :callers))               [FUNCTION]

---

#### (SETF CALLERS-LIST) (caller)                                [SETF MAPPING]

---

#### LIST-CALLERS (symbol)                                           [FUNCTION]
>      
>      Lists all functions which call SYMBOL as a function (function
>      invocation). 

---

#### LIST-READERS (symbol)                                           [FUNCTION]
>      
>      Lists all functions which refer to SYMBOL as a variable
>      (variable reference).

---

#### LIST-SETTERS (symbol)                                           [FUNCTION]
>      
>      Lists all functions which bind/set SYMBOL as a variable
>      (variable mutation).

---

#### LIST-USERS (symbol)                                             [FUNCTION]
>      
>      Lists all functions which use SYMBOL as a variable or function.

---

#### WHO-CALLS (symbol &optional how)                                [FUNCTION]
>      
>      Lists callers of symbol. HOW may be :function, :reader, :setter,
>      or :variable.

---

#### WHAT-FILES-CALL (symbol)                                        [FUNCTION]
>      
>      Lists names of files that contain uses of SYMBOL
>      as a function, variable, or constant.

---

#### LIST-CALLEES (symbol)                                           [FUNCTION]
>      
>      Lists names of functions and variables called by SYMBOL.

---

#### \*SOURCE-FILE\* ((make-hash-table :test #'equal))               [VARIABLE]
>      
>      Contains function name and source file for that name.

---

#### SOURCE-FILE (symbol)                                            [FUNCTION]
>      
>      Lists the names of files in which SYMBOL is defined/used.

---

#### (SETF SOURCE-FILE) (value)                                  [SETF MAPPING]

---

#### CLEAR-TABLES ()                                                 [FUNCTION]

---

#### \*PATTERN-CALLER-TYPE\* ((make-hash-table :test #'equal))       [VARIABLE]

---

#### PATTERN-CALLER-TYPE (name)                                      [FUNCTION]

---

#### (SETF PATTERN-CALLER-TYPE) (value)                          [SETF MAPPING]

---

#### \*PATTERN-SUBSTITUTION-TABLE\* ((make-hash-table :test #'equal))  [VARIABLE]
>      
>      Stores general patterns for function destructuring.

---

#### LOOKUP-PATTERN-SUBSTITUTION (name)                              [FUNCTION]

---

#### DEFINE-PATTERN-SUBSTITUTION (name pattern)                         [MACRO]
>      
>      Defines NAME to be equivalent to the specified pattern. Useful for
>      making patterns more readable. For example, the LAMBDA-LIST is
>      defined as a pattern substitution, making the definition of the
>      DEFUN caller-pattern simpler.

---

#### \*CALLER-PATTERN-TABLE\* ((make-hash-table :test #'equal))      [VARIABLE]
>      
>      Stores patterns for function destructuring.

---

#### LOOKUP-CALLER-PATTERN (name)                                    [FUNCTION]

---

#### DEFINE-CALLER-PATTERN (name pattern &optional caller-type)         [MACRO]
>      
>      Defines NAME as a function/macro call with argument structure
>      described by PATTERN. CALLER-TYPE, if specified, assigns a type to
>      the pattern, which may be used to exclude references to NAME while
>      viewing the database. For example, all the Common Lisp definitions
>      have a caller-type of :lisp or :lisp2, so that you can exclude
>      references to common lisp functions from the calling tree.

---

#### DEFINE-VARIABLE-PATTERN (name &optional caller-type)               [MACRO]
>      
>      Defines NAME as a variable reference of type CALLER-TYPE. This is
>      mainly used to establish the caller-type of the variable.

---

#### DEFINE-CALLER-PATTERN-SYNONYMS (source destinations)               [MACRO]
>      
>      For defining function caller pattern syntax synonyms. For each name
>      in DESTINATIONS, defines its pattern as a copy of the definition
>      of SOURCE. Allows a large number of identical patterns to be defined
>      simultaneously. Must occur after the SOURCE has been defined.

---

#### CLEAR-PATTERNS ()                                               [FUNCTION]

---

#### \*LAST-FORM\* (nil)                                             [VARIABLE]
>      
>      The last form read from the file. Useful for figuring out what went
>      wrong when xref-file drops into the debugger.

---

#### \*XREF-VERBOSE\* (t)                                            [VARIABLE]
>      
>      When T, xref-file(s) prints out the names of the files it looks at,
>      progress dots, and the number of forms read.

---

#### XREF-FILES (&rest files)                                        [FUNCTION]
>      
>      Grovels over the lisp code located in source file FILES, using
>      xref-file. 

---

#### \*HANDLE-PACKAGE-FORMS\* (nil)                                  [VARIABLE]
>      
>      When non-NIL, and XREF-FILE sees a package-setting form like
>      IN-PACKAGE, sets the current package to the specified package by
>      evaluating the form. When done with the file, xref-file resets the
>      package to its original value. In some of the displaying functions,
>      when this variable is non-NIL one may specify that all symbols from a
>      particular set of packages be ignored. This is only useful if the
>      files use different packages with conflicting names.

---

#### \*NORMAL-READTABLE\* ((copy-readtable nil))                     [VARIABLE]
>      
>      Normal, unadulterated CL readtable.

---

#### XREF-FILE (filename &optional (clear-tables t) (verbose *xref-verbose*))  [FUNCTION]
>      
>      Cross references the function and variable calls in FILENAME by
>      walking over the source code located in the file. Defaults type of
>      filename to ".lisp". Chomps on the code using record-callers and
>      record-callers*. If CLEAR-TABLES is T (the default), it clears the
>      callers database before processing the file. Specify CLEAR-TABLES as
>      nil to append to the database. If VERBOSE is T (the default), prints
>      out the name of the file, one progress dot for each form processed,
>      and the total number of forms.

---

#### \*HANDLE-FUNCTION-FORMS\* (t)                                   [VARIABLE]
>      
>      When T, XREF-FILE tries to be smart about forms which occur in
>      a function position, such as lambdas and arbitrary Lisp forms.
>      If so, it recursively calls record-callers with pattern 'FORM.
>      If the form is a lambda, makes the caller a caller of
>      :unnamed-lambda. 

---

#### \*HANDLE-MACRO-FORMS\* (t)                                      [VARIABLE]
>      
>      When T, if the file was loaded before being processed by XREF, and
>      the car of a form is a macro, it notes that the parent calls the
>      macro, and then calls macroexpand-1 on the form.

---

#### \*CALLEES-DATABASE-INCLUDES-VARIABLES\* (nil)                   [VARIABLE]

---

#### RECORD-CALLERS (filename form &optional pattern parent (environment nil) funcall)  [FUNCTION]
>      
>      RECORD-CALLERS is the main routine used to walk down the code. It
>      matches the PATTERN against the FORM, possibly adding statements to
>      the database. PARENT is the name defined by the current outermost
>      definition; it is the caller of the forms in the body (e.g., FORM).
>      ENVIRONMENT is used to keep track of the scoping of variables.
>      FUNCALL deals with the type of variable assignment and hence how the
>      environment should be modified. RECORD-CALLERS handles atomic
>      patterns and simple list-structure patterns. For complex
>      list-structure pattern destructuring, it calls RECORD-CALLERS*. 

---

#### RECORD-CALLERS\* (filename form pattern parent environment &optional continuation in-optionals in-keywords)  [FUNCTION]
>      
>      RECORD-CALLERS* handles complex list-structure patterns, such as
>      ordered lists of subpatterns, patterns involving :star, :plus,
>      &optional, &key, &rest, and so on. CONTINUATION is a stack of
>      unprocessed patterns, IN-OPTIONALS and IN-KEYWORDS are
>      corresponding stacks which determine whether &rest or &key has been
>      seen yet in the current pattern.

---

#### \*TYPES-TO-IGNORE\* ('(:lisp :lisp2))                           [VARIABLE]
>      
>      Default set of caller types (as specified in the patterns) to ignore
>      in the database handling functions. :lisp is CLtL 1st edition,
>      :lisp2 is additional patterns from CLtL 2nd edition.

---

#### DISPLAY-DATABASE (&optional (database :callers) (types-to-ignore *types-to-ignore*))  [FUNCTION]
>      
>      Prints out the name of each symbol and all its callers. Specify
>      database :callers (the default) to get function call references,
>      :fill to the get files in which the symbol is called, :readers to get
>      variable references, and :setters to get variable binding and
>      assignments. Ignores functions of types listed in types-to-ignore.

---

#### WRITE-CALLERS-DATABASE-TO-FILE (filename)                       [FUNCTION]
>      
>      Saves the contents of the current callers database to a file. This
>      file can be loaded to restore the previous contents of the
>      database. (For large systems it can take a long time to crunch
>      through the code, so this can save some time.)

---

#### INVERT-HASH-TABLE (table &optional (types-to-ignore *types-to-ignore*))  [FUNCTION]
>      
>      Makes a copy of the hash table in which (name value*) pairs
>      are inverted to (value name*) pairs.

---

#### DETERMINE-FILE-DEPENDENCIES ( &optional (database *callers-database*))  [FUNCTION]
>      
>      Makes a hash table of file dependencies for the references listed in
>      DATABASE. This function may be useful for automatically resolving
>      file references for automatic creation of a system definition
>      (defsystem). 

---

#### PRINT-FILE-DEPENDENCIES ( &optional (database *callers-database*))  [FUNCTION]
>      
>      Prints a list of file dependencies for the references listed in
>      DATABASE. This function may be useful for automatically computing
>      file loading constraints for a system definition tool.

---

#### \*LAST-CALLER-TREE\* (nil)                                      [VARIABLE]

---

#### \*DEFAULT-GRAPHING-MODE\* (:call-graph)                         [VARIABLE]
>      
>      Specifies whether we graph up or down. If :call-graph, the children
>      of a node are the functions it calls. If :caller-graph, the
>      children of a node are the functions that call it.

---

#### GATHER-TREE (parents &optional already-seen (mode *default-graphing-mode*) (types-to-ignore *types-to-ignore*) compact)  [FUNCTION]
>      
>      Extends the tree, copying it into list structure, until it repeats
>      a reference (hits a cycle).

---

#### FIND-ROOTS-AND-CYCLES (&optional (mode *default-graphing-mode*) (types-to-ignore *types-to-ignore*))  [FUNCTION]
>      
>      Returns a list of uncalled callers (roots) and called callers
>      (potential cycles).

---

#### MAKE-CALLER-TREE (&optional (mode *default-graphing-mode*) (types-to-ignore *types-to-ignore*) compact)  [FUNCTION]
>      
>      Outputs list structure of a tree which roughly represents the
>      possibly cyclical structure of the caller database.
>      If mode is :call-graph, the children of a node are the functions
>      it calls. If mode is :caller-graph, the children of a node are the
>      functions that call it.
>      If compact is T, tries to eliminate the already-seen nodes, so
>      that the graph for a node is printed at most once. Otherwise it will
>      duplicate the node's tree (except for cycles). This is usefull
>      because the call tree is actually a directed graph, so we can either
>      duplicate references or display only the first one.

---

#### \*INDENT-AMOUNT\* (3)                                           [VARIABLE]
>      
>      Number of spaces to indent successive levels in PRINT-INDENTED-TREE.

---

#### PRINT-INDENTED-TREE (trees &optional (indent 0))                [FUNCTION]
>      
>      Simple code to print out a list-structure tree (such as those created
>      by make-caller-tree) as indented text.

---

#### PRINT-CALLER-TREES (&key (mode *default-graphing-mode*) (types-to-ignore *types-to-ignore*) compact root-nodes)  [FUNCTION]
>      
>      Prints the calling trees (which may actually be a full graph and not
>      necessarily a DAG) as indented text trees using
>      PRINT-INDENTED-TREE. MODE is :call-graph for trees where the children
>      of a node are the functions called by the node, or :caller-graph for
>      trees where the children of a node are the functions the node calls.
>      TYPES-TO-IGNORE is a list of funcall types (as specified in the
>      patterns) to ignore in printing out the database. For example,
>      '(:lisp) would ignore all calls to common lisp functions. COMPACT is
>      a flag to tell the program to try to compact the trees a bit by not
>      printing trees if they have already been seen. ROOT-NODES is a list
>      of root nodes of trees to display. If ROOT-NODES is nil, tries to
>      find all root nodes in the database.

---

## Rudimentary Documentation for XREF-TEST.LISP

*Containing Directory /host/santanu/programming/Lisp/code-tools/xref/*

### API Documentation


#### TOP-LEVEL ()                                                    [FUNCTION]
>      
>      Top level function with null lambda list.

---

#### FROB (items)                                                    [FUNCTION]
>      
>      Here we test mapcar.

---

#### FROB-ITEM (item)                                                [FUNCTION]
>      
>      Here we test apply.

---

#### BARF (key &optional items)                                      [FUNCTION]
>      
>      Optional args test.

---

#### FROWZ (items &key key)                                          [FUNCTION]
>      
>      Keyword args test.

---

#### PROCESS-KEY (key)                                               [FUNCTION]

---

## Rudimentary Documentation for XREF-PATTERNS-FOR-MACL.LISP

*Containing Directory /host/santanu/programming/Lisp/code-tools/xref/*

### API Documentation


## Rudimentary Documentation for USER-MANUAL.LISP

*Containing Directory /host/santanu/programming/Lisp/code-tools/user_man/*

### API Documentation


#### \*USERMAN-VERSION\* ("2.0 20-oct-94")                          [PARAMETER]
>      
>      Current verison number/date for User-Manual.

---

#### USERMAN-COPYRIGHT (&optional (stream *standard-output*))        [FUNCTION]
>      
>      Prints a User Manual copyright notice and header upon startup.

---

#### EXTRACT-DOCUMENTATION (body)                                       [MACRO]

---

#### ATOM-OR-CAR (list-or-atom)                                      [FUNCTION]

---

#### \*DOCUMENTATION-HANDLERS\* ((make-hash-table :test #'equal))    [VARIABLE]
>      
>      Hash table of entries of the form (handler description),
>      where definer is the car of the definition form handled (for
>      example, DEFUN or DEFMACRO), handler is a function which takes the
>      form as input and value-returns the name, argument-list and
>      documentation string, and description is a one-word equivalent of
>      definer (for example, FUNCTION or MACRO).

---

#### DEFINE-DOC-HANDLER (definer arglist description &body body)        [MACRO]
>      
>      Defines a new documentation handler. DEFINER is the car of the
>      definition form handled (e.g., defun), DESCRIPTION is a one-word
>      string equivalent of definer (e.g., "function"), and ARGLIST
>      and BODY together define a function that takes the form as input
>      and value-returns the name, argument-list, documentation string,
>      and a list of any qualifiers of the form.

---

#### FIND-DOC-HANDLER (definer)                                      [FUNCTION]
>      
>      Given the car of a form, finds the appropriate documentation
>      handler for the form if one exists.

---

#### LISTIFY (x)                                                     [FUNCTION]

---

#### NULL-OR-CDR (l)                                                 [FUNCTION]

---

#### NULL-OR-CADR (l)                                                [FUNCTION]

---

#### \*FAILED-DEFINITION-TYPES\* (nil)                               [VARIABLE]
>      
>      List of definition types that create-user-manual couldn't handle.

---

#### CREATE-USER-MANUAL (filename &key (output-format 'text) (output-stream *standard-output*) (purge-latex t))  [FUNCTION]
>      
>      Automatically creates a user manual for the functions in a file by
>      collecting the documentation strings and argument lists of the
>      functions and formatting the output nicely. Returns a list of the
>      definition types of the forms it couldn't handle. Output-format
>      may be either 'TEXT, 'SCRIBE or 'LATEX. In this last case the extra
>      keyword 'purge-latex' may be specified: if non nil the latex
>      filter will try to substitute possible dangerous characters like '&',
>      '\' and '#'.

---

#### HANDLE-FORM-OUTPUT (form &optional (output-format 'text) (stream *standard-output*) (purge-latex t))  [FUNCTION]
>      
>      This function takes a form as input and outputs its documentation
>      segment to the output stream.

---

#### FIND-KEYWORD (sym)                                              [FUNCTION]

---

#### OUTPUT-FRAME-DOCUMENTATION (name type args documentation &optional (stream *standard-output*))  [FUNCTION]
>      
>      Prints out the user guide entry for a form in FrameMaker(tm) mode.

---

#### OUTPUT-TEXT-DOCUMENTATION (name type args documentation args-tab-pos type-pos &optional (stream *standard-output*))  [FUNCTION]
>      
>      Prints out the user guide entry for a form in TEXT mode.

---

#### OUTPUT-MARKDOWN-DOCUMENTATION (name type args documentation args-tab-pos type-pos &optional (stream *standard-output*))  [FUNCTION]
>      
>      Prints out the user guide entry for a form in MARKDOWN mode.

---

#### ESCAPE-NAME-FOR-MARKDOWN (name)                                 [FUNCTION]
>      
>      Escapes the Markdown format characters if present in NAME

---

#### OUTPUT-SCRIBE-DOCUMENTATION (name type args documentation &optional (stream *standard-output*))  [FUNCTION]
>      
>      Prints out the user guide entry for a form in SCRIBE mode.

---

#### OUTPUT-LATEX-DOCUMENTATION (name type args documentation &optional (stream *standard-output*) (purge-documentation t))  [FUNCTION]
>      
>      Prints out the user guide entry for a form in LaTeX mode.

---

#### PURGE-STRING-FOR-LATEX (a-string purge-doc)                     [FUNCTION]
>      
>      Tries to purge a string from characters that are potentially
>      dangerous for LaTeX.

---

#### PREPROCESS-LAMBDA-KEYWORDS (args)                               [FUNCTION]
>      
>      Unused

---

#### PREPROCESS-LISP-LATEX-CLASHES (args purge-doc)                  [FUNCTION]
>      
>      This function is used to make the strings for the arguments of the
>      form digestible for LaTeX, e.g. by removing '#' and '&'.

---

#### PREPROCESS-CHARACTER (c)                                        [FUNCTION]
>      
>      Low level processing of single characters, when passed as defaults
>      to optional, key and aux parameters.

---

#### PREPROCESS-SPECIALS (list-form purge-doc)                       [FUNCTION]
>      
>      Processing of some 'special' forms. Only 'quote' and 'function' are
>      treated for the time being.

---

#### SPLIT-STRING (string width &optional arglistp filled (trim-whitespace t))  [FUNCTION]
>      
>      Splits a string into a list of strings, each of which is shorter
>      than the specified width. Tries to be intelligent about where to
>      split the string if it is an argument list. If filled is T,
>      tries to fill out the strings as much as possible. This function
>      is used to break up long argument lists nicely, and to break up
>      wide lines of documentation nicely.

---

#### SPLIT-POINT (string max-length &optional arglistp filled)       [FUNCTION]
>      
>      Finds an appropriate point to break the string at given a target
>      length. If arglistp is T, tries to find an intelligent position to
>      break the string. If filled is T, tries to fill out the string as
>      much as possible. 

---

#### LAMBDA-LIST-KEYWORD-POSITION (string &optional end trailer-only)  [FUNCTION]
>      
>      If the previous symbol is a lambda-list keyword, returns
>      its position. Otherwise returns end.

---

#### BALANCED-PARENTHESIS-POSITION (string &optional end)            [FUNCTION]
>      
>      Finds the position of the left parenthesis which is closest to END
>      but leaves the prefix of the string with balanced parentheses or
>      at most 1 unbalanced left parenthesis.

---

#### UM-BUILD-SYMBOL (symbol &key (prefix nil prefix-p) (suffix nil suffix-p) (package nil package-p))  [FUNCTION]
>      
>      Build a symbol concatenating prefix (if not null), symbol, and suffix
>      (if not null). The newly generated symbol is interned in package, if
>      not null, or in the SYMBOL-PACKAGE of symbol, otherwise. 

---

#### CREATE-MANUALS (files &key (extension '.cl) (output-format 'text))  [FUNCTION]

---

#### PARSE-WITH-DELIMITER (line &optional (delim #\newline))         [FUNCTION]
>      
>      Breaks LINE into a list of strings, using DELIM as a
>      breaking point.

---

## Rudimentary Documentation for DOCUMENTATION-BUILDER.LISP

*Containing Directory /host/santanu/programming/Lisp/code-tools/code-docs/*

### API Documentation


#### SPIT-CALL-TREE-TO-STREAM (source-file)                          [FUNCTION]
>      
>      spit-call-tree-to-stream: outputs XREF caller tree to a stream.
>          Checks if XREF database for file is already created otherwise
>      creates it. 
>          Arguments: source-file - full file path as string

---

#### CREATE-XREFDB-FOR-SOURCE (source-dir &optional (file-type "lisp"))  [FUNCTION]
>      
>      create-xrefdb-for-source: creates XREF database for all source files
>      in SOURCE-DIR. Marks XREF-DBCREATED for the source file to TRUE.
>          Arguments: source-dir - source directory path in string
>                     file-type - source file extension (default is lisp)

---

#### ESCAPE-MARKDOWN-BEFORE-OUTPUT (&rest forms)                        [MACRO]
>      
>      Escapes all the output strings for markdown format characters before
>      passing them to standard output. FORMS represent valid LISP forms
>      that produces output to standard output stream.

---

#### SAVE-DOCS-FOR-SOURCE (source-dir &optional (file-type "lisp") doc-dir)  [FUNCTION]
>      
>      save-docs-for-source: saves source documentation in user_manual.md
>      file. Also assumes that XREF database has been created for files in
>      SOURCE-DIR. 
>          Arguments: source-dir - soure directory path in string
>                     file-type - source file extension (default is lisp)
>                     doc-dir - directory where user_manual.md is saved
>      (default is source-dir 

---

#### BUILD-DOCUMENTATION (for file-type files in source-dir save at doc-dir)  [MACRO]

---

#### ESCAPE-LINE-MARKDOWN (line)                                     [FUNCTION]
>      
>      Escapes the Markdown format characters if present in LINE

---


## Dependency Documentations

### File Dependencies

"/host/santanu/programming/Lisp/code-tools/xref/xref-test.lisp" --> ("/host/santanu/programming/Lisp/code-tools/xref/xref-test.lisp")
"/host/santanu/programming/Lisp/code-tools/user\_man/user-manual.lisp" --> 
("/host/santanu/programming/Lisp/code-tools/user\_man/user-manual.lisp"
 "/host/santanu/programming/Lisp/code-tools/code-docs/documentation-builder.lisp")
"/host/santanu/programming/Lisp/code-tools/code-docs/documentation-builder.lisp" --> ("/host/santanu/programming/Lisp/code-tools/code-docs/documentation-builder.lisp")
"/host/santanu/programming/Lisp/code-tools/xref/xref.lisp" --> 
("/host/santanu/programming/Lisp/code-tools/xref/xref.lisp"
 "/host/santanu/programming/Lisp/code-tools/code-docs/documentation-builder.lisp")

### Call Dependencies


#### Function/Macro Calls


XREF:DISPLAY-DATABASE is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::ESCAPE-LINE-MARKDOWN is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

XREF:PRINT-FILE-DEPENDENCIES is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::ESCAPE-MARKDOWN-BEFORE-OUTPUT is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::SPIT-DOCUMENTATION-FOR-FILE is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

XREF:PRINT-CALLER-TREES is referenced by CODE-DOCS::SPIT-CALL-TREE-TO-STREAM.

USER-MAN:CREATE-USER-MANUAL is referenced by CODE-DOCS::SPIT-DOCUMENTATION-FOR-FILE USER-MAN:CREATE-MANUALS.

USER-MAN::UM-BUILD-SYMBOL is referenced by USER-MAN:CREATE-MANUALS.

USER-MAN::BALANCED-PARENTHESIS-POSITION is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION USER-MAN::SPLIT-POINT.

USER-MAN::LAMBDA-LIST-KEYWORD-POSITION is referenced by USER-MAN::SPLIT-POINT.

USER-MAN::SPLIT-POINT is referenced by USER-MAN::SPLIT-STRING.

USER-MAN::PARSE-WITH-DELIMITER is referenced by USER-MAN::PARSE-WITH-DELIMITER USER-MAN::SPLIT-STRING.

USER-MAN::PREPROCESS-CHARACTER is referenced by USER-MAN::PREPROCESS-LISP-LATEX-CLASHES.

USER-MAN::PREPROCESS-SPECIALS is referenced by USER-MAN::PREPROCESS-LISP-LATEX-CLASHES.

USER-MAN::PURGE-STRING-FOR-LATEX is referenced by USER-MAN::PREPROCESS-SPECIALS USER-MAN::PREPROCESS-LISP-LATEX-CLASHES USER-MAN::OUTPUT-LATEX-DOCUMENTATION.

USER-MAN::PREPROCESS-LISP-LATEX-CLASHES is referenced by USER-MAN::PREPROCESS-SPECIALS USER-MAN::OUTPUT-LATEX-DOCUMENTATION.

USER-MAN::ESCAPE-NAME-FOR-MARKDOWN is referenced by USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION.

USER-MAN::OUTPUT-LATEX-DOCUMENTATION is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::OUTPUT-SCRIBE-DOCUMENTATION is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::OUTPUT-TEXT-DOCUMENTATION is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::SPLIT-STRING is referenced by USER-MAN::OUTPUT-LATEX-DOCUMENTATION USER-MAN::OUTPUT-SCRIBE-DOCUMENTATION USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION USER-MAN::OUTPUT-TEXT-DOCUMENTATION USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::HANDLER is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::FIND-DOC-HANDLER is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN:HANDLE-FORM-OUTPUT is referenced by USER-MAN:HANDLE-FORM-OUTPUT USER-MAN:CREATE-USER-MANUAL.

SYMBOL-NAME-KEY is referenced by PROCESS-KEY.

NODE-POSITION is referenced by FROWZ.

PROCESS-KEY is referenced by FROWZ.

SNARF-ITEM is referenced by FROWZ.

PROCESS-KEYS is referenced by FROWZ.

APPEND-FROBS is referenced by FROB-ITEM.

FROB-ITEM is referenced by FROB.

FROWZ is referenced by BARF TOP-LEVEL.

BARF is referenced by TOP-LEVEL.

FROB is referenced by TOP-LEVEL.

XREF:MAKE-CALLER-TREE is referenced by XREF:PRINT-CALLER-TREES.

XREF:PRINT-INDENTED-TREE is referenced by XREF:PRINT-CALLER-TREES XREF:PRINT-INDENTED-TREE.

XREF::GATHER-TREE is referenced by XREF:PRINT-CALLER-TREES XREF:MAKE-CALLER-TREE.

XREF::FIND-ROOTS-AND-CYCLES is referenced by XREF:MAKE-CALLER-TREE.

XREF::AMASS-TREE is referenced by XREF::AMASS-TREE.

XREF:DETERMINE-FILE-DEPENDENCIES is referenced by XREF:PRINT-FILE-DEPENDENCIES.

XREF::PATTERN-CALLER-TYPE is referenced by XREF::FIND-ROOTS-AND-CYCLES XREF::AMASS-TREE XREF::INVERT-HASH-TABLE XREF:DISPLAY-DATABASE.

:UNNAMED-LAMBDA is referenced by CODE-DOCS::ESCAPE-LINE-MARKDOWN USER-MAN::PREPROCESS-LISP-LATEX-CLASHES USER-MAN::PREPROCESS-LAMBDA-KEYWORDS USER-MAN::ESCAPE-NAME-FOR-MARKDOWN USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION XREF::FIND-ROOTS-AND-CYCLES XREF:PRINT-FILE-DEPENDENCIES XREF:DETERMINE-FILE-DEPENDENCIES XREF::INVERT-HASH-TABLE XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF:DISPLAY-DATABASE.

XREF::CAR-EQ is referenced by XREF::RECORD-CALLERS\*.

XREF::RECORD-CALLERS\* is referenced by XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS.

XREF::LOOKUP-PATTERN-SUBSTITUTION is referenced by XREF::RECORD-CALLERS.

XREF::LOOKUP-CALLER-PATTERN is referenced by XREF::RECORD-CALLERS.

XREF::LOOKUP is referenced by XREF::RECORD-CALLERS.

XREF::RECORD-CALLERS is referenced by XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS.

XREF:SOURCE-FILE is referenced by XREF:DETERMINE-FILE-DEPENDENCIES XREF::RECORD-CALLERS.

XREF:CLEAR-TABLES is referenced by CODE-DOCS:CREATE-XREFDB-FOR-SOURCE XREF:XREF-FILE.

XREF:XREF-FILE is referenced by CODE-DOCS:CREATE-XREFDB-FOR-SOURCE CODE-DOCS::SPIT-CALL-TREE-TO-STREAM XREF:XREF-FILES.

XREF:LIST-SETTERS is referenced by XREF:WHO-CALLS XREF:LIST-USERS.

XREF:LIST-READERS is referenced by XREF:WHO-CALLS XREF:LIST-USERS.

XREF:LIST-CALLERS is referenced by XREF:WHO-CALLS XREF:LIST-USERS.

XREF::CALLERS-LIST is referenced by XREF::RECORD-CALLERS XREF:LIST-CALLEES XREF:WHAT-FILES-CALL XREF:LIST-SETTERS XREF:LIST-READERS XREF:LIST-CALLERS.

#### Variable Readers


CODE-DOCS::LINE-CHARS is referenced by CODE-DOCS::ESCAPE-LINE-MARKDOWN.

CODE-DOCS::ESCAPED-LINE is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::DOC-FILE is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::DIR-PATH is referenced by CODE-DOCS::SPIT-DOCUMENTATION-FOR-FILE.

CODE-DOCS::FILE-NAME is referenced by CODE-DOCS::SPIT-DOCUMENTATION-FOR-FILE.

CODE-DOCS::SOURCE-FILES is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE CODE-DOCS:CREATE-XREFDB-FOR-SOURCE.

CODE-DOCS::STRING-CALL-TREE is referenced by CODE-DOCS::SPIT-CALL-TREE-TO-STREAM.

USER-MAN::IN is referenced by USER-MAN:CREATE-MANUALS.

USER-MAN::FILE is referenced by USER-MAN:CREATE-MANUALS.

USER-MAN::FOR is referenced by USER-MAN:CREATE-MANUALS.

USER-MAN::NAME-CHARS is referenced by USER-MAN::ESCAPE-NAME-FOR-MARKDOWN.

USER-MAN::P\_ARGS is referenced by USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION.

USER-MAN::\*FAILED-DEFINITION-TYPES\* is referenced by USER-MAN:HANDLE-FORM-OUTPUT USER-MAN:CREATE-USER-MANUAL.

USER-MAN::\*DOCUMENTATION-HANDLERS\* is referenced by USER-MAN::FIND-DOC-HANDLER.

USER-MAN::\*USERMAN-VERSION\* is referenced by USER-MAN::USERMAN-COPYRIGHT.

XREF:\*INDENT-AMOUNT\* is referenced by XREF:PRINT-INDENTED-TREE.

XREF:\*DEFAULT-GRAPHING-MODE\* is referenced by XREF:PRINT-CALLER-TREES XREF:MAKE-CALLER-TREE XREF::FIND-ROOTS-AND-CYCLES XREF::GATHER-TREE.

XREF:\*HANDLE-PACKAGE-FORMS\* is referenced by XREF:DISPLAY-DATABASE.

XREF:\*TYPES-TO-IGNORE\* is referenced by XREF:PRINT-CALLER-TREES XREF:MAKE-CALLER-TREE XREF::FIND-ROOTS-AND-CYCLES XREF::GATHER-TREE XREF::INVERT-HASH-TABLE XREF:DISPLAY-DATABASE.

XREF:\*HANDLE-MACRO-FORMS\* is referenced by XREF::RECORD-CALLERS.

XREF::\*CALLEES-DATABASE-INCLUDES-VARIABLES\* is referenced by XREF::RECORD-CALLERS.

XREF:\*HANDLE-FUNCTION-FORMS\* is referenced by XREF::RECORD-CALLERS.

XREF::\*NORMAL-READTABLE\* is referenced by XREF:XREF-FILE.

XREF:\*XREF-VERBOSE\* is referenced by XREF:XREF-FILE.

XREF::\*CALLER-PATTERN-TABLE\* is referenced by XREF:CLEAR-PATTERNS XREF::LOOKUP-CALLER-PATTERN.

XREF::\*PATTERN-SUBSTITUTION-TABLE\* is referenced by XREF:CLEAR-PATTERNS XREF::LOOKUP-PATTERN-SUBSTITUTION.

XREF::\*PATTERN-CALLER-TYPE\* is referenced by XREF:CLEAR-PATTERNS XREF::PATTERN-CALLER-TYPE.

XREF::\*SOURCE-FILE\* is referenced by XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF:CLEAR-TABLES XREF:SOURCE-FILE.

XREF::\*SETTERS-DATABASE\* is referenced by XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF:DISPLAY-DATABASE XREF:CLEAR-TABLES XREF::CALLERS-LIST.

XREF::\*READERS-DATABASE\* is referenced by XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF:DISPLAY-DATABASE XREF:CLEAR-TABLES XREF::CALLERS-LIST.

XREF::\*CALLERS-DATABASE\* is referenced by XREF::FIND-ROOTS-AND-CYCLES XREF::GATHER-TREE XREF:PRINT-FILE-DEPENDENCIES XREF:DETERMINE-FILE-DEPENDENCIES XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF:DISPLAY-DATABASE XREF:CLEAR-TABLES XREF::CALLERS-LIST.

XREF::\*CALLEES-DATABASE\* is referenced by XREF::FIND-ROOTS-AND-CYCLES XREF::GATHER-TREE XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF:CLEAR-TABLES XREF::CALLERS-LIST.

XREF::\*FILE-CALLERS-DATABASE\* is referenced by XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF:DISPLAY-DATABASE XREF:CLEAR-TABLES XREF::CALLERS-LIST.

#### Variable Setters


CODE-DOCS::X is referenced by CODE-DOCS::ESCAPE-LINE-MARKDOWN.

CODE-DOCS::LINE-CHARS is referenced by CODE-DOCS::ESCAPE-LINE-MARKDOWN.

CODE-DOCS::AT is referenced by CODE-DOCS:BUILD-DOCUMENTATION.

CODE-DOCS::SAVE is referenced by CODE-DOCS:BUILD-DOCUMENTATION.

CODE-DOCS::IN is referenced by CODE-DOCS:BUILD-DOCUMENTATION.

CODE-DOCS::FILES is referenced by CODE-DOCS:BUILD-DOCUMENTATION.

CODE-DOCS::FOR is referenced by CODE-DOCS:BUILD-DOCUMENTATION.

CODE-DOCS::ESCAPED-LINE is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::LINE is referenced by CODE-DOCS::ESCAPE-LINE-MARKDOWN CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::FORMS-OUTPUT is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::S is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::DOC-FILE is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::DOC-DIR is referenced by CODE-DOCS:BUILD-DOCUMENTATION CODE-DOCS:SAVE-DOCS-FOR-SOURCE.

CODE-DOCS::DIR-PATH is referenced by CODE-DOCS::SPIT-DOCUMENTATION-FOR-FILE.

CODE-DOCS::FILE-NAME is referenced by CODE-DOCS::SPIT-DOCUMENTATION-FOR-FILE.

CODE-DOCS::FORMS is referenced by CODE-DOCS::ESCAPE-MARKDOWN-BEFORE-OUTPUT.

CODE-DOCS::SOURCE-FILES is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE CODE-DOCS:CREATE-XREFDB-FOR-SOURCE.

CODE-DOCS::FILE-TYPE is referenced by CODE-DOCS:BUILD-DOCUMENTATION CODE-DOCS:SAVE-DOCS-FOR-SOURCE CODE-DOCS:CREATE-XREFDB-FOR-SOURCE.

CODE-DOCS::SOURCE-DIR is referenced by CODE-DOCS:BUILD-DOCUMENTATION CODE-DOCS:SAVE-DOCS-FOR-SOURCE CODE-DOCS:CREATE-XREFDB-FOR-SOURCE.

CODE-DOCS::SOURCE-FILE is referenced by CODE-DOCS:SAVE-DOCS-FOR-SOURCE CODE-DOCS::SPIT-DOCUMENTATION-FOR-FILE CODE-DOCS:CREATE-XREFDB-FOR-SOURCE CODE-DOCS::SPIT-CALL-TREE-TO-STREAM.

USER-MAN::DELIM is referenced by USER-MAN::PARSE-WITH-DELIMITER.

USER-MAN::LINE is referenced by USER-MAN::PARSE-WITH-DELIMITER.

USER-MAN::EXTENSION is referenced by USER-MAN:CREATE-MANUALS.

USER-MAN::FILES is referenced by USER-MAN:CREATE-MANUALS.

USER-MAN::NEWNAME is referenced by USER-MAN::UM-BUILD-SYMBOL.

USER-MAN::PACKAGE-P is referenced by USER-MAN::UM-BUILD-SYMBOL.

USER-MAN::SUFFIX-P is referenced by USER-MAN::UM-BUILD-SYMBOL.

USER-MAN::SUFFIX is referenced by USER-MAN::UM-BUILD-SYMBOL.

USER-MAN::PREFIX-P is referenced by USER-MAN::UM-BUILD-SYMBOL.

USER-MAN::PREFIX is referenced by USER-MAN::UM-BUILD-SYMBOL.

USER-MAN::RIGHTMOST-LEFT-PAREN is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION.

USER-MAN::LEFTMOST-RIGHT-PAREN is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION.

USER-MAN::LEFTMOST-LEFT-PAREN is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION.

USER-MAN::IMBALANCE is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION.

USER-MAN::NUM-RIGHT is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION.

USER-MAN::NUM-LEFT is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION.

USER-MAN::RIGHTMOST-SPACE is referenced by USER-MAN::LAMBDA-LIST-KEYWORD-POSITION.

USER-MAN::AMPERSAND is referenced by USER-MAN::LAMBDA-LIST-KEYWORD-POSITION.

USER-MAN::TRAILER-ONLY is referenced by USER-MAN::LAMBDA-LIST-KEYWORD-POSITION.

USER-MAN::END is referenced by USER-MAN::BALANCED-PARENTHESIS-POSITION USER-MAN::LAMBDA-LIST-KEYWORD-POSITION.

USER-MAN::PAREN is referenced by USER-MAN::SPLIT-POINT.

USER-MAN::POS is referenced by USER-MAN::PARSE-WITH-DELIMITER USER-MAN::SPLIT-POINT.

USER-MAN::SPACE-POS is referenced by USER-MAN::SPLIT-POINT.

USER-MAN::MAX-LENGTH is referenced by USER-MAN::SPLIT-POINT.

USER-MAN::S is referenced by USER-MAN::SPLIT-STRING.

USER-MAN::STRING-LIST is referenced by USER-MAN::SPLIT-STRING.

USER-MAN::TRIM-WHITESPACE is referenced by USER-MAN::SPLIT-STRING.

USER-MAN::FILLED is referenced by USER-MAN::SPLIT-POINT USER-MAN::SPLIT-STRING.

USER-MAN::ARGLISTP is referenced by USER-MAN::SPLIT-POINT USER-MAN::SPLIT-STRING.

USER-MAN::LIST-FORM is referenced by USER-MAN::PREPROCESS-SPECIALS.

USER-MAN::C is referenced by USER-MAN::PREPROCESS-CHARACTER USER-MAN::PURGE-STRING-FOR-LATEX.

USER-MAN::EOS is referenced by USER-MAN::PURGE-STRING-FOR-LATEX.

USER-MAN::RESULT is referenced by USER-MAN::SPLIT-STRING USER-MAN::PURGE-STRING-FOR-LATEX.

USER-MAN::A-STR is referenced by USER-MAN::PURGE-STRING-FOR-LATEX.

USER-MAN::PURGE-DOC is referenced by USER-MAN::PREPROCESS-SPECIALS USER-MAN::PREPROCESS-LISP-LATEX-CLASHES USER-MAN::PURGE-STRING-FOR-LATEX.

USER-MAN::A-STRING is referenced by USER-MAN::PURGE-STRING-FOR-LATEX.

USER-MAN::PURGE-DOCUMENTATION is referenced by USER-MAN::OUTPUT-LATEX-DOCUMENTATION.

USER-MAN::NAME-CHARS is referenced by USER-MAN::ESCAPE-NAME-FOR-MARKDOWN.

USER-MAN::P\_ARGS is referenced by USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION.

USER-MAN::ARGS-TAB-POS is referenced by USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION USER-MAN::OUTPUT-TEXT-DOCUMENTATION.

USER-MAN::ARG is referenced by USER-MAN::PREPROCESS-LISP-LATEX-CLASHES USER-MAN::PREPROCESS-LAMBDA-KEYWORDS USER-MAN::OUTPUT-SCRIBE-DOCUMENTATION USER-MAN::OUTPUT-TEXT-DOCUMENTATION USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::FIRST-&OPTIONAL-P is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::FIRST-&KEY-P is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::FIRST-&AUX-P is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::&OPTIONAL-P is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::&AUX-P is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::&KEY-P is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::&REST-P is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION.

USER-MAN::SYM is referenced by USER-MAN::FIND-KEYWORD.

USER-MAN::F is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::WIDTH is referenced by USER-MAN::SPLIT-STRING USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::ARGS-LIST-FORM is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::TYPE-POS is referenced by USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION USER-MAN::OUTPUT-TEXT-DOCUMENTATION USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::NAME-LENGTH is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::QUALIFIERS is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::ARGS is referenced by USER-MAN::PREPROCESS-LISP-LATEX-CLASHES USER-MAN::PREPROCESS-LAMBDA-KEYWORDS USER-MAN::OUTPUT-LATEX-DOCUMENTATION USER-MAN::OUTPUT-SCRIBE-DOCUMENTATION USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION USER-MAN::OUTPUT-TEXT-DOCUMENTATION USER-MAN::OUTPUT-FRAME-DOCUMENTATION USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::NAME is referenced by USER-MAN::OUTPUT-LATEX-DOCUMENTATION USER-MAN::OUTPUT-SCRIBE-DOCUMENTATION USER-MAN::ESCAPE-NAME-FOR-MARKDOWN USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION USER-MAN::OUTPUT-TEXT-DOCUMENTATION USER-MAN::OUTPUT-FRAME-DOCUMENTATION USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::HANDLER is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::HANDLER-ENTRY is referenced by USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::KEY is referenced by USER-MAN::OUTPUT-FRAME-DOCUMENTATION USER-MAN:HANDLE-FORM-OUTPUT.

USER-MAN::FORM is referenced by USER-MAN:HANDLE-FORM-OUTPUT USER-MAN:CREATE-USER-MANUAL.

USER-MAN::EOF is referenced by USER-MAN:CREATE-USER-MANUAL.

USER-MAN::\*FAILED-DEFINITION-TYPES\* is referenced by USER-MAN:CREATE-USER-MANUAL.

USER-MAN::PURGE-LATEX is referenced by USER-MAN:HANDLE-FORM-OUTPUT USER-MAN:CREATE-USER-MANUAL.

USER-MAN::OUTPUT-STREAM is referenced by USER-MAN:CREATE-USER-MANUAL.

USER-MAN::OUTPUT-FORMAT is referenced by USER-MAN:CREATE-MANUALS USER-MAN:HANDLE-FORM-OUTPUT USER-MAN:CREATE-USER-MANUAL.

USER-MAN::FILENAME is referenced by USER-MAN:CREATE-USER-MANUAL.

USER-MAN::L is referenced by USER-MAN::NULL-OR-CADR USER-MAN::NULL-OR-CDR.

USER-MAN::X is referenced by USER-MAN::ESCAPE-NAME-FOR-MARKDOWN USER-MAN::OUTPUT-MARKDOWN-DOCUMENTATION USER-MAN::LISTIFY.

USER-MAN::DESCRIPTION is referenced by USER-MAN:DEFINE-DOC-HANDLER.

USER-MAN::ARGLIST is referenced by USER-MAN:DEFINE-DOC-HANDLER.

USER-MAN::DEFINER is referenced by USER-MAN::FIND-DOC-HANDLER USER-MAN:DEFINE-DOC-HANDLER.

USER-MAN::LIST-OR-ATOM is referenced by USER-MAN::ATOM-OR-CAR.

USER-MAN::BODY is referenced by USER-MAN::EXTRACT-DOCUMENTATION.

FROWZ is referenced by FROWZ.

ITEM is referenced by FROWZ FROB-ITEM.

ITEMS is referenced by FROWZ BARF FROB.

KEY is referenced by PROCESS-KEY FROWZ BARF TOP-LEVEL.

INPUT is referenced by TOP-LEVEL.

XREF::CYCLES is referenced by XREF:PRINT-CALLER-TREES.

XREF::ROOTED is referenced by XREF:PRINT-CALLER-TREES.

XREF::ROOT-NODES is referenced by XREF:PRINT-CALLER-TREES.

XREF::TREE is referenced by XREF:PRINT-INDENTED-TREE.

XREF::INDENT is referenced by XREF:PRINT-INDENTED-TREE.

XREF::MORE-TREES is referenced by XREF:MAKE-CALLER-TREE.

XREF:\*LAST-CALLER-TREE\* is referenced by XREF:MAKE-CALLER-TREE.

XREF::TREES is referenced by XREF:PRINT-INDENTED-TREE XREF:MAKE-CALLER-TREE.

XREF::OTHER-DATABASE is referenced by XREF::FIND-ROOTS-AND-CYCLES.

XREF::CALLED-CALLERS is referenced by XREF:MAKE-CALLER-TREE XREF::FIND-ROOTS-AND-CYCLES.

XREF::UNCALLED-CALLERS is referenced by XREF:MAKE-CALLER-TREE XREF::FIND-ROOTS-AND-CYCLES.

XREF::THIS-ITEM is referenced by XREF::AMASS-TREE.

XREF::RESULT is referenced by XREF::AMASS-TREE.

XREF::\*ALREADY-SEEN\* is referenced by XREF::GATHER-TREE.

XREF::COMPACT is referenced by XREF:PRINT-CALLER-TREES XREF:MAKE-CALLER-TREE XREF::GATHER-TREE.

XREF::MODE is referenced by XREF:PRINT-CALLER-TREES XREF:MAKE-CALLER-TREE XREF::FIND-ROOTS-AND-CYCLES XREF::GATHER-TREE.

XREF::ALREADY-SEEN is referenced by XREF:MAKE-CALLER-TREE XREF::AMASS-TREE XREF::GATHER-TREE.

XREF::PARENTS is referenced by XREF::AMASS-TREE XREF::GATHER-TREE.

XREF::S is referenced by XREF:DETERMINE-FILE-DEPENDENCIES.

XREF::VALUE-FILE is referenced by XREF:DETERMINE-FILE-DEPENDENCIES.

XREF::KEY-FILE is referenced by XREF:DETERMINE-FILE-DEPENDENCIES.

XREF::FILE-REF-HT is referenced by XREF:DETERMINE-FILE-DEPENDENCIES.

XREF::VALUE is referenced by XREF::FIND-ROOTS-AND-CYCLES XREF:PRINT-FILE-DEPENDENCIES XREF:DETERMINE-FILE-DEPENDENCIES XREF::INVERT-HASH-TABLE.

XREF::KEY is referenced by XREF:PRINT-FILE-DEPENDENCIES XREF:DETERMINE-FILE-DEPENDENCIES XREF::INVERT-HASH-TABLE.

XREF::TARGET is referenced by XREF::INVERT-HASH-TABLE.

XREF::TABLE is referenced by XREF::INVERT-HASH-TABLE.

XREF::Y is referenced by XREF:WRITE-CALLERS-DATABASE-TO-FILE.

XREF::X is referenced by XREF:WRITE-CALLERS-DATABASE-TO-FILE.

XREF::CALLERS is referenced by XREF:DISPLAY-DATABASE.

XREF::TYPES-TO-IGNORE is referenced by XREF:PRINT-CALLER-TREES XREF:MAKE-CALLER-TREE XREF::FIND-ROOTS-AND-CYCLES XREF::GATHER-TREE XREF::INVERT-HASH-TABLE XREF:DISPLAY-DATABASE.

XREF::PAR is referenced by XREF::RECORD-CALLERS\*.

XREF::PATTERN-ELT is referenced by XREF::RECORD-CALLERS\*.

XREF::IN-KEYWORDS is referenced by XREF::RECORD-CALLERS\*.

XREF::IN-OPTIONALS is referenced by XREF::RECORD-CALLERS\*.

XREF::CONTINUATION is referenced by XREF::RECORD-CALLERS\*.

XREF::ENV is referenced by XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS.

XREF::P is referenced by XREF::RECORD-CALLERS.

XREF::D is referenced by XREF:DETERMINE-FILE-DEPENDENCIES XREF::RECORD-CALLERS.

XREF::PROCESSED is referenced by XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS.

XREF::SUBPAT is referenced by XREF::RECORD-CALLERS.

XREF::NEW-PATTERN is referenced by XREF::RECORD-CALLERS.

XREF::PARENT is referenced by XREF::AMASS-TREE XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS.

XREF::FORM is referenced by XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS XREF:XREF-FILE.

XREF::OLD-PACKAGE is referenced by XREF:XREF-FILE.

XREF::VERBOSE is referenced by XREF:XREF-FILE.

XREF:CLEAR-TABLES is referenced by XREF:XREF-FILE.

XREF::FILENAME is referenced by XREF:WRITE-CALLERS-DATABASE-TO-FILE XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS XREF:XREF-FILE.

XREF::FILE is referenced by XREF:XREF-FILES.

XREF::FILES is referenced by XREF:XREF-FILES.

XREF::DESTINATIONS is referenced by XREF:DEFINE-CALLER-PATTERN-SYNONYMS.

XREF::SOURCE is referenced by XREF:DEFINE-CALLER-PATTERN-SYNONYMS.

XREF::CALLER-TYPE is referenced by XREF:DEFINE-VARIABLE-PATTERN XREF:DEFINE-CALLER-PATTERN.

XREF::PATTERN is referenced by XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS XREF:DEFINE-CALLER-PATTERN XREF:DEFINE-PATTERN-SUBSTITUTION.

XREF::HOW is referenced by XREF:WHO-CALLS.

XREF::DATABASE is referenced by XREF::FIND-ROOTS-AND-CYCLES XREF::GATHER-TREE XREF:PRINT-FILE-DEPENDENCIES XREF:DETERMINE-FILE-DEPENDENCIES XREF:DISPLAY-DATABASE XREF::CALLERS-LIST.

XREF::NAME is referenced by XREF::FIND-ROOTS-AND-CYCLES XREF:DISPLAY-DATABASE XREF:DEFINE-VARIABLE-PATTERN XREF:DEFINE-CALLER-PATTERN XREF::LOOKUP-CALLER-PATTERN XREF:DEFINE-PATTERN-SUBSTITUTION XREF::LOOKUP-PATTERN-SUBSTITUTION XREF::PATTERN-CALLER-TYPE XREF::CALLERS-LIST.

XREF::ITEM is referenced by XREF::CAR-EQ.

XREF::FRAME is referenced by XREF::LOOKUP.

XREF::ENVIRONMENT is referenced by XREF::RECORD-CALLERS\* XREF::RECORD-CALLERS XREF::LOOKUP.
