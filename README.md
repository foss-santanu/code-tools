CODE-TOOLS: Collection of Tools to process LISP code file
=========================================================

[*AI repository*](http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/) of **Carnegie Mellon University** is a treasure trove both for AI enthusiasts and "common lispers". I found it while searching internet for some topic I can't remember now. May be something related to Common Lisp or AI.

It has such a rich collection of freely available tools. You are free to do whatever you are willing to do with them. So the two tools that first caught my eyes are *XREF* and *USER-MANUAL*. Why? Well the reason is I am too lazy to write a manual documenting my code. By combining these two tools I can create a tool that can extract documentation from my Lisp code base and generate a manual. I am not after a very well formatted one, but something that is readable and can be referred to get an idea of the code base.

So I created **CODE-DOCS**, a wrapper on the two tools XREF and USER-MANUAL. I also made some small modifications in both XREF and USER-MANUAL for my purpose.

### Modifications I Made

Both XREF and USER-MANUAL are copy-righted to the persons or entities as mentioned at the begining of the respective LISP files. I only claim copy-right for the codes in **code-docs folder**. To learn about how the two tools can be used please refer to the [*User Guide*](http://repository.cmu.edu/cgi/viewcontent.cgi?article=3036&context=compsci).

#### Changes in XREF

+ I made it ASDF loadable so that I can use `(ql:quickload 'xref)`.

+ Character literals were getting recognized as variable name - fixed that.

+ Backquote (\`) was getting recognized as function name - fixed that.

+ Some part of the code was throwing errors in CLISP - fixed that.

#### Changes in USER-MANUAL

+ I made it ASDF loadable so that I can use `(ql:quickload 'user-man)`.

+ Implemented support for MARKDOWN formatted output.

### Usage for CODE-DOCS

+ To load CODE-DOCS: `(ql:quickload 'code-docs)`. This will automatically load both XREF and USER-MANUAL.

+ To create a document in MARKDOWN format: `(code-docs:build-documentation for "lisp" files in <code directory path> save at <document directory path>)`. CODE-DOCS create document in MARKDOWN format only.
