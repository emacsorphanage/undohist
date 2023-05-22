[![MELPA](https://melpa.org/packages/undohist-badge.svg)](https://melpa.org/#/undohist)
[![MELPA Stable](https://stable.melpa.org/packages/undohist-badge.svg)](https://stable.melpa.org/#/undohist)


# undohist

Persistent undo history for GNU Emacs

## Usage

This extension provides a way to use undo history of individual file 
buffers persistently.

Write the following code to your .emacs:

```el
(require 'undohist)
(undohist-initialize)
```

Now you can record and recover undohist by typing C-x C-s (save-buffer) an 
C-x C-f (find-file). And then type C-/ (undo).

## Alternatives

Note that this package is no longer actively maintained.

- [undo-fu-session](https://melpa.org/#/undo-fu-session) is an updated
  version of this package, with support for undo/redo and compression
  among other improvements.

- [undo-tree](https://elpa.gnu.org/packages/undo-tree.html) is an undo
  system for Emacs, which includes support for restoring an undo-session
  from saved files.
