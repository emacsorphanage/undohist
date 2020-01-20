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
