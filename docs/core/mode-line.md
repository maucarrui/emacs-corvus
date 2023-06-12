# Corvus mode-line

The mode-line *should* hold the most relevant information about the buffer it
corresponds to. For example: name, major mode, etc.

## Mode-line format

The variable `mode-line-format`, as its name implies, indicates the format that
a general mode-line will have. Corvus mode-line format consists of the following
in the specified order:

1. Mule information
2. Buffer information
   1. Has been modified
   2. Identification
   3. Has been locked
3. Major and minor modes
4. Version Control
5. Line and column
6. Line percentage
7. End spaces

### Mule information

Mode-line construct to report the multilingual environment. That is the current
input method and mnemonics of the following coding systems for:

* Saving and writing buffers.
* Input for text terminals.
* Output for text terminals.

The previous is appended to the mode-line via `'mode-line-mule-info`, and it
corresponds to the first character available in the mode-line.

### Buffer information

The following constructs correspond to the buffer.

#### Has been modified

Displays whether the buffer has been modified. It shows as a `-` if the buffer
hasn't been modified, otherwise it appears as `*`. It is appended to the
mode-line via `mode-line-modified`.

#### Identification

The vanilla `mode-line-buffer-identification` variable has been
modified. Buffers whose names are longer than `20` characters have their
identifications trimmed so that they can fit properly in the mode-line.

The following function available at `core/corvus-utilities.el` does the
trimming:

```elisp
(defun trim-function-name (fun-path)
  "Given a function path, trim it so that it only displays the
first 20 characters of the function name."
  (let* ((fun-name (get-proper-function-name fun-path)))
    (if (> (length fun-name) 20)
        (concat (truncate-string-to-width fun-name 17) "...")
      fun-name)))
```

Finally, the `mode-line-buffer-identification` is modified:

```elisp
(setq-default mode-line-buffer-identification
              (quote
               (:eval (trim-buffer-name (buffer-name)))))
```

And appended to the mode-line via `mode-line-buffer-identification`.

#### Has been locked

A locked buffer is will always be visible on the windows it appears, no matter
what action we do. The icon `[Locked]` appears in the mode-line to indicate if a
buffer is locked or not.

```elisp
'(:eval (when locked-buffer-mode
                 (propertize " [Locked]" 'face '(:foreground "#ebcb8b"))))
```

### Major and minor modes

Corvus mode-line *should* only show minor and major modes that are relevant to a
buffer. Taking this into consideration, the command `diminish` should be used to
hide irrelevant minor modes. The major and minor modes are appended to the
mode-line via `mode-line-modes`.

### Version Control

The function `vc-mode` shows if the current file is managed under a version
control system like `git`. In case it is, is shows which version control is
handling the system and the current branch. It is appended to the mode-line via
`(vc-mode vc-mode)`.

### Lines and columns

The mode-line specifies the current line and column the pointer is at on the
buffer. It is appended to the mode-line via `(%l, %c)`, where `%l` is the line,
and `%c` is the column.

### Line percentage

Corvus mode-line, additionally, indicates the line percentage position of the
pointer. It's similar to `mode-line-percent-position` which indicates the
current percentage of the visited buffer, however this approximation is more
precise. It is appended to the mode-line via `[%d]`, where `%d` is
`current-line/total-lines`.

```elisp
(format "%d" (/ (* (line-number-at-pos) 100)
                                (count-lines (point-min) (point-max))))
```

### End spaces

Mode-line construct to put at the end of the mode-line.
