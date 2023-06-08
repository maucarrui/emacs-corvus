# Corvus Editor

This documentations explains how the editor behaves.

## Wrapping text and comments

Emacs commands like `fill-region` and `fill-paragrah` line-wrap text and
comments so that they're always visible on the buffer. The command `fill-column`
specifies from which column an automatic line-wrapping should happen.

By default, Corvus column limit is: `80`.

```elisp
(setq-default fill-column 80)
```

## Tabs

### Spaces as tabs

By default, all indentations performed by Corvus are done using space characters.

```elisp
(setq-default indent-tabs-mode nil)
```

### Length of tabs

The length of a tab is the distance between tab stops (for display of tab
characters), in columns. 

`Note:` The global value is `8`. However, major modes do tend to change this
value. If you want to specify a mode specific tab-length then you should look at
the mode's respective module.

```elisp
(setq-default tab-width 8)
```

## Delete selection

Vanilla Emacs doesn't delete the whole region (selected characters) when
pressing delete. Corvus enables this feature, which is present in many other
text editors, by enabling the `delete-selection` mode.

```elisp
(delete-selection-mode t)
```

## End of files

Corvus inserts a newline at the end of files when one is not present.

```elisp
(setq require-final-newline t)
```

## Pointer (cursor) details

### Margins

By default, Corvus specifies that there are (when possible) `3` visible lines
above and below the point (cursor).

```
(setq scroll-margin 3)
```

### Automatic reposition

Perform an automatic scrolling when the pointer is out of the visible portion of
the text.

```
(setq scroll-conservatively 100000)
```

### Scrolling preservation

Preserve the pointer position when scrolling.

```
(setq scroll-presever-screen-postion 1)
```

## Automatic parentheses and pairs

Automatically insert parenthesis, brackets, and other pairs (major mode
dependant).

```
(require 'smartparens-config)
(smartparens-global-mode 1)
```

## Search and Replace

### Number of matches

Display in the mode-line the current amount of matches of a given search.

```
(require 'anzu)
(global-anzu-mode)
```
