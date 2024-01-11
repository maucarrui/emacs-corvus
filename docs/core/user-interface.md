# Corvus User Interface

This documentation explains how Corvus achieves certain user interface aspects.

## Disable start-up screen

By default, Corvus disables the start-up screen available in Vanilla Emacs.

```elisp
(setq inhibit-startup-screen t)
```

## Always start full-screen

When initializing, Corvus starts at full-screen; the screen can be resized
afterwards.

```elisp
(add-to-list 'default-frame-alist '(fullscreen . maximized))
```

`Note:` In certain screens, Emacs is not properly maximized leaving an empty
space at the bottom of the screen. The following command fixes the problem:

```elisp
(setq frame-resize-pixelwise t)
```

## Adapt font-size to screen

Depending on the size of the screen, Corvus can adapt the font size to fit it
properly.

```elisp
(if (> (display-pixel-width) 1600)
    (set-face-attribute 'default (selected-frame) :height 120)
  (set-face-attribute 'default (selected-frame) :height 110))
```

## Disabling tool, menu, and scrolling bars

The tool, menu, and scrolling bars are disables by default in Corvus.

```elisp
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
```

## Line numbers

Always display the line numbers for all available modes.

```elisp
(global-display-line-numbers-mode)
```

## Fringes

The fringe is the same color as the background.

```elisp
(set-face-attribute 'fringe nil :background nil)
```

## Truncate long lines

Vanilla Emacs has the option to wrap long lines so that the contents are always
visible on screen; this can lead to certain lines having more width and height
than others. To have a more consistent design on most of the cases, Corvus
truncates lines so that they all have the same width and height.

```elisp
(setq-default truncate-lines t)
```

## Indentation Highlighting Guides

Indentation guides, as the name implies, are a way to visualize in a more 
friendly way indentations. In this case, the `|` char is used as a guide.
The following function is an example of how this guide is shown:

```golang
func EqualByteMatrix(a, b [][]byte) bool {
|	if len(a) != len(b) {
|	|	return false
|	}
|	for i, v := range a {
|	|	if !bytes.Equal(v, b[i]) {
|	|	|	return false
|	|	}
|	}
|	return true
}
```

The package `highlight-indent-guides` is the one responsible for this
functionality.

```elisp
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
```

This package allows the use for many different guides. For example, instead of
using the character `|`, we can define a bitmap. However, in the case for
Corvus, the most useful/simplistic approach is to use the mentioned character.

```elisp
(setq-default highlight-indent-guided-method 'character)
```

Guides can also be highlighted depending on where the cursor is.

```elisp
(setq-default highlight-indent-guides-responsive 'top)
(setq-default highlight-indent-guides-delay 0)
```

## Git diff highlights

The following configurations are designed for the `diff-hl` package.

### Bit-map for changes

The bit-map is a matrix that specifies the form that will be shown in the
fringe. In the case for the diffs, whether they're adding, removing or modifying
a line, the expected symbol is ` |` (a line with some padding to the left).

```elisp
(let ((bitstr (fringe-helper-convert
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX")))
  (define-fringe-bitmap 'my-diff-hl-bitmap bitstr nil nil 'center))
(setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap))
```

### Diffs on the fly

As one modifies a version-controlled file, show the changes (addition, removal,
or modification) on the fly at the left fringe for the supporting major modes.

```elisp
(setq diff-hl-flydiff-delay 0.05)
(diff-hl-flydiff-mode)
(global-diff-hl-mode)
```

### Version-control accuracy with Magit

Update the fringes appearance after committing with Magit.

```elisp
(with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
```

### Version-control in dired mode

Enable `diff-hl` fringe indicator on dired mode.

```elisp
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
```
