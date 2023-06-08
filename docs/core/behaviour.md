# Corvus Behaviour

This documentations explains the different behaviours Corvus can have.

## Buffers

### Backups

Vanilla Emacs has a backup feature so that the (most recent) contents of the
file you're editing are saved, even when you didn't saved it manually. These
files are usually named after the original but with a trailing `~`. Example:
`behaviour.md~`.

This backup files are created on the same directory your original file is
located; this can lead to having unnecessary files in a specified directory. To
solve the previous problem, all of the backup files are stored in the temporary
file directory (`/tmp`).

```elisp
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
```

### Keeping buffers automatically up-to-date

A buffer can get out of sync with respect to its visited file on disk if that
file is changed by another program. To keep it up to date, Corvus enables Auto
Revert mode globally.

```elisp
(global-auto-revert-mode t)
```

### Auto-Saves

Corvus automatically saves buffers associated when one of the following actions
is done:

* Switching to another window.

```elisp
(require 'super-save)
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)
```

### Buffers sharing names

To deal with buffers sharing the same name, Corvus specifies the directory
they're located by placing it after the buffer name and with the separator `|`.
Example: The buffer name associated with files `A/text.txt` and `B/text.txt` are
`text.txt | A` and `text.txt | B`, respectively.

```elisp
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
```

### Locking buffers

When working with multiple buffers, certain actions will replace a previous
buffer with a new one. For example, the command `C-h v` will either replace or
create a new window to describe a variable. This can lead sometimes to
situations where buffers we're actively using are replaced for another.

To solve the previous problem a new minor mode is defined in Corvus,
`locked-buffer-mode`. When interacting with the minor mode, the current window
is locked to the current buffer; no matter what action is performed, the window
containing a locked buffer will not show a different buffer, until it's
unlocked.

To lock a buffer simply execute the command `locked-buffer-mode`, to unlock it
simply execute the command again. You can know which buffers are locked or not by
checking in the mode-line if the icon `[Locked]` appears.

```elisp
(define-minor-mode locked-buffer-mode
  "Lock the current window to display the current buffer."
  :init-value nil
  :lighter " Locked"
  :keyword nil
  (set-window-dedicated-p (selected-window) locked-buffer-mode))
```

## Windows

### Moving between windows

Corvus allows the use of the Shift + Arrow Keys to move between windows.

```elisp
(require 'windmove)
(windmove-default-keybindings)
```

## Compilation

The following configurations are for the behaviour of the `M-x compile` command.

```elisp
(require 'compile)
```

### Save before compiling

When asked to compile, Corvus will save all modified buffers without asking
before compilation.

```
(setq compilation-ask-about-save nil)
```

### Kill previous compilation process

Corvus will always kill a running compilation process before starting a new one.

```elisp
(setq compilation-always-kill t)
```

### Point at the end of output

Corvus puts the point at the end of the compilation output window.

```elisp
(setq compilation-scroll-output nil)
```

## Spell Checking

The following configurations are for the behaviour for Corvus' spell checker,
`Flyspell`, which enables on-the-fly spell checking in Emacs by the means of a
minor mode.

`WARNING:` Spell checking commands only work if a spelling checker program is
already installed. `Flyspell` default program is `ispell`, however Corvus
requires `Hunspell` to properly work. The default language is english.

```elisp
(require 'flyspell)
(with-eval-after-load "ispell"
    (setenv "LANG" "en_US.UTF-8")
    (setq ispell-program-name "hunspell")
    ...
    )
```

### Multiple languages

`Hunspell` is capable of working with multiple dictionaries, that means that
Corvus is capable of detecting spelling mistakes of multiple languages at the
same time without having to specify a change of dictionaries.

Corvus default languages are: British and American English, and Mexican Spanish.

```elisp
(setq ispell-dictionary "en_GB,en_US,es_MX")
(ispell-set-spellchecker-params)
(ispell-hunspell-add-multi-dic "en_GB,en_US,es_MX")
```

### Personal dictionary

Corvus defines a file to save words to a personal dictionary
`~/.hunspell_personal`. The personal dictionary file has to exist, otherwise
`Hunspell` will write directly to another file; in case the file doesn't exists,
Corvus creates an empty personal dictionary file.

```elisp
(setq ispell-personal-dictionary "~/.hunspell_personal")

(unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))
```

### Major modes with spell checking

Spell checking is enabled for the major modes contained in `text-mode` and
`prog-mode`.

```elisp
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
```

## Auto-completion configurations

The completion library used in Corvus is `company`. By default it is enabled in
all major groups contained in `prog-mode`.

```elisp
(add-hook 'prog-mode-hook #'company-mode)
```
