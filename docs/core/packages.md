# Corvus Packages

Documentation for the main dependencies of Corvus, and the automatic
installation of packages.

All of these packages can be found on MELPA.

```elisp
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
```

## List of Packages

| Package          | Description                                             |
|:-----------------|:--------------------------------------------------------|
| Anzu             | Show the number of matches in the mode-line.            |
|                  |                                                         |
| Crux             | A collection of ridiculously useful extensions (CRUX).  |
|                  | Mainly used for the following commands:                 |
|                  | * `crux-move-beginning-of-line`                         |
|                  |                                                         |
| Diff-hl          | Emacs package for highlighting uncommitted changes.     |
|                  |                                                         |
| Diminish         | Properly hide specified minor modes from the mode-line. |
|                  |                                                         |
| Dumb Jump        | Jump-to-Definition functionality.                       |
|                  |                                                         |
| Expand Region    | Smart expanding system, useful to select words.         |
|                  |                                                         |
| Flycheck         | Spell-checking on the fly as one types.                 |
|                  |                                                         |
| Highlight Indent | Shows in a more friendly way how code is indented.      |
| Guides           |                                                         |
|                  |                                                         |
| Magit            | Git interface for Emacs.                                |
|                  |                                                         |
| Multiple Cursors | Allows the editor to have multiple cursors (pointers).  |
|                  |                                                         |
| Nord Theme       | Cold Theme for the editor.                              |
|                  |                                                         |
| Smartparens      | Automatic closure for multiple pairs, such as:          |
|                  | parenthesis, brackets, etcetera.                        |
|                  |                                                         |
| Super Save       | Automatic save functions for certain actions, like:     |
|                  | Switching buffers and windows.                          |
|                  |                                                         |
| Windmove         | Functions to move between windows.                      |
    
## Automatic installation

The following functions handle the automatic installation of the required
packages.

### Install package

The following function checks if a package is already installed, if not, install
it using the `package-install` command.

```elisp
(defun corvus-install-package (package)
  "Install the given package when necessary."
  (if (package-installed-p package)
      (corvus-message (format "Package %s is already installed." package))
    (package-install package)
    (corvus-message (format "Installed package %s." package))))
```

### Install packages

Given a list of packages names, the following functions tries to install them;
in case a package is already installed move on to the next.

`Note:` Before installing packages, it's necessary to initialize the already
downloaded packages by using the `package-initialize` command.

```elisp
(defun corvus-install-packages ()
  "Install all the dependencies of Corvus"
  (corvus-message "Initializing packages...")
  (package-initialize)
  (corvus-message "Installing dependencies...")
  (mapc #'corvus-install-package corvus-main-packages))
```
