# ahk-mode

An Emacs major mode for editing [AutoHotKey](https://autohotkey.com/) (AHK) script files.

forked form [ahk-mode](https://github.com/punassuming/ahk-mode), to fit in personal usage.

## Installation

Clone this repository, then add to your load-path then require:

``` emacs-lisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/ahk-mode/")
(require 'ahk-mode)

;; alternative
(use-package ahk-mode
  :load-path "c:/Users/incubator/Projects/ahk-mode"
  :ensure nil)

```

## Usage

download [ahk2-lsp](https://github.com/thqby/vscode-autohotkey2-lsp) for completion:

``` powershell
mkdir vscode-autohotkey2-lsp
cd .\vscode-autohotkey2-lsp\
curl -o install.js https://raw.githubusercontent.com/thqby/vscode-autohotkey2-lsp/main/tools/install.js
node .\install.js

```

then, add the following to your emacs config file:

``` emacs-lisp
(add-to-list 'eglot-server-programs '(ahk-mode "node" "C:/Users/incubator/Projects/vscode-autohotkey2-lsp/server/dist/server.js" "--stdio"))

;; if you prefer not enabling eglot on prog-mode, add the following line to activate only on ahk-mode
(add-hook 'ahk-mode-hook #'eglot-ensure)
```

and you are good to go.

## License

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> inside emacs to view it.
