# translate-shell.el

`translate-shell.el` is an unofficial Emacs front-end for
[translate-shell](https://github.com/soimort/translate-shell). translate-shell
is command-line translator powered by Google Translate.

## Installations

To install manually, make sure this file is saved in a directory in your
`load-path`, and add the line:

    (require 'translate-shell)

to your Emacs initialization file.

## Usage

You can call `translate-shell-brief` and `translate-shell` to get the
definition your query word. The result will display in echo area and
"*Translate Shell*" buffer. You might want to bind them to some keys.

## Cache

The result returned from `trans` (the translate-shell command-line program)
are stored in the `translate-shell-brief-cache` and `translate-shell-cache`
variable.

If you want to use cache even after restarting Emacs, you need tt save these
two variable yourself. An easy solution is using `desktop.el`, see below.


## Customization

Below is my customization around this package:

```elisp
(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config
  (add-to-list 'desktop-globals-to-save 'translate-shell-cache)
  (add-to-list 'desktop-globals-to-save 'translate-shell-brief-cache))

(use-package translate-shell
  :load-path "~/wip/translate-shell.el"
  :bind (("C-c s"   . translate-shell-brief)
         ("C-c S"   . translate-shell))
  :config
  ;; <https://translate.google.com> is blocked in China for no apparent
  ;; reason. No one ever asked my option.
  (setq translate-shell-command "proxychains4 -q trans -t en %s"
        translate-shell-brief-command "proxychains4 -q trans -brief -t zh %s"))
```
