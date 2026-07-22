# nxs-emacs.d

My personal Emacs configuration.

The configuration is intended for Emacs 30 or later and consists of
`early-init.el`, `init.el`, and smaller modules in `lisp/`.

## Installation

Clone the repository into a folder of your choice, then start Emacs using that
folder as its configuration directory. For example:

```sh
git clone https://github.com/algon236/nxs-emacs.d.git ~/.config/nxs.emacs.d
emacs --init-directory ~/.config/nxs.emacs.d
```

Personal information and local data belong in `var/private.el`, which is not
version-controlled. Installed packages, cache files, history, and other
generated files are likewise excluded.

## Background and attribution

The configuration is further developed from Rahul Martim Juliato’s
[Emacs Solo](https://github.com/LionyxML/emacs-solo). The original copyright
and licence information in the source files remains applicable.

## Licence

See [LICENSE](LICENSE).
