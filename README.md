# nxs-emacs.d

Min personlige Emacs-konfiguration.

Konfigurationen er beregnet til Emacs 30 eller nyere og består af
`early-init.el`, `init.el` og mindre moduler i `lisp/`.

## Installation

Klon repository'et til en valgfri mappe, og start Emacs med mappen som
konfigurationsmappe. Eksempel:

```sh
git clone https://github.com/algon236/nxs-emacs.d.git ~/.config/nxs.emacs.d
emacs --init-directory ~/.config/nxs.emacs.d
```

Personlige oplysninger og lokale data hører til i `var/private.el`, som ikke
versionsstyres. Installerede pakker, cache, historik og andre genererede filer
er ligeledes udeladt.

## Baggrund og kreditering

Konfigurationen er videreudviklet fra Rahul Martim Juliatos
[Emacs Solo](https://github.com/LionyxML/emacs-solo). De oprindelige
copyright- og licensoplysninger i kildefilerne gælder fortsat.

## Licens

Se [LICENSE](LICENSE).
