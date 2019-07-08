# collatz-conjecture

A Quil sketch designed to draw animated collatz conjecture with funky colors.

## Usage

LightTable - open `core.clj` and press `Ctrl+Shift+Enter` to evaluate the file.

Emacs - run cider, open `core.clj` and press `C-c C-k` to evaluate the file.

REPL - run `(require 'collatz-conjecture.core)`.

## Controls 
Default controls:
    space :reset-all
        Q :dec-not-forks-angle
        W :reset-not-forks-angle
        E :inc-not-forks-angle
        A :dec-even-angle
        S :reset-even-angle
        D :inc-even-angle
        Z :dec-odd-angle
        X :reset-odd-angle
        C :inc-odd-angle
        R :inc-init-angle
        T :reset-init-angle
        Y :dec-init-angle
        F :inc-root
        G :reset-root
        H :dec-root
        V :inc-max-val
        B :reset-max-val
        N :dec-max-val

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
