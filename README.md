# ng2-ts-snippets

Integration of [Angular Cli](https://cli.angular.io/) to
[emacs](https://www.gnu.org/software/emacs/)
[ng2-ts-mode](https://github.com/AdamNiederer/ng2-mode) via
[YASnippets](http://joaotavora.github.io/yasnippet/). Installation and usage of
this software assumes some familiarity with all of these.

# Installation

You can install [ng2-ts-mode](https://github.com/AdamNiederer/ng2-mode) and
[YASnippets](http://joaotavora.github.io/yasnippet/) via
[Melpa](https://melpa.org/#/). Install ng-ts-snippets the usuarl way, for
example:

```emacs-lisp
(setq yas-snippet-dirs '("~/local/ng2-ts-snippets/snippets"))
(yas-global-mode 1)
```

# Usage

There is only a single snippet

*ng-gen*

Open the file you want to generate, i.e. my-component.component.ts and expand
the ng-gen (ng-gen[TAB])


## License

GPL 3.0
