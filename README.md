# emacs-parinfer

Parinfer (written by @shaunlebron http://shaunlebron.github.io/parinfer/#editor-plugins)
is a fantastic new solution to the time-honored problem of keeping track of your
parentheses and keeping your code aligned (not just Lispers know this pain).
This is a work-in-progress to bring Parinfer to Emacs as a minor-mode.

## Installation

Right now, this minor mode is not functional so **do not install**.

## To Be Done

As noted above, this is a project to port @shaunlebron's Parinfer from clojureScript to elisp. As you will see from the existing code, this is in process.

The central "plan" for this was to recode from CLJS to ELISP and then iron out any kinks. Thus allowing us to have a native, minor-mode version of parinfer;
without having to resort to any node.js/RPC tricks.

Some assistance that I need is porting the following clojure/clojureScript functions to ELISP:
* assoc
* assoc-in
* cond->
* iterate
* merge
* merge-wth
* peek
* select-keys
* seq
* slurp
* spit
* update
* update-in

## Credits

http://shaunlebron.github.io/parinfer/#editor-plugins

## License

MIT License (this needs to be cleaned up in the files)
