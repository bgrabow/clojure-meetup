# Atom Clojure Party REPL Setup

This is a beginner-friendly Clojure setup for Atom, with a minimal set of package dependencies and custom keybindings. Credit goes to [@jasongilman](https://github.com/jasongilman) for his excellent [ProtoREPL setup guide](https://gist.github.com/jasongilman/d1f70507bed021b48625/cde5626b1df66fc62c315c38b52f827b46777d61), from which I copied much of the content for this guide.

## Install Atom

[Download Atom](https://atom.io/)

The [Atom documentation](https://atom.io/docs) is excellent. It's highly worth reading the flight manual.

## Install Java and Leiningen

* Java Development Kit (choose one of the following; I prefer Amazon Corretto)
  * [Amazon Corretto](https://docs.aws.amazon.com/corretto/latest/corretto-11-ug/downloads-list.html)
  * [AdoptOpenJDK](https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot)
  * [Other options](https://en.wikipedia.org/wiki/OpenJDK#OpenJDK_builds) (mind the license restrictions)
* [Leiningen](http://leiningen.org/)

## Install These Packages

These are the ones I install related to Clojure development.

* [Party REPL](https://atom.io/packages/clojure-party-repl) - Clojure REPL, remote pairing support
* [Parinfer](https://atom.io/packages/parinfer) - Handles parentheses and general Lisp editing.
* [lisp-paredit](https://atom.io/packages/lisp-paredit) - Used only for some basic cursor motions (expand/contract selection, forward/backward s-expression)
* (optional) [teletype](https://atom.io/packages/teletype) - Remote collaboration tool (required for Party REPL's remote pairing support)

## Package Settings

### language-clojure

This is the built in package that comes with Atom for the Clojure Grammar. I find the default settings bad for the way that I work. I recommend changing them to the following.

* Auto Indent: unchecked
* Auto Indent On Paste: unchecked
* Non Word Characters: `())"':,;~@#$%^&{}[]`
* Tab Length: `1`

Everything else is left at the default.

### lisp-paredit

* Enabled: checked
* Strict: unchecked
* Indentation Forms: `try, catch, finally, /^let/, are, /^def/, fn, cond, condp, /^if.*/, /.*\/for/, for, for-all, /^when.*/, testing, doseq, dotimes, ns, routes, GET, POST, PUT, DELETE, extend-protocol, loop, do, case, with-bindings, checking, with-open`
* Keybindings Enable: unchecked

### parinfer

* Smart Mode: checked

## Atom Settings

These are main Atom Settings related to Clojure that are different than the default.

* Auto Indent On Paste: unchecked
* Scroll Past End: checked
  * Due to [this autocomplete issue](https://github.com/atom/autocomplete-plus/issues/680) there is a lot of flashing from the autocomplete window that pops up. Scrolling down farther usually resolves the issue.

## `config.cson`

Open your `config.cson` file (enter `Application: Open Your Config` in Atom command palette) and add the following under the `".clojure.source":` heading:

```cson
  "bracket-matcher":
    autocompleteCharacters: [
      "()"
      "[]"
      "{}"
      "\"\""
      "“”"
      "‘’"
      "«»"
      "‹›"
    ]
```

## `keymap.cson`

Open your `keymap.cson` file (enter `Application: Open Your Keymap` in Atom command palette) and add the following entry:

```
'atom-text-editor[data-grammar~="clojure"]':
  'alt-up': 'lisp-paredit:expand-selection'
  'alt-down': 'lisp-paredit:contract-selection'
  'alt-right': 'lisp-paredit:forward-sexp'
  'alt-left': 'lisp-paredit:backward-sexp'
```

These keybindings give you basic navigation capabilities. `alt-up` and `alt-down` expand and contract your text selection by one level in the s-expression tree.
