# unbound

A propositional logic library (Clojure/Script).

![Spock](https://pixel.nymag.com/imgs/thecut/slideshows/2014/9/eyebrows/eyebrows-29.nocrop.w840.h1330.2x.jpg)


## Usage

No releases are provided at this time. You can use unbound as a git dependency:
https://clojure.org/guides/deps_and_cli#_using_git_libraries

```clojure
(ns my.ns
  (:require [unbound.logic :as l]))
```

### Unification

```clojure
(l/unify '(X 1 (foo Z)) '(2 Y (foo X)))
;=> {Z 2, Y 1, X 2}
```

### Query

#### Facts

```clojure
(let [line-facts '[(vertical (line (point X _) (point X _)))
                   (horizontal (line (point _ Y) (point _ Y)))]
      question '(horizontal (line (point 1 1) (point 2 Y)))]
  (l/query line-facts question))
;=> {Y 1}
```

#### Rules

```clojure
(let [facts-and-rule '[(f a)
                       (f b)
                       (g a)
                       (g b)
                       (h b)
                       ((k X) :- (and (f X) (g X) (h X)))]]
  (query facts-and-rule '(k Y)))
;=> {Y b}
```

## API Documentation

Still in flux.
See the [tests](test/) and [docstrings](src/) for more info.

## About

In order to evaluate the current options available to me for logic programming in Clojure I decided to learn Prolog.
As part of that I felt drawn to implement some of the basic principles myself.
At the heart of Prolog is the concept of Unification.
Implementing unification felt akin to implementing eval/apply of Lisp.
The core idea fits in very few lines of self-referential interdependent code,
and can solve otherwise baffling logic questions.


## License

Copyright © 2019 Timothy Pratley

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
