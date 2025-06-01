---
title: Common Lisp is a dumpster
categories: ['Functional Programming', 'Weird Programming']
tags: ['lisp', 'common lisp', 'sbcl', 'prog']
img: /img/posts/cl-is-a-dumpster.jpg
img_caption: >-
  Image by @pch-vector on Freepik
description: >-
  Common Lisp has a LOT of stuff in it. Some of it is charming. Some of it is
  plain weird

---

# Intro

I've switched from Ruby and Clojure for my personal stuff to Common Lisp a couple of years ago. I think it fits my programming style very well. But it's a very strange language.

CL has **a lot** of stuff in it. It feels like it tried to cater to many different types of programmers all at once. It mixes functional, procedural, and object-oriented ideas. It makes use of dynamic binding (which isn't very common nowadays) whilst being mainly lexically scoped and supporting closures.

However, some things in CL don't seem practical at all. Or maybe just weird. In this essay I'm going to show some things I'm weirded out by.

<!--more-->

Starting with the strangest one&#x2026;


# The prog macro

From the ANSI standard:

> Three distinct operations are performed by `prog` and `prog*`: they bind local variables, they permit use of the `return` statement, and they permit use of the `go` statement.

Essentially, it allows you to use a "goto" and "return" like in older imperative languages. Other related special forms are `block` and `tagbody`. In fact, in SBCL `prog` is a macro defined in terms of `block`, `tagbody`, and `let`.

Here's the example provided in the standard:

```lisp
(defun king-of-confusion (w)
  "Take a cons of two lists and make a list of conses.
   Think of this function as being like a zipper."
  (prog (x y z)          ;Initialize x, y, z to NIL
     (setq y (car w) z (cdr w))
   loop
     (cond ((null y) (return x))
           ((null z) (go err)))
   rejoin
     (setq x (cons (cons (car y) (car z)) x))
     (setq y (cdr y) z (cdr z))
     (go loop)
   err
     (cerror "Will self-pair extraneous items"
             "Mismatch - gleep!  ~S" y)
     (setq z y)
     (go rejoin)))
```

Followed by:

```lisp
(defun prince-of-clarity (w)
  "Take a cons of two lists and make a list of conses.
   Think of this function as being like a zipper."
  (do ((y (car w) (cdr y))
       (z (cdr w) (cdr z))
       (x '() (cons (cons (car y) (car z)) x)))
      ((null y) x)
    (when (null z)
      (cerror "Will self-pair extraneous items"
              "Mismatch - gleep!  ~S" y)
      (setq z y))))
```

This oddball existed since LISP-1. Both LISP-1 and LISP-1.5 programmer's manuals refer to it as "The Program Feature". Considering that the manuals dedicate a whole chapter to it, it makes me think John McCarthy and his colleagues thought it was an important facility.

Interestingly, the LISP-1.5 manual says it enables "Algol-like" style, while the LISP-1 manual compares it to FORTRAN.

Reading both manuals, I got the impression that the assignment operators (`setq` and `set` in that case) only worked within the `prog` macro. My guess is that "pure" Lisp was intended to be used to write math-like expressions (many people I've met do say Lisp was based on Lambda Calculus).

Also, according to "The evolution of Lisp" (Steele, Gabriel), `prog` may have been used instead of `let` (`let` itself wasn't really a thing until late 70s). The only other way to introduce new bindings was to call a function (or lambda). Like this:

```lisp
((lambda (seconds-in-a-day) (* 20 seconds-in-a-day)) (* 60 60 24))
```


## prog1, prog2, progn

Confusingly, `prog1`, `prog2`, and `progn` don't have much in common with `prog`.

-   `prog1` evaluates all forms in order and returns the value returned by the 1st form
-   `prog2` works just like `prog1` but returns the result of the 2nd form.
-   `progn` is quite commonly used and returns the last form. It helps in cases where a single form is expected but one needs to evaluate a few.

Interestingly, only `prog2` was included in LISP-1 and it worked differently: it only accepted 2 forms returning the result of the second one. This further supports my theory that lisp was intended to be used to simply evaluate math expressions. It seems that only `prog` allowed imperative programming style in the original LISP (modern lisps support imperative style without it).


## progv

Yet another outlier. All `progv` does is it creates some temporary dynamic variables. I struggle to come up with a use for it but HyperSpec has this note:

> Among other things, `progv` is useful when writing interpreters for languages embedded in Lisp; it provides a handle on the mechanism for binding dynamic variables.


## Opinion

I think `prog`, `prog1`, and `prog2` are artefacts of the past that largely have been dragged into the language for compatibility reasons (to consolidate different Lisp implementations, e.g. MacLisp and Interlisp).

`prog` (`tagbody` and `block`) simply enables the old-school imperative programming style. Maybe it was introduced because some algorithms are hard to expressive in functional programming. Or maybe it was introduced because they tried to cater to the crowd who didn't want to do FP all the time. Who knows?

On the other hand, it is a whole different programming style allowed in the language. I even used `block` and `return` a couple of times where I thought it gave a more straightforward algorithm. I'll allow it.

However, `prog1` and `prog2` don't seem to bring *anything* to the table. Their behaviour can be achieved with a simple `let`.


# Assignment operators

One thing I had to google a while ago was `setq` vs `setf`. Being a long time Emacs user, I'm accustomed to `setq` however I've seen CL programmers using both of them with seemingly no difference.

Well, the difference is that `setf` also works for some things that aren't variables. For example, parts of a cons-cell:

```lisp
(let ((xs '(1 2 3)))
  (setf (car xs) 5)
  xs)
;; ==> (5 2 3)
```

You may be wondering where the names come from. Well, originally, the lowest level assignment function was `set` which takes a symbol and associates a value with it:

```lisp
(set (quote x) 123) ;; I don't think the reader macro ' existed in LISP-1
```

Since this construction was so common, the macro `setq` was created (for "set quoted").

The etymology of `setf` is a bit more complex. The "Evolution of Lisp" attributes the idea to Peter Deutsch (who also was one of the creators of LISP-1.5, worked on Smalltalk, and other cool things). Essentially, he suggested that functions like `car` have two modes: "load" and "store", so they can be used both for getting values as well as setting.

> However, his syntax is suggestive; here is the proposed definition of RPLACA:
>
> `(lambda (x y) (setfq (car x) y))`
>
> Deutsch commented that the special form used here is called SETFQ because “it quotes the function and evaluates everything else.” This name was abbreviated to SETF in Lisp-Machine Lisp. Deutsch attributed the idea of dual functions to Alan Kay.
>
> &#x2013; The Evolution of Lisp

So I guess the "f" in `setf` simply means "function" because it can work with dual-mode functions like `car`. Likely, `setf` was first introduced in the MIT Lisp-Machine Lisp dialect.

Considering that normally `setf` is doing the exact same thing `setq` does, there's no reason to keep both, in my opinion. `setq` could be simply a private function within `COMMON-LISP` package since `setf` is a macro defined in terms of lower level functions like `setq`. Speaking of which&#x2026;


## Other assignment operators

`setf` and `setq` aren't the only assignment operators present in Common Lisp. Here's the full list of operators I found:

-   `set` - assigns value to the symbol provided, e.g. `(set 'x 123)`
-   `setq` - from "set quoted". That is, `(set 'x 123)` is the same as `(setq x 123)`. One interesting difference is that `setq` can't assign variables dynamically.
-   `rplaca`, `rplacd` - modify car/cdr of a cons cell. It appeared in LISP-1.5. I'm guessing Deutsch felt that having a single macro for assignment would've been more convenient. Which it is.
-   `setf` - the general assignment macro.
-   `incf=/=decf` - like `setf` but simply updates the value by some delta (1 by default).

I'm sure there're more. Can never have enough, right?


# NIL, (), and T

Stepping away from functions and macros for a second, let's take a look at something more fundamental. `NIL` and `T` in Common Lisp are *weird*. Where to begin?

Let's start with `nil`:

-   `nil` is the only false value in the language. Everything else is treated as true.
-   It's a symbol that evaluates into itself. `(symbolp nil)` is true.
-   It *also* means empty list. In fact `()` and `nil` are exactly the same, no difference except the spelling. `(listp nil)` is true.
-   It's **not** a cons cell (`(consp nil)` is true) however `car` and `cdr` accept it with no errors. This behaviour was popularised by Interlisp but wasn't universally accepted.
-   It has its own type: `(type-of nil)` returns `null`.

Now `t` is hard to explain (mainly because I don't fully understand it myself):

-   It's simply a symbol that evaluates into itself.
-   It's got its own type: `(type-of nil)` returns `boolean`. Which is weird because there're no other boolean values I know of.
-   It's used as some magic constant in different contexts, for example:
    -   `format` uses `t` as stdout stream.
    -   `t` is the omnipresent "supertype". Everything is a subtype of `t`.
    -   `cond` often uses `t` as its else-clause. It's not magic though as any value except `nil` would do the trick. Personally, I use `:else` for that but in LISP-1 and LISP-1.5 manuals they used `T` so I guess it was/is idiomatic (I'll stick to `:else` though, thank you very much).


# List mapping functions

In a FP language, one would expect to have a `map` function. Common Lisp has at least 9!

Here they are:

-   `map` - generic function. It works with all `sequence` objects (which includes lists, arrays, strings) and returns the type you want.
-   `mapcar` - the `map` function you'd expect. But it works only with lists
-   `mapc` - like `mapcar` but doesn't build a list with returned values. Pretty much intended for side-effects.
-   `maplist` - calls the function on the successive CDRs of the list starting with the full list. So for `(1 2 3)` it'd call the function with `(1 2 3)`, `(2 3)`, and `(3)`.
-   `mapl` - this is to `maplist` what `mapc` is to `mapcar`. It runs the function on successive CDRs but doesn't build a new list.
-   `mapcan` - uses `nconc` (ugh. "conc" from "concatenate" and "n" from&#x2026; "n"?). Basically, a "flatmap" function: it returns a concatenation of the lists returned by the provided function.
-   `mapcon` - like `mapcan` but uses successive CDRs of the lists (starting with the full list).
-   `map-into` - kinda like `mapcar` but requires to provide a destination list that will be filled with the results.
-   `maphash` - calls a 2-argument function for each entry of a hash. Returns `nil`.
-   `loop` - not exactly a mapping function but it's so over-complicated it can be used as such. For example:

    ```lisp
    (loop for x in '(1 2 3)
          collect (* x x))
    ```

That's a lot of functions. And I'm pretty sure that's not even all of them, just the ones I've seen/found. The naming is pretty terrible and inconsistent (because the API wasn't designed by a single person).

Fun fact: in LISP-1, `map` worked like `mapl`. Other functions provided were `maplist` and `mapcon`. Interestingly, there was no `mapcar`. I guess `maplist` was the bare minimum needed to process lists without recursion. Maybe the original designers didn't think of having something like `mapcar` (as it's really just a subset of `maplist`) and it was later introduced by the programmers that used Lisp.


# Loop

I've already mentioned the `loop` macro above. But I think it deserves a special place in my personal hell.

The ANSI standard has a whole section dedicated to it. It's 30 pages explaining how to use a *single* macro. There're roughly 25 keywords in `loop` and they can be combined together infinitely. Here's an example from the ANSI standard:

```lisp
;; Group conditional clauses.
(loop for i in ’(1 324 2345 323 2 4 235 252)
      when (oddp i)
        do (print i)
        and collect i into odd-numbers
        and do (terpri)
      else  ; (evenp i)
      collect i into even-numbers
      finally
         (return (values odd-numbers even-numbers)))
```

At this point, it's its own programming language. Pretty sure it's also Turing-complete.

I'm not advocating for removal or anything. In fact, I think it's a pretty good feature. But it's overly complex and I prefer to keep things simple. I never use it in my own code unless I'm worried about performance. Generally, I prefer using recursion and `dolist`.

Although as part of writing this essay I also wrote a docstring for it (SBCL doesn't have a docstring for `loop`) and may start using it a little. Maybe it's an acquired taste


# Quote and backquote

The only difference between backquote and quote is that backquote allows unquoting with commas. It's fully backward-compatible with the normal quote. Using commas anywhere outside backquote throws an error.

```lisp
'(1 2 ,x) ; ==> error
`abc ; ==> same as 'abc
```

There's really no reason to keep both. Unless maybe it can be helpful in complex nested expressions? Personally, I'd remove backquote and add unquoting to the normal quote


# Printing

There's also a bunch of printing functions. They often don't even have consistent parameters. Some seem so niche they have no business being included in the language, IMHO.

I searched printing functions and here's the list. I explained some but the others I've never seen before and wasn't bothered to read through docs.

-   `prin1` - produces read-able output
-   `prin1-to-string` - same as `(with-output-to-string (s) (prin1 OBJ s))`
-   `princ` - like `prin1` but the output is supposed to be readable by humans, not necessarily by the lisp reader.
-   `princ-to-string` - same as `(with-output-to-string (s) (princ OBJ s))`
-   `print` - like `prin1` but adds newline in the beginning and space in the end
-   `pprint` - like `print` but without trailing space and also sets `*print-pretty*` flag.
-   `pprint-tabular` - prints out a list like a table
-   `print-object` - generic function to print different objects
-   `print-unreadable-object` - I imagine, useful in `print-object` overloads
-   `pprint-pop`
-   `print-not-readable`
-   `print-not-readable-object`
-   `pprint-dispatch`
-   `copy-pprint-dispatch`
-   `pprint-exit-if-list-exhausted`
-   `pprint-tab`
-   `pprint-indent`
-   `pprint-fill`
-   `pprint-linear`
-   `pprint-newline`


# Symbol property lists

In the context of Lisp-1 vs Lisp-2 the 1 and 2 refer to the namespaces. In Lisp-1 languages functions and variables live in the same space. Simply put, named functions are simply lambdas assigned to a variable. JavaScript works like that:

```js
function sqr(x) { return x * x }
// is the same as
var x = function(x) { return x * x }
```

In Lisp-2, on the other hand, functions aren't the same as other values. A symbol (variable) can be assigned a value and a function at the same time. The interpreter/compiler picks between the two depending on the context. Ruby works like that. Which may get confusing at times:

```ruby
def f
  "method"
end

f # ==> "method"
f = "variable"
f # ==> "variable"
f() # ==> "method"
```

However, some Lisps (I'm aware of CL, Elisp, and PicoLisp) also have a 3rd plane: property lists. A symbol can have a value associated with it, a function, and a plist.

```lisp
(defun abc () "function")
(setf abc (lambda () "lambda"))
(symbol-plist 'abc) ; ==> nil
(setf (get 'abc :hello) "world")

;;
;; symbol 'abc used in different contexts
;;
(abc) ; ==> "function"
(funcall abc) ; ==> "lambda"
(funcall 'abc) ; ==> "function"
(funcall #'abc) ; ==> "function"
(symbol-plist 'abc) ; ==> (:hello "world")
```

Looking at SBCL, it doesn't seem property lists are used extensively. There're about 416 symbols that have anything in their plist. In my running Sly session this number goes up to 631.

It seems that this facility was introduced to Common Lisp to be compatible with older Lisp dialects. It feels very unnecessary as one can achieve that by using normal variables.

The thing that winds me up the most is that this archaic feature uses the word "get" for it's function. It makes no sense to give such generic names to niche APIs.

Emacs Lisp gets more use out of symbol plists. It's used for different configuration. For example, these are some properties set in my config:

```elisp
;; 'disabled property marks certain interactive functions as disabled
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; configures how if macro should be indented in elisp code
(put 'if 'lisp-indent-function 0)
```


# Honourable mentions


## Dynamic binding

I actually love dynamic binding. However, I only use it in CL to introduce implicit parameters, e.g. API keys. I think it's great. But it does fit into my narrative of Common Lisp being a dumpster of a bunch of different things programmers came up with


## Multiple values return

In a dynamically typed language, returning multiple values from functions can be achieved with a simple list (or array, or whatever your language uses). However, Common Lisp *also* has a dedicated facility to work with multiple values.

The purpose is to be able to return multiple values without allocating extra memory (for cons cells). However, it comes at a price of having to remember special forms to deal with them.

```lisp
(defun f ()
  (values 1 2 3 4 5))

(let ((result (f)))
  result) ; ==> 1

(multiple-value-bind (one two three) (f)
  (list one two three)) ; ==> (1 2 3)
```

Is this optimisation worth it? I don't think so. **However**, `values` has a very neat feature: if you don't handle the multiple values explicitly, the extras are simply discarded like they aren't even there. This is useful when the extra returned values are only needed in certain scenarios.

A good example of that is the function `gethash`. Normally, it simply returns the value of the key or `nil`. However, it also returns a second value one can check whether the key was found or not. If they were returned as a list, it'd get pretty tiresome to keep writing `(car (gethash key hash))`.

Emacs Lisp has `gethash` too. However, it doesn't have the `values` facility so it's not trivial to check whether a key exists in a hash or not. For example, if `gethash` returns nil, does it mean the key didn't exist or that it was set to `nil`? The way I do it, I use the third parameter (the fallback value) and set it to something nonsensical and then check if the nonsense was returned.

Compare Common Lisp:

```lisp
(defun key-exists? (key h)
  (nth-value 1 (gethash key h)))
```

With Elisp:

```elisp
(defun key-exists? (k h)
  ;; HACK: simply assume nobody will ever use this value and check if gethash
  ;;       returns it
  (not (equal (gethash k h :__key-not-found)
              :__key-not-found)))

(let ((h (make-hash-table)))
  (key-exists? :somekey h) ; ==> nil
  (setf (gethash :somekey h) 123)
  (key-exists? :somekey h) ; ==> t
  (setf (gethash :somekey h) :__key-not-found)
  (key-exists? :__key-not-found h)) ; ==> nil
```

So it's a nice feature. But it does mean the language gets bigger and more complicated. I can never remember the functions and macros to process multi-value functions.


## let vs let\*

This also affects `do/do*` and `prog/prog*`. In practice, the difference is that non-asterisk versions don't guarantee any execution order and don't allow references to other variables during assignment. In theory, the assignments can happen in parallel.

I don't believe there's any practical reason to have both. And if there is, I'm sure it's better to use the more obvious behaviour by default so `let` and `let*` should be at least swapped.


## Car/cdr combinations

Just a small quirk. Often we need to get second, third, etc element from a list. This leads to constructions like `(car (cdr (cdr lst)))`. It gets even more complicated with more tree-like structured cons-cells.

That's why there're functions like `cadr` (second value), `caddr` (third), and `cddadr` (something like "second list in a list with the first two elements dropped"? it's so niche I can't even tell what it might be without context).

It's very simple and convenient: for some (up to 4?) combinations of `car` and `cdr` you can simply write `(c...r)` and insert a combination of `a` and `d` in between `c` and `r`. So `(caaadr x)` is the same as `(car (car (car (cdr x))))`.

They're pretty good functions and I use them all the time (because I prefer working only with lists). However I do find them a bit funny and somewhat low-tech haha


# Outro

I have a love-hate relationship with Common Lisp. I think that it's too old and got a lot of baggage that's not of any use to someone like me. It feels like the authors just glued together a bunch of different programming languages together and made it a lisp. It feels more like an experiment rather than a well designed and production-ready language. I mean, Common Lisp has a namespace (package) system and yet nobody bothered to organise standard library. Everything is just dumped in the `common-lisp` package. One could argue that it's because the packages are for the programmers to build libraries and software but then I'd expect the stdlib to be much thinner than this. I wasn't there and I'm not a language designer but to me it feels wrong to go out of your way in order to keep backward compatibility with a bunch of old software. Not to mention, I'm sure they still had to do a bunch of work in order to migrate from previous implementations to Common Lisp.

On the other hand, catering to different programmers made CL incredibly flexible and unopinionated. I always loved Perl's TMTOWTDI philosophy. I trust my own gut. I want to make decisions. I want to experiment. I want freedom. And Common Lisp delivers. Sort of.
