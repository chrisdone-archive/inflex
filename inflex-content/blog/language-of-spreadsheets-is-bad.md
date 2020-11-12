Article {
 title = "The language of spreadsheets is bad",
 date = 2020-11-13,
 content = ""
}

The user interface in a spreadsheet is modal. There are two modes:

* The first mode is **code** (also known as: formula): `=SUM(A1:A5)`
* The second mode is the **result** of running that code: `234`

You type in some equation, some mathematics, some conditions, and then
you hit return, and then you see the results.

## Declarative programming is a good fit

The programming language that you write your code in is a declarative
programming language. We call programming languages declarative when
you say what you want (declarative) more than instructing the computer
how to go about achieving it (imperative). When languages are
declarative, it gives the system more freedom to make choices about
how to go about computing results.

The code in spreadsheets is this kind of language. It has to be. When
you change code in one cell, all the other cells are updated. If the
language was imperative, that would mean it could change
things. That's bad when cells are re-run all the time during your
course of work. It'd be chaos to keep track of what you changed.

That's why the language must be declarative, to give the system
freedom to run formulae whenever it needs to without worrying about
side effects. On VisiCalc, the first spreadsheet software from 1979,
Ted Nelson said:

"VISICALC represented a new idea of a way to use a computer and a new
way of thinking about the world. Where conventional programming was
thought of as a sequence of steps, this new thing was no longer
sequential in effect: When you made a change in one place, all other
things changed instantly and automatically."

## That's where it ends

However, it’s quite a limited programming language. It was initially
designed to handle a small subset of problems that you might encounter
in finance. It has been extended with plenty of functions like
trigonometric functions and things like that, but without any
particular rigour or academic insight or critical eye.

It’s very convenient for simple problems, dealing with simple numbers,
text and dates. However, any programmer can tell you immediately that
there are limitations to this language. And indeed any Excel or Google
Sheets user can tell you that they have hit the limits of this
language often.

## The solutions aren't solutions

When you hit the limits of the spreadsheets expression language, there
are two approaches:

* The first is to try to remodel your problem to avoid the
  limitation, this is really just a hack/kludge.
* The other is to simply abandon this language and use either Visual
  Basic or JavaScript, or Python, or some other general purpose
  programming language that has the power to express what you really
  wanted to express in the first place. This is typically called
  "scripting".

The first approach is not a solution.

For the second, there are two solid problems with this approach:

* The first problem is that you are no longer using the original
  language and therefore you have to keep two languages in sync and in
  your head.
* You have to work with two completely different programming
  paradigms, because all of the mainstream scripting languages are
  imperative.

You have lost the declarative nature of what makes spreadsheets
great. You also have to learn a new language.

If you were a normal spreadsheet user, with a full plate of work, the
chances of actually learning Visual Basic or Python on your own time
are very slim.

So, probably, you’ll have to ask a "programmer" to solve your problem
for you, which is really annoying. A wise person once said: in
computing, there is nothing worse than a computer telling you that you
cannot express a thought.

## Wizard politics

This creates a funny class system of "muggles and wizards", to borrow
a Harry Potter term, wherein the millions of users of spreadsheets are
the muggles that make do with rudimentary tools, and the wizards are a
privileged class with all the power. Modern offerings like AirTable
continue this narrative: in the community forums, I have read this
comment by a community leader:

> I don’t recommend attempting this using a formula field. Look into a
> scripting solution—either in the Scripting app, or in a “Run script”
> action in an automation—where you can tap into the built-in sorting
> features of the JavaScript language.

It also makes bad sense from a business perspective; I paid for a tool
for all my employees, and now my employees are asking for developers
to do something that the tool should be able to do already. My
employees are wasted even though they are perfectly good at their
domain and are willing.

We are also faced with a secondary problem, which is the problem of
choice. You have to choose where to put your logic, either in the
spreadsheet or in the scripting language. Now you have two problems.

## Let's call a spade a spade

The elephant in the room is simply that the expression language in
spreadsheets is insufficient, not up to the task, not up to snuff,
including for people who aren’t programmers or
engineers. Hybridisation does not work well to paper over this issue
either.

This is also omitting other criticisms, like a lack of first-class
functions, which would make awkward abominations like `VLOOKUP` and
friends unnecessary.

We also know that this language simply does not scale. People write
god-awful messes of `IF(IF(..))` expressions that fill a whole screen
in one cell. Formulae are duplicated across ranges and then
accidentally modified only in some of them. It's the wild
west.

## There is already a better language

The good news is that for 35 years there has been in development
so-called _pure functional programming languages_, which are
declarative languages which have the power of general purpose
languages like Visual Basic, yet retain the declarative purity that we
enjoy in spreadsheets.

The most popular incarnation of this is called Haskell. It has a
static type system which prevents some issues. It has a
well-developed, comprehensive set of functions for expressing common
problems like loops, filters, reductions, etc.

Unlike your Visual Basics, your Pythons, your JavaScripts, Haskell
knows how to express normal every day programming problems in a
functional declarative way, and that is what makes it a perfect
candidate replacement language for spreadsheets.

Haskell also has something to say about dealing with time (think:
`=NOW()`), streams (think reading data from external systems), events
(think button clicks) and the rest in a pure language. Spreadsheet
systems side-step the whole issue (and miss a huge opportunity),
opting to simply call these "volatile" cells that may change in a
variety of ad hoc cases, but we'll see more about that in another
future article.

## Conclusion

In conclusion, there’s no point trying to maintain a hybrid approach
of using a very restricted language combined with an imperative
language, when you could go straight to the obvious solution and use a
real, functional, powerful language from the beginning, which has been
tried and tested for 35 years, and is easily up to the task of
expressing spreadsheet problems. There is also an optimising compiler
that can compile it to machine code to run very quickly.

You can educate people in a tool to do simple arithmetic and filters
very easily, I've done it. But the key addition is that you can level
up in the same language to do more complex things. There's a
progression path.

Our Inflex language is built from the ground up based on Haskell (with
lessons from PureScript, Unison and OCaml). It's designed to be run in
a reactive document, to deal with numbers, records, lists, tables,
etc. and in the future, streams. At the time of writing, we're in an
invite-only beta, but we'll be documenting more of that language over
time.
