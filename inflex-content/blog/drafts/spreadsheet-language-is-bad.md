Article {
 title = "The spreadsheet language is bad",
 date = 2020-11-13,
 content = ""
}

The user interface in a spreadsheet is Modal.  There are two modes. The first mode is code. The second mode is the result of running that code. You type in some equation, some mathematics, some conditions, and then you hit return, and then you see the results.

The programming language that you write your code in is a declarative probably language. We call programming language is declarative when you state what you want in a more mathematical way, instead of stating imperative steps: do this, do that. Instead you write for example some of some range, and so on. The code in spreadsheets is this kind of language. And it is a real programming language.  What are examples of declarative programming are structured query language or SQL, and that’s pretty much it.

However, it’s quite a limited programming language. It was initially designed to handle a small subset of problems that you might encounter in finance. It has been extended with plenty of functions like calculating the present value and things like that, but without any particular rigor or academic Pedegree or critical eye. It’s very convenient for simple problems in accurate system, dealing with simple numbers and Rings and dates. However, any programmer can tell you immediately that there are limitations to this language. And indeed any XL or Google spreadsheets of her can tell you that they have hit the limits of this language often.

When you hit the limits of the spreadsheets expression language, there are two approaches One: the first is to try to remodel your problem to avoid the limitation, this is also called a hawk or a clutch. The other is to simply abandon this language and use either a visual basic or JavaScript or python or some other general purpose pregnant language which has the power to express what you really wanted to express in the first place. This is called scripting.

There are two solid problems with this approach. The first problem is that you are no longer using the original language and therefore you have to keep two languages in sync and in your head. You have to work with two different programming models. Because all of the popular scripture languages are imperative. You have lost the declarative nature of what makes spreadsheets great. You also have to learn a new language. If you were a normal person the chances of actually learning visual basic or python or some thing else are very slim. So probably you’ll have to ask a programmer to solve your problem for you, which is really annoying. A wise person once said there is nothing worse than a computer telling you that you cannot express a thought.

We were also faced with a secondary problem which is the problem of choice. You have to choose where to put your logic, either in the spreadsheet or in the scripting language.

The elephant in the room is simply that the expression language in spreadsheets is insufficient, not up to the task, even for normal people who aren’t programmers or engineers.

The good news is that for 30 years there has been in development so-called functional programming languages which are declarative languages which have the power of general purpose languages like visual basic, yet retain the declarative purity that we enjoy in spreadsheets.

It’s called Haskell. It has a study type system which prevents correctness please. It has a well developed and comprehensive set of functions for expressing common problems like loops.  For example, insert example here. Unlike a visual basic or python or JavaScript, Haskell knows how to express normal every day program and problems in a functional declarative way, and that is what makes it a perfect candidate replacement language for spreadsheets.

In conclusion, there’s no point trying to maintain a hybrid approach of using a very restricted language combined with an imperative language, when you could go straight to the obvious solution and use a real, functional, powerful language from the beginning, which has been tried and tested for 30 years, and is easily Up to the task of expressing spreadsheet problems. And can also be optimized and compiled to machine code to run very quickly.
