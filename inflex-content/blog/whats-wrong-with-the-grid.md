Article {
 title = "What’s wrong with the grid?",
 date = 2020-11-12,
 content = ""
}

The grid system in spreadsheets is very simple. It’s also very
powerful, and it’s powerful because it’s simple. As a user you can
immediately start entering data without even thinking about it. And
then, in an ad hoc fashion, you can add logic and conditions as you
think of them.

## Lack of boundaries cause bugs

This fast and loose paradigm is also the downfall of spreadsheets,
because the user is encouraged to do a form of programming, without
encouraging them to do any kind of discipline. Spreadsheets quickly
get out of hand. The boundaries are arbitrary, and usually are only
visual, which means it’s very difficult to write good code based on
this.

For example, there is the famous case of a large spreadsheet used in
an economics paper, called
[Growth in a time of Debt](https://en.wikipedia.org/wiki/Growth_in_a_Time_of_Debt#Methodological_flaws),
and there was a miscalculation because a few rows at the end of a list
of items were not taken into account, as, per usual in a spreadsheet,
the user specified them in a range. It has been criticised anywhere
from a gross oversight to disastrous.

## Proper data structures are simpler

However, in normal programming, we don't have just one data
structure. The grid is just one data structure, a matrix of
practically infinite width and height. You can access its elements by
coordinates. But in programming––normal programming––we have, for
example, lists. If a proper list data structure were were used for the
list of elements in the above example, there wouldn’t have been this
bug, because you don’t have to specify how many elements of a list you
want to work on. We just say "apply this function to all elements of
the list."

Data structures like lists and matrices can all be approximated in a
spreadsheet by a grid, badly. However, anything more than this becomes
awkward very quickly.

For example, a simple record consisting of a name and an age in a
spreadsheet must be modelled by using a cell for the label of the name
and label of the age, and then the value for each thing. This is a
hack, using superficial visual embellishments to reflect what is
actually data.

In normal programming languages, this is called a record. There’s no
need for any kind of strange and brittle hacking and tricks to make
this work as you have to do in a spreadsheet. The basic spreadsheet,
as-is, is very accessible to the normal person who just wants to do
every day calculations about their business. But the ideas of a
record, list, tree or graph are also intuitive for normal people
because they actually model the kind of data that their domain is
dealing with. Sadly, knowledge workers using spreadsheets are deprived
of these tools.

## Data modelling is poor

Therefore we find spreadsheets very lacking, in fact, when we try to
model even every day pedestrian problems.  Consider for example trying
to model a family tree, or the hierarchical structure of the US
government, or a taxonomy of a species.

Trees are three data structures, perfectly normal, well understood,
pliable, things that you can express in a programming language and
manipulate using code in a very logical manner. You can count how many
items are in the tree, you can restrict the tree to a certain branch,
perform a transformation on each node in the tree, or transpose the
tree and flip it upside down!

Another example is a graph. For example, the Coronavirus can be
modelled by its spread using a graph the graph consists of a set of
people in the connections between those people. You can easily write
code to count how many connections a given node has to it; that's a
normal graph operation. Or count how many transitive node are
connected to it in one direction, in other words; you can measure the
influences of one person within a community.

Aside from using code to manipulate said data structures, your user
interface should allow you to click a node and edit the text, maybe
write some code in there, like any other type of cell.

Both of those things are impossible, practically, to describe, talk
about, express, manipulate, or visualise, in a traditional grid-based
spreadsheet!

So, we’ve established two things so far. The first is that the grid
system lacks boundaries between different data structures, which can
lead to bugs. Second is that there are actually very normal, every
day, useful data structures which we should, in a reasonable system,
be able to express, but which the grid system is unable to express.

Additionally, we have seen that these data structures actually have
very well understood and rich and useful operations which work only if
you have that kind of data structure.

But a spreadsheet simply has one single data structure: the grid. And,
the only elements that you can put in that data structure are numbers,
strings and dates. In other words, atomic values. But that’s not real
life. There’s no reason that I should not be able to have a list of
lists. Or a tree of records. Or a list of records (aka a "table"). I
should, hypothetically, be able to arbitrarily nest data structures as
it fits my problem.

## Correctness checks

Turning to a different topic, when you have different data structures,
you can start to add checks that help you avoid making other types of
mistakes. If your system knows the shape of data, it can have
expectations about how you use it.

You can add a type system to the system. The purpose of a type system
is to make sure that you are combining your different operations
together in a way that makes sense. Not putting a square peg in around
hole. For example, you can calculate the sum of an list, of a graph,
of a tree. However, you cannot concatenate a graph to an list. What
would that even mean?

## Conclusion

In conclusion, we’ve looked at reasons why a traditional spreadsheet
grid system is lacking in several key areas of expression,
correctness, and even convenience, in a way that is actually important
for normal knowledge work and not just for programmers.

If you look through various competitors to the established spreadsheet
vendors, you'll see a lot of "reinvent spreadsheets" language. But
it's always the same thing: we have a grid system with coordinates and
an underwhelming untyped expression language. But they added
JavaScript or Python to it, or added a "low-code" app generator on top
of the spreadsheet, or some special views.

We're working on [a system called Inflex](https://inflex.io/) that
really does rethink the spreadsheet fundamentally from the ground
up. And the first thing to be thrown out is the grid, the coordinate
system and replace them with real data structures and a type system.

We keep what's *good* about spreadsheets: the reactivity and
"edit-in-place" user experience.

In our next post we'll be discussing the fundamental problems with the
expression/formula languages used in spreadsheet software, and how we
are fixing it.
