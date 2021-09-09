# Inflex introduction

Here's a brief introduction to Inflex syntax. We'll be writing a much
more detailed set of documentation soon, including interactive
examples in the page. But right now, hopefully the below is enough to
get you started.

**Table of Contents**

- [Arithmetic](#arithmetic)
- [Ordering and comparison](#ordering-and-comparison)
- [Records](#records)
- [Variants](#variants)
- [Text](#text)
- [Functions](#functions)
- [Function calls](#function-calls)
- [Available functions](#available-functions)
- [If](#if)
- [Cases](#cases)
- [Tables](#tables)

<!-- markdown-toc end -->


## Arithmetic

`*`, `/`, `+`, `-` are supported. Try `2 * 3 + 5.0` in the app.

You can use `* 1.00000` to get e.g. 5 decimal places of precision.

## Ordering and comparison

`<`, `<=`, `>=` for ordering. Try `4 = 5` or `42 * 23 < 53 * 24` in the app.

`=` and `/=` for comparison.

Currently these work on numbers and text, but not lists or records (yet).

## Records

You can put records together as:

`{foo: 1, "bar": 123}`

access:

`myrecord.name`

Try calling one cell `person` with `{name: "Charlie"}` in it. Then in
another cell, you can write `person.name`.

## Variants

You can refer to choices with hashtag syntax, like:

`#true`/`#false`

`#red`/`#black`/whatever

`#ok(123)`/`#none` - this is used by CSV import for missing fields.

It's fine to make a list like `[#red, #black, #red, #blue]`.

See `case` section.

## Text

Put your text in speech marks:

`"foo"`

But the Inflex interface will display a text editor, so you can just
edit it like that after hitting enter.

## Functions

Simply write,

```haskell
x: x * x
```

And this means "a function of input `x` with output `x * x`". Done!

See below for a good example.

## Function calls

Familiar like in spreadsheets:

```haskell
map(x:x*x,[1,2,3,4])
```

As convenience, for lists or records, you can omit the parens:

```haskell
sum [1,2,3]
foo {some:"123",thing:567}
```

Which can be handy especially for named arguments.

A nicer way to call functions is using dot syntax:

```haskell
[1,2,3].filter(x:x>2).sum()
```

Any built in function or cell with a function in it can be called in this way.

## Available functions

There is a limited list so far:

<table><tr><th>Name</th><th>Example</th></tr>
<tr><td>map</td><td><code>map(x: x * 2, [1,2,3])</code></td></tr>
<tr><td>filter</td><td><code>filter(x: x > 2, [1,2,3])</code></td></tr>
<tr><td>sum</td><td><code>sum([1,2,3,4])</code></td></tr>
<tr><td>average</td><td><code>average([1,2,3,4])</code></td></tr>
<tr><td>vega</td><td><code>vega({...})</code> (see <a href="https://vega.github.io/vega-lite/">vega</a>)</td></tr>
<tr><td>null</td><td><code>null([])</code></td></tr>
<tr><td>length</td><td><code>length([1,2,3])</code></td></tr>
<tr><td>distinct</td><td><code>distinct([2,3,3,4,3,2])</code></td></tr>
<tr><td>minimum</td><td><code>minimum([1,2,3])</code></td></tr>
<tr><td>maximum</td><td><code>maximum([1,2,3])</code></td></tr>
<tr><td>sort</td><td><code>sort([4,3,2,4,2,1])</code></td></tr>
<tr><td>find</td><td><code>find(x:x>5,[2,5,8,2,1])</code></td></tr>
<tr><td>all</td><td><code>all(x:x /= 0,[1,2,3,4])</code></td></tr>
<tr><td>any</td><td><code>any(x:x=0,[1,2,3,0,4])</code></td></tr>
<tr><td>from_ok</td><td><code>from_ok(0,#ok(1))</code></td></tr>
</table>

## If

```haskell
if #true then "ok" else "boo!"

if 3 > 2 then "Yep" else "Maths is broken!"
```

## Cases

Cases currently only work on variants (`#foo`).

```haskell
case #true { #true: "true", #false: "false" }
```

Wildcards (`x` below):

```haskell
case #true { #true: "true", x: "false" }
```

Arguments to variants:

```haskell
case sum([1,2,3,4]) { #ok(n): n, x: 0 }
```

## Tables

Tables are just lists of records. In the UI, you see:

<div class="cell-wrapper"><div class="cell"><div class="cell-header"><div class="cell-name " title="Click to edit cell's name">(unnamed)</div><button class="delete-cell" title="Delete this cell">×</button></div><div class="cell-body"><div class="editor-boundary-wrap"><div class="ellipsis-button" title="Edit this as code"></div><table class="table"><thead class="table-header"><th class="table-column" title=""></th><th class="table-column" title="Click to edit"><div class="table-column-content"><div class="cell-name " title="Click to edit column name">name</div><button class="remove-column-button">×</button></div></th><th class="table-column" title="Click to edit"><div class="table-column-content"><div class="cell-name " title="Click to edit column name">age</div><button class="remove-column-button">×</button></div></th><th class="add-column"><button class="add-column-button" title="Add column to this table">+</button></th></thead><tbody class="table-body"><tr><td class="row-number"><div class="row-number-div"><div class="row-number-text">1</div><button class="remove-row-button">×</button></div></td><td class="table-datum-value"><div class="editor-boundary-wrap" title=""><div class="ellipsis-button" title="Edit this as code"></div><div class="text"><div class="cell-name " title="Click to edit text">Terry</div></div></div></td><td class="table-datum-value"><div class="editor-boundary-wrap clickable-to-edit" title="Click to edit"><div class="misc">52</div></div></td><td class="add-column-blank"></td></tr><tr><td class="row-number"><div class="row-number-div"><div class="row-number-text">2</div><button class="remove-row-button">×</button></div></td><td class="table-datum-value" colspan="1"><div class="editor-boundary-wrap" title=""><div class="ellipsis-button" title="Edit this as code"></div><div class="text"><div class="cell-name " title="Click to edit text">Billie</div></div></div></td><td class="table-datum-value"><div class="editor-boundary-wrap clickable-to-edit" title="Click to edit"><div class="misc">42</div></div></td><td class="add-column-blank"></td></tr><tr><td class="add-row"><button class="add-row-button " title="Add row">+</button></td><td class="bottom-blank" colspan="3"></td></tr></tbody></table></div></div></div></div>

Underneath, this is just:

```haskell
[{name:"Terry",age:52},{name:"Billie",age:42}]
```

So you can use `map`, `filter`, `sum`, etc. on them.
