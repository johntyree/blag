---
title: Brnfckr - An optimizing brainfuck toolchain (Part 3)
tags: Hack Haskell Compilers Brainfuck
---
#Brnfckr - An optimizing brainfuck toolchain (Pt 3) [![](/images/github-logo_32.png)][repo]

In the last post, we sketched out a little interpreter for our brainfuck `Term`
values. This week I want to talk about some ideas for optimizations we can do
on those values to speed things up.

This isn't really about optimizing the *interpreter* itself, just coming up
with a better representation of the operations so that all interpreters will
show improvements. To rephrase, we're optimizing the program code, not the
computer.

## Optimization

The first thing to notice is that brainfuck code is very repetitive. In fact,
all operations are expressed in unary! Imagine if we had to express numbers in
real life using the notches-on-the-wall method. We'd never get anywhere.

Consider the following snippet.
```scheme
+++++
```

It's obvious what this should do, We aren't moving around our touching STDIN
and STDOUT, we're just incrementing the value in the current address by 5. This
leads to the first optimization idea! Let's represent repeated operations as
single term. This is an example of [run-length encoding][rle].

Remember previously we represented the increment operation as a parameterized
function.

```haskell
data Term = ValIncr Word8 | ...
```
We can simply increase the byte value associated with our `ValIncr` operation.
To do this we'll have to look for neighboring `ValIncr` values and combine
them. This gives us our first optimization pass!

Since all of our optimizations will take a list of `[Term]` representing the
program and return a new, hopefully better list of `[Term]`, I'll be
representing them as a [`fold`][]. You might know `fold` as Python's `reduce`,
but even if you don't, this should make it clear what's going on.
```haskell
Prelude> foldr1 (+) [1, 2, 3, 4]
10
```
Given a `Term` and the list of `[Term]` so far, we want to
return a new list of better `[Term]`. We'll then just fold up our input
`[Term]` with this function.
```haskell
compress :: [Term] -> [Term]
compress terms = snd $ fix run ([], terms)
  where
      smoosh = foldr go []
      run f (prev, cur) = if cur == prev
                          then (cur, cur)
                          else f (cur, smoosh cur)
      go :: Term -> [Term] -> [Term]
      go (ValIncr j) (ValIncr i:rest) = ValIncr (i+j) : rest
```
I've left the definition of `go` very simple here, but one can imagine many
more clauses to that definition, for example an analogous clause `ValDecr` is
easy to imagine.

## Optimization as a fixed point computation

So what's that stuff at the top of our `compress` function?

```haskell
compress terms = snd $ fix run ([], terms)
```
When we're applying our optimization passes, it isn't clear how many times
we'll need go over the input. In the naive case, you might imagine that in each
pass over the term list that we will combine two `ValIncr` terms, requiring
`n-1` applications of `compress`. With many other optimizations implemented,
this becomes very difficult to track.

Instead, let's just think about how we know we're done optimizing. If we can
apply all of the optimizations that we have to the code and get the same
`[Term]` back that we started with, we must be done. It turns out this idea of
applying a function repeatedly until the value stabilizes is common in the
study of dynamical systems. Indeed, such a stable value is considered to be a
[fixed point][] of the function. For our problem, we'd like to find a brainfuck
program that is a fixed point for our optimization function, i.e. a program
which can be optimized no further.

We can define a function that performs such a search very succinctly in
Haskell.
```haskell
fix :: (a -> a) -> a
fix f = f (fix f)
```
This just composes the function with itself forever, but thanks to Haskell's
non-strict evaluation model if the function ever short circuits itself, we will
"break out" of the loop.

Take another look at `run` above.

```haskell
run f (prev, cur) = if cur == prev
                    then (cur, cur)
                    else f (cur, smoosh cur)
```

So if the two tuple elements are equal, we stop and return them, otherwise we
we optimise the right element and apply `f` to the whole thing. This is perfect
for use with `fix`. It will continue to apply the `smoosh` function up until
the output no longer changes. Now we just need `smoosh` to implement a single
optimization pass, which it does by folding our optimizing function over the
`[Term]` of the program.

```haskell
smoosh :: [Term] -> [Term]
smoosh = foldr go []
```

Together, these compute the fixed point of our optimization function and we get
results like this.

```haskell
> compress [ValIncr 1, ValIncr 1, ValIncr 1]
[ValIncr 3]
```
Awesome! To check out some other optimizations I've tried checkout the code at
[GitHub][brnfckr].

[rle]: https://en.wikipedia.org/wiki/Run_length_encoding
[repo]: https://github.com/johntyree/brnfckr
[brnfckr]: https://github.com/johntyree/brnfckr/blob/master/src/Brnfckr/Eval.hs
[`fold`]: https://en.wikipedia.org/wiki/Fold_(higher-order_function)
[fixed point]: https://en.wikipedia.org/wiki/Fixed_point_(mathematics)
