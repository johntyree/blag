---
title: Brnfckr - An optimizing brainfuck toolchain (Part 2)
tags: Hack Haskell Compilers Brainfuck
---

# Brnfckr - An optimizing brainfuck toolchain (Part 2)

Last week I introduced the brainfuck language and my strategy for
parsing it. In this post we'll build an interpreter to evaluate our code. It's
important to do this step first so that we can get started with building a test
suite, which in turn will be invaluable when trying to develop optimizations.

As usual with Haskell, it's best to start by laying out our foundational data
types. Here I'll represent the state of the program as a pointer into current
memory and a stream of bytes for STDIN.

```haskell
data World = W { mem :: Ptr, dataInput :: Input }
data Ptr = Ptr [Word8] Word8 [Word8]
type Input = [Word8]
```
Here the `Ptr` type is a [zipper][zipper], which works a bit like two stacks
and a "selected" element. If we want to increment the pointer, we put our
center byte onto the left stack, then take a byte from the right stack and put
it in the center. We can decrement by simply doing the reverse. Together, these
operations let us move around in an infinite list while remembering our
position. Unfortunately, it's not a very efficient representation, but we'll
stick with it for now because it's so clearly correct.

We'll also use a stream of bytes to represent the output of the program
```haskell
type Output = [Word8]
```

We're ready now to start building up the type that will represent a brainfuck
computation on our little machine. In our case, we don't need to be able to do
general IO, just write bytes to STDOUT as we encounter `.` commands.
Lucky for us, we never have to read anything we've written in the past,
although we could simply copy the bytes to another memory location if we did.
This is almost the definition of the `Writer` monad, and indeed we'll use a
`Writer Output` instead of `IO ()` as our base monad.

We do need more than just writing out bytes though, we'll have to track the
state of our memory as well as how much input has been consumed as we go along.
This implies the `State` monad, but since we need to combine it with the
`Writer` mentioned above, we'll be using `StateT` instead.

Finally, we want to be able to capture any failure modes we might encounter,
such as an invalid program or insufficient input.

```haskell
data BrainFuckError = UnbalancedBracket String
                    | ParseFailure String
                    | InsufficientInput
  deriving (Show, Eq)
```
 We'll be using the `ExceptT` transformer for error handling.
 
 Building up a monad transformer stack that pieces together the little bits of
 functionality you need (and nothing else!) is a very common way to structure
 larger Haskell programs. Ours has turned out pretty nicely.

```haskell
type BrainFuck a = ExceptT BrainFuckError (StateT World (Writer Output)) a
```
We'll need a function for "running" our computation, which unwraps each layer
of our stack and passes along the result.
```haskell
run :: World -> [Term] -> ((Either BrainFuckError (), World), String)
run world = runWriter . flip runStateT world . runExceptT
```
OK whew. Now we have something to build on.

## Sketching Out Operations

With our stack established, we can start thinking about the operations we want
to support. With brainfuck it's fairly obvious, all our operations are laid out
pretty explicitly in the language definition.

First, we need to be able to read and write to our current memory address, as
many other operations will make use of this. We know the return type of these
functions should be `BrainFuck a` as well, so we can start writing out type
signatures to be filled in later.

```haskell
getMem :: BrainFuck Ptr
setMem :: Ptr -> BrainFuck ()
```
Continuing with this trend, lets identify some more operations on our
machine. We want to be able to move our pointer left and right, increment or
decrement the current memory address, and do IO by reading from STDIN and
writing to STDOUT.
```haskell
ptrIncr, ptrDecr :: Int -> BrainFuck ()
valIncr, valDecr :: Word8 -> BrainFuck ()
valInput, valOutput :: BrainFuck ()
```
The only "complicated" feature we need to support is looping, which is just a
function from a collection of `Term` values to a `BrainFuck ()`.

```haskell
runLoop :: [Term] -> BrainFuck ()
```

Finally, we need a function to perform each of these as appropriate, based on
the `Term` values encountered. For this one, the implementation follows
directly from the function names.

```haskell
eval :: [Term] -> BrainFuck ()
eval = mapM_ f
  where
      f (ValIncr i) = valIncr i
      f (ValDecr i) = valDecr i
      f (PtrIncr i) = ptrIncr i
      f (PtrDecr i) = ptrDecr i
      f (Loop e) = runLoop e
      f ValInput = valInput
      f ValOutput = valOutput
```
The implementations of each of the other operations is good practice for anyone
following along at home so I'll leave them off here. If you'd like to sneak a
glance at how I've implemented them, you can check out the code
[on GitHub][repo].

And that's it! Now we can run brainfuck code using our own little interpreter.

In the next post I'll talk about optimization passes and some ideas for
speeding it up.

[zipper]: FIXME
[repo]: https://github.com/johntyree/brnfckr/blob/tree/FIXME
