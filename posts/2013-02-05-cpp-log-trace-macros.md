---
title: C++ LOG and TRACE: macros > printf.
tags: C++ Hack
---

This is a short one.

GDB is a usability nightmare, but when we want to trace function calls and
track variables what else can we do? Most of us turn to `printf` or
`std::cout`. This can be effective, but since most debug messages are something
like `printf("WTF!? %i", i)` it quickly turns into a horrible mess. Of course,
writing better messages as a means of handling code complexity is right out the
window. If we weren't so lazy, we'd be using a real debugger, right?

There's a (slightly) better way to keep things under control: good ol'
fashioned preprocessor magic. In particular, the magic words `__func__`,
`__LINE__`, and `__FILE__`, which I have only recently come to appreciate. If
you're devloping with GCC (and why shouldn't you be?), you also have
`__PRETTY_FUNC__`, which is as nice as it sounds. Unlike `__func__`, it
includes the function signature.  [The
documentation](http://gcc.gnu.org/onlinedocs/gcc/Function-Names.html "Function
Names - Using the GNU Compiler Collection (GCC)") is clear and provides all the
boring implications you could hope for. Let's just jump to the good parts.

For logging, I use a `stringstream` to allow for a lisp-ish "code is data" way
to pass a message into a function. The result ends up being slightly magical,
but super lightweight and immensely satisfying. You can see that it even works
correctly from headers.

Without further ado...

<script src="https://gist.github.com/johntyree/4718393.js"></script>
