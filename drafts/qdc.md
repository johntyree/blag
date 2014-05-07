---
title: Quick and Dirty C/C++
tags: C C++ Hack
---

I am tired of C. I am tired of C++. I am tired of sacrificing goats to the gods
of Google and Stack Overflow every time I have something *trivial* that needs
to be embedded in my application. Enter,
[Quick-and-Dirty-C](https://github.com/johntyree/QDC). It's a
[growing](2013-01-30-cuda-gcc-47.html) collection of tricks, tweaks, and handy
functions for doing the kinds of trivial things that everyone expects will take
5 minutes to write, but in fact require many hours to get right. In this post
I'll introduce the most recent addition, a small collection of functions for
filtering a buffer of data through another program via `stdin` and `stdout`. So
if you weren't planning on using strlen() for some reason, you might filter
your string through `wc -c`, doing the equivalent of this:

    $ echo "Hello, Monde!" | wc -c.

As it turns out, this is *far* from trivial to get working correctly.


<!-- This is really another post -->
Case in point:

In my thesis work, I have adopted ![Cython](cython.org) to bridge the gap
between an easy and fun library for prototyping Finite Difference problems and
a messy reimplementation of some key parts in C++ /
[Cuda](http://www.nvidia.com/object/cuda_home_new.html). Part of what makes
this such a great solution is the ability to play with my C++ code in an
interpreted environment with a REPL. In particular, it allows me to leverage
Python's excellent exception handling to help with debugging.

Unfortunately, Cython can only throw a python exception from inside a python
function, and even then, it doesn't keep the backtrace that is so desperately
needed. What it *does* do, however, is use the what() method of thrown
exception to produce its python error message.

    Example?


