---
title: Getting a Demangled Backtrace out of C++
tags: C++ C
---

In a [previous post][cpp-symbols], we looked at how C++ mangles our function
names when it compiles object files, resulting in symbols that are difficult to
read and impossible to link non-C++ programs against. Now we'll talk about how
to deal with object files that have already been built or which *must* be built
without using `extern "C"`.

We can't alter the symbol in the file, but the `binutils` package on most Linux
and Unix distributions comes with a tool to help us out. The `c++filt` utility
takes a mangled name as an argument and returns the proper C-style signature.

It's seriously great. Check it out!

```bash
$ nm dummylib.o
0000000000000000 T _Z5dummyifPc
$ c++filt _Z5dummyifPc
dummy(int, float, char*)
```
And that's it!

It's super easy to use on the command line when you're trying to understand a
particular problem, but what about from inside a program? My use case was a
Python wrapping of the [Thrust][thrust] library for CUDA, which provides an API
similar to C++'s `<algorithms>`. Thanks to the magic of [Cython][cython] the
actual wrappers weren't very difficult. The fun part was dealing with exception
handling.

I wanted exceptions in the C++ layer to be caught be Cython and marshalled into
Python-space as Python exceptions. Cython can handle this for us, but it
doesn't give us much information about the content of the exception and it
*definitely* doesn't fix any text associated with the exception. For that we'll
have to roll our own system.

### Python is supposed to be great for this, right!?

In grad school, you have lots of time. So much time in fact, that you can hatch
crazy ideas like

> *Hey I know how to make this easier! Let's use Cython to wrap some C++ that
> calls into the Thrust template library which then runs code on the GPU! How
> hard can it be!?*

and still expect to finish your project on time. I miss it.

In my thesis work, I adopted [Cython][cython] to bridge the gap
between an easy and fun library for prototyping Finite Difference problems
(that's the Python half) and
a messy reimplementation of some key parts in C++ /
[Cuda][cuda] (that's what I was actually supposed to be doing). Part of what
made it such a neat toy was the ability to play with my C++ code in an
interpreted environment with a REPL. In particular, it allowed me to leverage
Python's excellent exception handling to help with debugging.

Unfortunately, Cython can only throw a Python exception from inside a Python
function, and even then it doesn't keep the backtrace that is so desperately
needed. With no backtrace, all you know is that your C++ code has died. When
your stack was as rickety as mine was (Python -> Cython -> C++ -> Thrust ->
Cuda) that really doesn't narrow it down very much. What Cython *does* do,
however, is use the `.what()` method of thrown exception to produce its Python
error message. This was my sneaky way in.

My plan was to deal with exceptions in C++, grab a backtrace myself using
good old fashioned C code, translate the mangled C++ names back to
readable C-style names, then re-throw the exception from C++ with the
now-readable backtrace attached. This is then picked up by Cython via `.what()`
and brought into Python space.

The end result was pretty sexy and I was indeed able to realize my dream of
playing with boundary conditions for options pricing problems on the GPU from
the Python interpreter, *and* get real C++-aware backtraces when something went
wrong. I probably spent an entire two days setting this up, but the extra help
it gives when debugging paid for itself by the end of week.


### Piping output through a program from C

In pure Python, none of this would really be a problem, or even worth a blog
post. `subprocess.Popen` lets us call subprocesses and handle both their input
and output in a few short lines. As usual, C makes life is much harder.

If you've never looked at how fork/exec works in C, brace yourself, it's
@#$#ing complicated.

So when we fork, we actually duplicate the current process entirely, file
handles and all. In order to actually run something else, we need to detect
that we are the newly forked process and call exec(), which wipes out our
brain and replaces it with the new program to be run. In order for that to
be useful, we *also* need to be able to control STDIN and STDOUT so we can feed
data to our new process and read the output back in the original process.

I've started a little git repository for these kinds of ugly hacks. It's called
[QDC][qdc]. If you're interested in how it all comes together, take a look. A
quick test file is shown below. The real magic is in [`filter.h`][filter]


```{.c .numberLines startFrom="0"}
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#include "backtrace.h"
#include "filter.h"


int example() {
    char *res = (char * const)malloc(1024);
    char const *in = "\n\nSome input.\n";

                          // path,     pname, args...,    NULL
    char * const argv[] = {"/bin/cat", "cat", "-A", "-v", NULL};
    int argc = 5;

    int size = filter(&res, 1024, in, strlen(in), argc, argv);
    if (size == -1) {
        perror("Pipe failed");
        exit(EXIT_FAILURE);
    }
    printf("Wrote %i bytes.\n", size);
    fwrite(res, size, 1, stdout);
    puts("Safe with puts because size != sizeof(res).");
    puts(res);
    free(res);
    return 0;
}

int cppfilt(size_t size) {
    /* Sample backtrace for use with c++filt */
    char *output = (char *)malloc(size);
    // char *output = (char *)malloc(size);
    char * const text = "\
BandedOperatorGPU.so(_Z9backtracePc+0x26) [0x2b097cd2adca]\n\
BandedOperatorGPU.so(_ZN10SizedArrayIdE12sanity_checkEv+0x730) [0x2b097cd2d544]\n\
BandedOperatorGPU.so(_ZN10SizedArrayIdEC1ElSs+0x65) [0x2b097cd3a879]\n\
BandedOperatorGPU.so(_ZN18_CSRBandedOperator5applyER10SizedArrayIdE+0x3fc) [0x2b097cd34570]\n\
BandedOperatorGPU.so(+0x7a550) [0x2b097ccb2550]\n\
BandedOperatorGPU.so(+0x7c367) [0x2b097ccb4367]";
    char * const argv[] = {"/usr/bin/c++filt", "c++filt", NULL};
    char *input = text;
    int argc = 3;
    assert(output);
    assert(input);
    size = demangle(output, input, size);
    printf("Wrote %lu bytes.\n", size);
    fwrite(output, size, 1, stdout);
    puts("\n");
    free(output);
    return 0;
}

int main() {
    cppfilt(2048);
    return 0;
}
```
You can see the big ugly backtrace in there on lines 36-41. But when we run
this beauty...

```bash
$ ./test 
Wrote 482 bytes.
BandedOperatorGPU.so(backtrace(char*)+0x26) [0x2b097cd2adca]
BandedOperatorGPU.so(SizedArray<double>::sanity_check()+0x730) [0x2b097cd2d544]
BandedOperatorGPU.so(SizedArray<double>::SizedArray(long, std::basic_string<char, std::char_traits<char>, std::allocator<char> >)+0x65) [0x2b097cd3a879]
BandedOperatorGPU.so(_CSRBandedOperator::apply(SizedArray<double>&)+0x3fc) [0x2b097cd34570]
BandedOperatorGPU.so(+0x7a550) [0x2b097ccb2550]
BandedOperatorGPU.so(+0x7c367) [0x2b097ccb4367]
```
Ahhhhh... now we're ready to pass this up to Python.


[cpp-symbols]: 2013-03-12-cpp_symbols.html
[thrust]: http://
[cython]: http://cython.org
[cuda]: http://www.nvidia.com/object/cuda_home_new.html
[qdc]: https://github.com/johntyree/QDC
[filter]: https://github.com/johntyree/QDC/blob/master/filter.h
