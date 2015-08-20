---
title: C++ mangles symbols
tags: C++ C
---

Anyone who has tried to use C and C++ together is probably aware of the
`extern` keyword. What's up with that? I'll tell you what's up. It turns out
that in order to achieve function overloading without the overhead of a lookup
table, C++ cheats and renames everything.

## For example

Let's take a really simple C file with just one function in it and create an
object file.

```c
#include <stdlib.h>

void *dummy(int a, float b, char *c) {
    return NULL;
}
```
and compile it...

```bash
$ gcc dummylib.c -c
$ ls
dummylib.c dummylib.o
```

OK fabulous. Now we can use our function in other programs we might make. If
you aren't familiar with the `nm` tool, now is a great time to start. According
to its manpage it can "List symbols in [file(s)]." Let's try it out.

```bash
$ nm dummylib.o
0000000000000000 T dummy
$
```

Tada! There's our function, patiently waiting. The `T` there tells us that it's
part of the "text" section of the file, i.e. where the code lives. Let's make a
little program to use it.

```c
#include <stdlib.h>

extern void *dummy(int, float, char *);

int main(int argc, char* argv[]) {
    dummy(0, 0.0, NULL);
    return 0;
}
```
and we build this one...

```bash
$ gcc main.c -c
$ ls
dummylib.c  dummylib.o  main.c  main.o
$ nm main.o
                 U dummy
0000000000000000 T main
$
```
and there's our main function. Here you can also see the `dummy` function but
without an address. We told GCC that `dummy` exists when we did `extern void
*dummy(..)` but we haven't yet told it where. That `U` lets us know that this
symbol is expected eventually, but is currently undefined. When we finally link
these two files together, the definition of `dummy` will slot in there and
everything will work out.

```{.bash .numberLines}
$ gcc main.o dummylib.o -o main
$ nm main
00000000006006f0 d _DYNAMIC
00000000006008c8 d _GLOBAL_OFFSET_TABLE_
0000000000400580 R _IO_stdin_used
                 w _ITM_deregisterTMCloneTable
                 w _ITM_registerTMCloneTable
                 w _Jv_RegisterClasses
00000000004006d0 r __FRAME_END__
00000000006006e8 d __JCR_END__
00000000006006e8 d __JCR_LIST__
0000000000600900 D __TMC_END__
0000000000600900 B __bss_start
00000000006008f0 D __data_start
0000000000400470 t __do_global_dtors_aux
00000000006006e0 t __do_global_dtors_aux_fini_array_entry
00000000006008f8 D __dso_handle
00000000006006d8 t __frame_dummy_init_array_entry
                 w __gmon_start__
00000000006006e0 t __init_array_end
00000000006006d8 t __init_array_start
0000000000400570 T __libc_csu_fini
0000000000400500 T __libc_csu_init
                 U __libc_start_main@@GLIBC_2.2.5
0000000000600900 D _edata
0000000000600908 B _end
0000000000400574 T _fini
0000000000400370 T _init
00000000004003c0 T _start
0000000000600900 b completed.6938
00000000006008f0 W data_start
00000000004003f0 t deregister_tm_clones
00000000004004df T dummy
0000000000400490 t frame_dummy
00000000004004b6 T main
0000000000400430 t register_tm_clones
```
Yikes! Look at all that crap. That's the C runtime that GCC automatically links
for us so that our program will actually run. If you ignore that and check out
line 33 you can see our little dummy function, ready to go.


### So we're done right?

Wrong. Let's try that same thing with C++. Just swap out `g++` for `gcc` and
see what you get.

```bash
$ g++ dummylib.c -c
$ nm dummylib.o
0000000000000000 T _Z5dummyifPc
```

<center>
![](/images/squinty.jpg)
</center>

So what is that mess? That's C++'s support for overloading. The name `dummy`
has been "mangled" to produce a unique identifier based on the types
of its arguments and its return value. This seems pretty clever, but what
happens to our poor `main` object now when we try to link them together?

```bash
$ g++ main.o dummylib.o -o main
main.o: In function `main':
main.c:(.text+0x1e): undefined reference to `dummy'
collect2: error: ld returned 1 exit status
```
It fails of course! It's looking for `dummy` which clearly isn't there. So now
what? We turn off name-mangling, that's what.

```cpp
#include <stdlib.h>

extern "C" {
    void *dummy(int a, float b, char *c) {
        return NULL;
    }
}
```
That `extern` tells `g++` to use the [C ABI][ABI] for the functions in the block. This
prevents their names from being mangled so that programs expecting C functions
will be able to find them.

```bash
$ g++ dummylib.cpp -c
$ nm dummylib.o
0000000000000000 T dummy
```
Much better. Now we're free to link against it again.
```bash
$ g++ dummylib.o main.o -o main
$
```

Of course, this prevents overloading function names by type, but if you want to
use your C++ code from outside of C++ then you have no choice. A common
approach is create a wrapper library that simply reexports functions as `extern
"C"`.

Sometimes, though, you have legit C++ code you need to use as-is.  I had to
deal with this while writing CUDA code for my MSc Thesis. In an another post,
I'll talk about using a C++ library from Python and dealing with mangled names
in error messages.

[ABI]: https://en.wikipedia.org/wiki/Application_binary_interface
