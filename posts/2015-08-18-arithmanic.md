---
title: Mental math
tags: Math Python
---

# Arithmanic [![](/images/github-logo_32.png)][repo]

Quick, what's 345 / 223?

Who knows? I don't. Mental math is hard. Personally, I'm pretty terrible at it.

It's useful though and so I decided it was time to improve. My first step was
to buy a little Android app called [Math Workout][math app]. It's a simple app,
but it's also a joy to use. Kudos to them and I hope they are making money off
of it.

<center>
![Math Workout][img math app]
</center>

That's all well and good if you are on your phone, but often I am not, and
besides, it seems like a fun tool to try to make ourselves in Python! Let's
get started.

I've been playing with a lot of [Haskell](http://haskell.org) lately, so lets
start by figuring out the types our program will have.

We'll need an object for representing a challenge to the player somewhere to
record the player's answer. I'm choosing to use a `namedtuple` for this. Each
result will be a binary operator with left and right operands, the correct
answer, and whether or not the player guessed it.

```python
from collections import namedtuple
Result = namedtuple('Result', 'ok op left right value'.split())
```
We'll use this to track our performance over time and potentially do some smart
things to help us focus on problem areas.

Next let's figure out how we're going to ask a question. I want the operands
and operator to be randomized, so I'll need to define the collection of
valid operators up front, and write a function for generating appropriate
operand values.

```python
from operator import add, mul, sub, truediv
operators = {'+': add, '-': sub, 'x': mul, '/': truediv}
```
The `operator` module is obviously very hand here as it gives us normal Python
function versions of the standard arithmetic operations `+ - * /`.

```python
def choose_operands(left_range, right_range):
    """Randomly choose operands less than the values specified."""
    left = random.randint(*left_range)
    right = random.randint(*right_range)
    return (left, right)
```

So now we have a way to choose individually bounded operands (to divide large
numbers by small numbers, for example) but we still need to present the
challenge to the user somehow. I've elected to represent the question and
answer as a string and a value, for easy printing and validation of the
response.

```python
def make_q_and_a(op, left, right, digits=1):
    """ Return a string representation of a question. """
    q = "{:{digits}} {} {:{digits}}".format(left, op, right, digits=digits)
    if op == '/' and right == 0:
        a = float('inf')
    else:
        a = operators[op](left, right)
    return q, a
```
Note that we have to be careful of division by zero! I could have taken care to
exclude this case entirely, but for simplicity we'll just pretend to be taking
the limit and call it `inf`.

All we're missing now is the main loop that ties it all together. For a first
try, let's just ask our question and report whether the player is correct.

```python
def play():
    range1, range2 = (0, 10), (0, 10)
    left, right = choose_operands(range1, range2)
    digits = max(len(str(r)) for r in range1 + range2)
    q, a = make_q_and_a(op, left, right, digits=digits)
    resp = read("{} = ".format(q))
    ok = resp == a
    return Result(ok=ok, op=op, left=left, right=right, value=a)
```
And we're ready to play! I have a more robust version of this little program on
my [GitHub][repo] page. I've called it
"Arithmanic" because it will introduce time trials soon :). Check it out. It's
pretty fun.

```python
$ arithmanic
10 /  7 = .241
!!! NO NO NO NO NO !!!
10 /  7 = 1.4323
!!! NO NO NO NO NO !!!
    1.4285714285714286
 1 x  9 = 9
11 x 12 = 131
!!! NO NO NO NO NO !!!
11 x 12 = 132
 1 -  9 = -8
 4 /  7 = .6342
!!! NO NO NO NO NO !!!
 4 /  7 = .5735
!!! NO NO NO NO NO !!!
    0.5714285714285714
 0 / 12 = 0
 3 -  9 = -6
 2 x 10 = 20
 1 +  0 = 1
 7 - 12 = -5
 6 -  9 =
[Result(ok=False, op='/', left=10, right=7, value=1.4285714285714286),
 Result(ok=False, op='/', left=10, right=7, value=1.4285714285714286),
 Result(ok=True, op='x', left=1, right=9, value=9),
 Result(ok=False, op='x', left=11, right=12, value=132),
 Result(ok=True, op='x', left=11, right=12, value=132),
 Result(ok=True, op='-', left=1, right=9, value=-8),
 Result(ok=False, op='/', left=4, right=7, value=0.5714285714285714),
 Result(ok=False, op='/', left=4, right=7, value=0.5714285714285714),
 Result(ok=True, op='/', left=0, right=12, value=0.0),
 Result(ok=True, op='-', left=3, right=9, value=-6),
 Result(ok=True, op='x', left=2, right=10, value=20),
 Result(ok=True, op='+', left=1, right=0, value=1),
 Result(ok=True, op='-', left=7, right=12, value=-5)]
```

[math app]: http://www.mathsworkout.net
[img math app]: /images/mathsworkout.png
[repo]: https://github.com/johntyree/arithmanic
