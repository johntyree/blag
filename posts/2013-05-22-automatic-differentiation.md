---
title: Computing the Greeks with AD
tags: Haskell Finance AD Math
---

# Computing the Greeks with Automatic Differentiation [![][githublogo]][gist]

A lot of effort in computational fields is spent quantifying uncertainty. Of
course we want to know the answer to the question at hand, but just as
crucially, we are often interested in how *trustworthy* that answer is. What
kinds of hidden assumptions was I making when I solved this problem? Do they
affect my answer wildly?

- What if I had run this test during Friday traffic instead of Saturday?
- What if I did it at night?
- What if I let it run for twice as long?

Answers to these questions build our confidence in the robustness and
predictability of our work.

Depending on your field you might call this *sensitivity analysis*, *stability
analysis*, or even just computing a gradient.

### In Finance, we refer to it as *[the Greeks][].*

Given some assumptions about the current state of the market, such as its
volatility or the best interest rate you could theoretically get in a loan, we
use a formula (if we're lucky) or a simulation to determine what the financial
product should cost. This isn't enough, however, and everyone who thought it
was went bankrupt in 2008. In addition to price we also need to know how likely
that price is to change in the future. We need to know the *sensitivity* of the
price with respect to the state of the market.

Traditionally, this is done by taking a model such as the Black-Scholes
equation and solving it to get the price. Differentiating it by any of the
parameters, such as volatility, tenor, interest rate, etc., will give you the
sensitivity of the price to that particular parameter. The problem with this,
of course, is that Black-Scholes is useful, but [wrong][volsmile].

There are more complicated models being proposed all the time that attempt to
correct some number of known-bad assumptions in Black-Scholes. In my thesis, I
worked with the Heston Model, which assumes a stochastic volatility in addition
to a stochastic underlying asset price. The problem now is that it's not at all
obvious how to solve the resulting equations. Typical approaches involve using
simulation of some kind, either [Monte Carlo integration][] or
[finite-differencing][] (this is what I worked on), to get a good
approximation of the true value. It's difficult to figure out how to
differentiate that, though.

All of this rambling is leading up to a topic I personally think is really
exciting. [Automatic Differentiation][] is the idea that you apply the
principles of basic high school calculus to each variable *as you do the
computation*. Edward Kmett has done a fabulous job of explaining it succinctly
in his [`ad`][ad] package, but I'll reproduce a piece here.

> AD employs the fact that any program $y = F(x)$ that computes one or more
> value does so by composing multiple primitive operations. If the (partial)
> derivatives of each of those operations is known, then they can be composed
> to derive the answer for the derivative of the entire program at a point.

So what this means for us is that if we have a way to compute a value using any
number of standard (differentiable) operations, we can also compute the
partial derivatives of the value. So we could use this for all runs in a
Monte Carlo estimation and recover our Greeks without needing to perturb our
inputs. We could do the same for finite differencing as well!

There are implementations of this idea in pretty much every language that
anyone cares about and I find it curious that it isn't more widely used. I
think that once it becomes mainstream as a programming technique, people will
wonder why we were doing anything without it.

I put together a very basic example of how easy it is to use by computed
several partial derivatives of the plain old vanilla Black-Scholes model. If
you want to see it working a little harder, it looks like some benchmark
examples have been added to the [repo][ad] that are probably worth
investigating. Have fun!


<script src="https://gist.github.com/johntyree/3956381.js"></script>



[the greeks]: https://en.wikipedia.org/wiki/Greeks_(finance)
[gist]: https://gist.github.com/johntyree/3956381/
[githublogo]: /images/github-logo_32.png
[volsmile]: https://en.wikipedia.org/wiki/Volatility_smile
[automatic differentiation]: https://en.wikipedia.org/wiki/Automatic_differentiation
[monte carlo integration]: https://en.wikipedia.org/wiki/Monte_Carlo_Integration
[finite-differencing]: https://en.wikipedia.org/wiki/Finite_difference
[ad]: https://github.com/ekmett/ad
