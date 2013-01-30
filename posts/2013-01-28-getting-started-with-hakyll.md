----------------------------------
title: Getting Started with Hakyll
tags: meta Hakyll MathML MathJax
----------------------------------

Having just started with [Hakyll](http://jaspervdj.be/hakyll/), it seems fitting
to make the first post about how to get this damn thing off of the ground.

Fortunately, Hakyll's benevolent creator, Jasper van der Jeugt, has decided to
make "blog site for noobs" the default setup after a new install.

*Unfortunately*, the stylish blogger, having shunned tumblr and WordPress, is
not satisfied with this. There are two major features missing, **math support**
and **syntax highlighting**. Enabling them isn't terribly difficult, but it's
not terribly obvious either. I'm trying to fix that.

Starting from scratch, we'll do both.


Installing Hakyll is as easy as it gets...

```
cabal install -j hakyll
```

... and wait patiently.

Done?

No?

Well everyone knows you should always read instructions fully before you start
a new project anyway so let's keep going.

Pick a home for your new site and run `hakyll-init` there.

```bash
    ~$ mkdir brog && cd brog
    ~/brog$ hakyll-init
```

That dumps a fully working setup into our blog directory, ready for our
ignorant, ham-fisted baby steps towards liberated publishing.

Next we'll add support for math like this sexy thing: $\int_a^b f(x) dx$.

We will need some Pandoc specifics so open up `site.hs` and add it to our
imports.

```haskell
    ...
    import           Text.Pandoc
    ...
```

Further down, you'll see the all important expression for
handling new blog posts:

```haskell
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
```

What we care about is the `pandocCompiler` function. Hakyll's default Pandoc
settings don't include support for MathML, let's add it.

```haskell
    pandocMathCompiler = pandocCompilerWith readers writers
      where
        readers = def { readerExtensions = pandocExtensions }
        writers = def { writerHTMLMathMethod = MathML (Just "") }
```

What we've done here is simply defined a new compiler with some fancier options.
By using `pandocExtensions` with our reader, we enable a lot of
[goodies](http://hackage.haskell.org/packages/archive/pandoc/1.10.0.5/doc/html/src/Text-Pandoc-Options.html#pandocExtensions "src/Text/Pandoc/Options.hs")
that we'll probably eventually want anyway. Feel free to make a more reserved
choice here, but make sure it includes `Ext_tex_math_dollars`.

The writer is a little more straight forward. The default options are all fine
with the exception of `writerHTMLMathMethod`, which we change to output
[MathML](https://en.wikipedia.org/wiki/MathML "MathML - Wikipedia, the free encyclopedia").

Alright we're almost there. Plug this new compiler in in place of the default
one and we'll be done with `site.hs` entirely.

```haskell
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            ...
```

*Technically*, we're done now. However, since most browsers' support for MathML
is [woefully
inadequate](https://www.mozilla.org/projects/mathml/demo/texvsmml.html "MathML
Torture Test") we have one final step.

We'll use the excellent [MathJax](http://mathjax.org "Beautiful math in all
browsers.") library to take us the rest of the way.

Open the default template for all our pages, `templates/default.html` and look
for the closing `</head>` tag. Just *before* it we'll insert the MathJax script
and a config option to enable MathML support and MathJax goodies like
click-to-zoom.

```html
    ...
    <link rel="stylesheet" type="text/css" href="/css/syntax.css" />
    <script type="text/javascript" src=
    "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" />
    </head>
...
```

OK. That's it! Now we write a post with some math in it...

```markdown
    ---
    title: Math Test
    ---
    $\sum_{i=0}^\infty \frac{i}{i+1}$
```

and give it a whirl:

```bash
    brog$ ghc --make site
    brog$ site preview
```

TADA!
-----

Now that we've done all the hard work, we're just one step away from syntax
highlighting too.

In general, Pandoc will generate the appropriate CSS to color the code, but
since Hakyll only uses Pandoc to create fragments of HTML instead of stand alone
pages, it is unable to do so. We'll help it out by adding the appropriate CSS
manually.

First we need to grab the CSS file from Pandoc.  [Here's
mine](https://gist.github.com/raw/4660579/84f292e8f546693f999443935307609233ba7cbc/syntax.css).
You can generate your own using a different color theme by simply running Pandoc
on your markdown file with the appropriate flags. Check out `pandoc help`.

**Note: Using line numbers will *break this CSS file*. You will need to generate
your own if you want to use line numbers.**

Once you have a CSS file, save it to `css/syntax.css`.

Again we'll need to open up `templates/default.html` and make a small addition.
Just above where we added the MathJax library we'll add a link to our new CSS.

```html
    ...
    <link rel="stylesheet" type="text/css" href="/css/syntax.css" />
    <script type="text/javascript"
    ...
```

And a quick test:


````markdown
    ---
    title: Math Test
    ---
    $\sum_{i=0}^\infty \frac{i}{i+1}$

    ```haskell
    sum :: Num a => a
    sum [ x / succ x | x <- [0..]]
    ```
````

Refresh the browser and bask in the glory of your first post!

<div class="indent">
title: Math Test
---
$\sum_{i=0}^\infty \frac{i}{i+1}$

```haskell
sum :: Num a => a
sum [ x / succ x | x <- [0..]]
```
</div>

