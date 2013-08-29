# What is it?

Hquery can be used to manipulate xmlhtml trees (and has a similar selector
syntax to jquery, hence the name). This enables easy manipulation and content
generation from html templates. If you're familiar with [Lift's][1]
[CssSel][2]s, this is basically an implementation of those in haskell.

[![Build Status](https://travis-ci.org/tych0/hquery.png)](https://travis-ci.org/tych0/hquery)


# Examples

Suppose you have the template:
```html
<div class="person">
  <div class="name"></div>
  <div class="occupation"></div>
</div>
```

If you invoke hquery as:
```haskell
import Text.Hquery
template = ... -- parse your template here
people = [ ("Justin Bieber", "Celebrity")
         , ("Jens Kidman", "Musician")
         ]
bindPerson (n, o) = hq ".name *" n . hq ".occupation *" o
f = hq ".person *" $ map bindPerson people
f template
```

You'll get:
```html
<div class="person">
  <div class="name">Justin Bieber</div>
  <div class="occupation">Celebrity</div>
</div>
<div class="person">
  <div class="name">Jens Kidman</div>
  <div class="occupation">Musician</div>
</div>
```

You can also add, remove, and append to element attributes. For example if we
have: `<div class="foo"></div>` , below are some example transformations:

  * `hq "div [class+]" "hidden"` gives `<div class="foo hidden"></div>`
  * `hq ".foo [id]" "bar"` gives `<div id="bar" class="foo"></div>`
  * `hq "* [class!]" "foo"` gives `<div></div>`

# Installation

Hquery is availiable on [HackageDB][3], via `cabal install hquery`.

# License

Hquery is MIT licensed.

  [1]: http://liftweb.net
  [2]: http://simply.liftweb.net/index-7.10.html
  [3]: http://hackage.haskell.org/package/hquery
