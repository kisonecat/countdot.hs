# countdot.hs

![Example of a few cards showing the front and back](./samples/example.png)

These are some cards students can use to practice
[subitizing](https://en.wikipedia.org/wiki/Subitizing) and counting by
grouping.  Students could play a game like
[war](https://en.wikipedia.org/wiki/War_(card_game)) using these
cards.

## Where to purchase?

You can purchase a set of cards from 
[makeplayingcards.com](https://www.makeplayingcards.com/sell/marketplace/countdot.html).


## Design

These patterns of dots are built using some "algebraic" notation, specifically

```
data DotPattern =
  Grid Integer Integer |
  Ring Integer |
  DotPattern :* DotPattern |
  DotPattern :- Integer
```

## Generating the cards

To produce the `front000.tiff` and `back000.tiff` files:

```
nix-shell
cabal build
cabal run
mutool draw -r 1200 -o front%03d.tiff front.pdf
mutool draw -r 1200 -o back%03d.tiff back.pdf
```
