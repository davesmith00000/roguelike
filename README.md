# My Generic Roguelike

[Click here to play!](https://davesmith00000.github.io/roguelike/)

## What is this?

This is a reworking of the [Indigo Roguelike-Tutorials](https://github.com/davesmith00000/roguelike-tutorial) I produced in the summer of 2021.

Current status: Working, but ugly and unbalanced.

The plan is to take the game made by following the roguelike tutorials at face value, because that allows me to remove many game design considerations like: Premise, names of things, theme, purpose, core mechanics, and so on.

This is: **My Generic Roguelike!**

That said, the plan is to ultimately polish up as much as possible. The ascii terminal has already been replaced with placeholder graphics, some rudimentary animations have been added, there is a camera. Time will reveal how far I get.

## Eating my own dog food

Although I had a wonderful time producing that repository and was greatly satisfied to have completed the tutorials, the more I looked at the end result, the more I realised that the solution was ...poor. There was lots of interesting stuff in there, but it didn't serve as a good example of how to write games in Indigo, or of what Indigo can do.

This version is a _work in progress_ I'm producing in public, that has two goals:

1. Produce a well coded, idiomatic, game in [Indigo](https://indigoengine.io/) to serve as a reference for other budding game builders.
2. To take a game as far to the point of completion as I can, and drive out engine and ecosystem issues.

## Local build

The game is a completely ordinary and largely self-contained Scala project, with one exception:

You will need to do a local publish of the [roguelike-starterkit](https://github.com/PurpleKingdomGames/roguelike-starterkit). Instructions in the README, it is not difficult. 🙂
