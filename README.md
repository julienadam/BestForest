## Jurassic Forest

During [Breizhcamp 2023](https://www.breizhcamp.org/), [Scalian](https://www.scalian.com/) proposed a [little coding challenge](https://jurassic-forest.scalian.com/challenge) (now offline). The goal was to optimize the placement of trees on a grid in order to maximize growth according to a set of rules and input data.

Since I like puzzles I whipped up a little F# script and got to about a score of 6500 using a very crude randomization and selection algorithm.

This version is more polished, it uses [simulated annealing](https://en.wikipedia.org/wiki/Simulated_annealing) which is a much better way of doing what I was doing. It also uses [Spectre.Console](https://spectreconsole.net/) for visualization, so you can see the map evolve graphically (requires an ansi compatible terminal and more than 100 columns and 60 rows for optimal display). It now easily goes to a score of 12000+.

Many thanks to Paul M. and Scalian for the challenge !

## Instructions

### Prerequisites

.NET 6.0 with F# support

### Usage

```sh
dotnet build && dotnet run
```

This will run for some time (until temp reaches 0.01), display progress and update the map.txt file continuously. If no map.txt is present at startup, it will create a random one. If it already exists, it will take that as its baseline and try to optimize it. Multiple runs generally improve the solution, with diminishing returns.
