# haskell-game-of-life

My first foray into Haskell by attempting to build a Conway's Game of Life MVP.

Apologies for the lack of git history prior to the first commit: I hacked the majority of this together in [repl.it](https://repl.it/).

## Diary

Biggest frustrations / learning curves:
* order of operations, particularly for function application.
  * I'll have the exact order of application in my head and then struggle to implement that syntactically in Haskell.
  * I'm still confused by `.`s and `$`s and so I'll usually defer to parentheses all over the place.
* working with IO data.
  * Some of my biggest struggles and time sinks have been the reading and transformation of data from a text file.
  * I also wasted a lot of time trying to pretty print output of data.
  * Were I to start this project again, I would declare the starting board string inline so that data exists at compile time without having to read from IO. I'm not sure what the answer to the display problem is: part of the magic of game of life is seeing the world evolve.
* the `Data.Array` interface feels clunky and unintuitive.
  * I started my implementation with nested lists, then switched to a one dimensional list. As I moved towards the implementation of collecting and checking orthogonal neighbors, I realized that lists in Haskell are _true lists_ (singly linked) and hence aren't indexed — not great for checking neighbors.
  * Getting data into the array in the right order is a real pain
  * Some things I like about Haskell arrays:
    * Indexes can be any indexable type (ints, chars or tuples)
    * You can represent multi-dimensional arrays with tuples for indexes
  * Although I'm new to Haskell, using arrays didn't feel very idiomatic. While walking and riding around the city, I've been wondering whether something like an adjancency list would allow for a more elegant approach in Haskell. However, cyclic references [seem a little knotty](https://wiki.haskell.org/Tying_the_Knot) in Haskell.
    
Useful tools and references:
* [zvon.org](http://zvon.org/comp/r/ref-Haskell.html#intro) — particularly for the simple usage examples [like here](http://zvon.org/other/haskell/Outputarray/index.html)
* [Learn You a Haskell for Great Good](http://learnyouahaskell.com/chapters) — seems to be a well regarded, frequently referenced Haskell guide.
* [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell) — thorough reference-style book
* Stack Overflow — because most of the questions anyone will have have already been answered. However, I do note that there were not as many questions and answers on working with arrays in Haskell as I had hoped.
