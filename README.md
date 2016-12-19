Advent of Code
--------------

My solutions for [Advent of Code Challenge](http://adventofcode.com), in Haskell.

Do NOT read it if you want to have fun yourself.

The Haskell code is sometimes not very good, sometimes I wrote it in a hurry. I am a beginner and it could be improved. In particular:

* I use built-in String type (not recommended) where I should use Data.Text instead.
* I use built-in List type where arrays, vectors or binary strings would be better.
* Parsing is mostly very adhoc and done with pattern matching and list functions. I should use a proper parser.
* I use partial functions (head, last, read etc.) quite a lot, and also there are some hidden assumptions on the shape of the input everywhere in the code. 
* Many functions are missing type signatures, contrary to what is a good practice.
* I use tuples quite a lot, some data structures should be made into a proper type.
* There is no documentation for reused utility functions.
* Sometimes I don't really care about efficiency.

Note: The information in the data/ directory are copyrighted by the authors of Advent of Code Challenge.
