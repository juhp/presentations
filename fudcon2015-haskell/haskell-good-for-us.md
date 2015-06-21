% Haskell is good for you!
% Jens Petersen (juhp)
  <petersen@fedoraproject.org>
% 27 June 2015 - FUDcon 2015, Pune

# Intro

- haskell.org
- ghc and ghci
- cabal-install
- Hackage and Stackage
- Monads
- Type Classes

<div class="notes">
This is my note.
</div>

# What is Haskell

Haskell is a modern, general-purpose purely functional programming language, with lazy semantics, strong static typing, and a passionate community. Haskell's powerful type system with type inference, type classes, and pure higher order functions and monads give it a formal mathematical beauty and expressiveness not available in many other languages.

# Basic types

```
Bool     True, False
Int      -1, 0 , 2, 3, 24
Integer  -2, 0, 1, 6, 26
Char     'a', 'b', 'C', '!'
String   "Hello there!"
Float    1.0
```

# Functions
```
add :: Integer -> Integer -> Integer
add m n = m + n

add 3 2  => 5 :: Integer

add 5 :: Integer -> Integer
```

# Lists
`[a]` is a list of type `a`

The type variable `a` is an example of polymorphism.

```
[True, False, True] :: [Bool]
[1..5] :: [Int]
['C', 'A', 'B'] :: [Char]
```

```
head :: [a] -> a
tail :: [a] -> [a]
length :: [a] -> Int
```

```
head [1, 2, 3] => 1
tail [1, 2, 3] => [2, 3]
length [1, 2, 3] => 3
```

`['a', 1, True]` is not a Haskell list!

Haskell actually defines `String` to be [Char]!

# Tuples
- `(a, b)` is a pair
- `(a, b ,c)` is a triple
- etc

```
("Life", 42) :: (String, Int)
(False, 'a', 1.1) :: (Bool, Char, Float)
```

# Pattern matching


# Static type safety
- python backtraces (screenshot!)

# Newtype and data types

# Functional Purity
- quarantees no side-effects

# Hello World
```
$ cat > hello.hs
main = putStrLn "hello"
$ ghc hello.hs
$ ./hello
hello
$ 
```

# Monads and IO

# Concurrency
- async

# Shell
- turtle

# Projects
- pandoc: http://pandoc.org/
- xmonad: http://xmonad.org/
- conduit
- stackage and stack
- shake
- hledger
- git-annex
- gtk2hs
- Agda and idris
- hakyll
- yi

# Web frameworks
- Yesod http://www.yesodweb.com/
- Warp can "provide performance on a par with nginx" - http://www.aosabook.org/en/posa/warp.html
- Snap
- Happstack
- Scotty

# Fedora Haskell
- https://fedoraproject.org/wiki/Haskell_SIG
- cabal-rpm

# Resources
- http://haskell.org
- http://wiki.haskell.org
- School of Haskell
- HCAR https://wiki.haskell.org/Haskell_Communities_and_Activities_Report
