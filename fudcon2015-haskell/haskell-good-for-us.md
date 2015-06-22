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

# What is Haskell

Haskell is

- Functional
- Pure
- Lazy
- Statically typed


<div class="notes">
- functions are first-class
- programs centered around evaluating expressions rather than executing instructions
- variables and data structures are immutable
- Expressions never have “side effects”
- Equational reasoning and refactoring
- Parallelism
</div>

# Basic types

```
Bool     True, False
Int      -1, 0 , 2, 3, 24
Integer  -2, 0, 1, 6, 26
Char     'a', 'b', 'C', '!'
String   "Hello there!"
Double   1.0
()       ()
```

# Functions
```haskell
add :: Integer -> Integer -> Integer
add m n = m + n

add 3 2  => 5 :: Integer

add 5 :: Integer -> Integer
```

# Lists
`[a]` is a list of type `a`

The type variable `a` is an example of polymorphism.

```haskell
[True, False, True] :: [Bool]
[1..5] :: [Int]
['C', 'A', 'B'] :: [Char]
```

```haskell
head :: [a] -> a
tail :: [a] -> [a]
length :: [a] -> Integer
```

```
head [1, 2, 3] => 1
tail [1, 2, 3] => [2, 3]
length [1, 2, 3] => 3
```

# Lists (2)
Note `['a', 1, True]` is not a Haskell list!

Haskell actually defines `String` to be [Char]!

Cons constructor:
```
a : as
```

# Laziness

Lazy evaluation allows infinite lists:

```haskell
naturals = [1..]
```

Fibonacci series

# Comprehensions

```haskell
perfect = [ x*x | x <- naturals ]

pairs = [(x,y) | x <- [1..10], y <- [1..10]]
```

# Tuples
- `(a, b)` is a pair
- `(a, b ,c)` is a triple
- etc

```haskell
("Life", 42) :: (String, Int)
(False, 'a', 1.1) :: (Bool, Char, Float)
```

# Pattern matching

```haskell
take :: Int -> [a] -> [a]
take  0     _           =  []
take  _     []          =  []
take  n     (x:xs)      =  x : take (n-1) xs
```

# Static type safety
- Python backtraces (screenshot!)
- Java NullPointerException

# Newtype and data types
```haskell
data Maybe a = Just a | Nothing

data Bool = False | True

-- recursive datatype
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

# Functional Purity
- quarantees no side-effects

# GHC
- <https://www.haskell.org/ghc/>
- Latest stable release is 7.10.1
- Fedora 22 has 7.8.4
- Native code generator for Intel
- ARM uses LLVM backend

- good support for concurrency and parallelism, including support for Software Transactional Memory (STM).
- generates fast code, particularly for concurrent programs. See [The Computer Language Benchmarks Game].
- works on Windows, Mac, Linux, most varieties of Unix, and several different processor architectures.
- interactive environment ghci compiles Haskell to bytecode, and supports execution of mixed bytecode/compiled programs.

# Hello World
```
$ cat > test.hs
main = putStrLn "hello"
$ ghc test.hs
$ ./test
hello
$ 
```

# Monads and IO
```haskell
putStrLn :: String -> IO ()

getLine :: IO String
```

# Concurrency
- threads
- [async](https://hackage.haskell.org/package/async)

```haskell
(page1, page2) <- concurrently (getURL url1) (getURL url2)
```

# Shell
- [turtle](https://hackage.haskell.org/package/turtle)

```haskell
#!/usr/bin/env runhaskell
                           -- #!/bin/bash
import Turtle              --
                           --
main = do                  --
    dir  <- pwd            -- DIR=$(pwd)
    time <- datefile dir   -- TIME=$(date -r $DIR)
    print time             -- echo $TIME
```

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
- <http://haskell.org>
- <http://wiki.haskell.org>
- [School of Haskell](https://www.fpcomplete.com/school)
- [Haskell Communities and Activities Report](https://wiki.haskell.org/Haskell_Communities_and_Activities_Report)
