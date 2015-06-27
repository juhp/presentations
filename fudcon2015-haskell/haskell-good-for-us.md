% Haskell\ is\ good for\ you!
% Jens Petersen (juhp)
  <petersen@fedoraproject.org>
% 27 June 2015 -- FUDCon Pune 2015

# Learning Haskell makes you a better programmer

<div class="notes">
Not possibly to teach how to program in Haskell in 40min

but try anyway for a subset

give a taste of Haskell
</div>

## Slides

<https://petersen.fedorapeople.org/talks/fudcon-pune-2015/>

(<http://bit.ly/haskellgood>)

# What is Haskell?

## Haskell is a

- Functional
- Pure
- Lazy
- Statically typed

programming language.

<div align="right">
<small>(Haskell Basics by Brent Yorgey)</small>
</div>

## Functional

- functions are first-class
- evaluation of expressions rather than\
  executing instructions\
  (functional vs imperative)

## Pure

- variables and data structures are immutable
- expressions do not have “side effects”
- calling the same function with same args gives same result every time: *deterministic*

⇓

- Equational reasoning
- Easy refactoring
- Parallelism

## Lazy
- evaluation by need

⇓

- can easily define control structures
- allows handling of infinite data structures

## Statically typed

- Every Haskell expression has a type
- Types are type-checked at compile-time
- Programs with type errors or mismatches\
  will not compile

## Haskell 25 years old
designed by a committee of CS academics\
in the late 80's

to create a standard lazy\
functional programming language

![](https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png "Haskell Logo")

# Haskell uses indentation heavily like Python

# GHC

## Glasgow Haskell Compiler
- BSD license
- native compiler
- ghci interactive client

## ghc
- <https://www.haskell.org/ghc/>
- Latest stable release is 7.10.1
- Fedora 22 has 7.8.4
- Native Code Generator for i686/x86_64
- ARM uses LLVM backend
- ghc is a large project
- many optional extensions to Haskell

<div class="notes">
## ghc
- good support for concurrency and parallelism, including support for Software Transactional Memory (STM).
- generates fast code, particularly for concurrent programs. See [The Computer Language Benchmarks Game].
- works on Linux, MacOS, Windows, most varieties of Unix, and several different processor architectures.
- interactive environment ghci compiles Haskell to bytecode, and supports execution of mixed bytecode/compiled programs.
</div>

# Cabal and Hackage

## Cabal

Haskell package build system

```
$ ghc Setup
$ ./Setup configure
$ ./Setup build
$ ./Setup install
```

like Python's pip

## Hackage

source repository of open source Haskell packages

<http://hackage.haskell.org/>

## cabal-install

User package manager for Hackage packages

<div align="left">
eg to install pandoc
</div>
```
$ cabal install pandoc
```
resolves and builds dependencies

# Haskell Basics

## Basic types

`::` = "has type"


```haskell
True :: Bool

1 :: Int                 -- machine integers
2^2^2^2^2 -1 :: Integer  -- arbitrary precision

'a' :: Char
"Hello!" :: String

1.0 :: Double

() :: ()
```

## Functions

```haskell
add :: Integer -> Integer -> Integer
add m n = m + n

add 3 2 ⟹ 5 :: Integer

-- partial application
add 5 :: Integer -> Integer
```

## Functions vs operators
prefix vs infix

```haskell
2 + 4 ⟹ 6
-- prefix
(+) 2 4 ⟹ 6

div 4 2 ⟹ 2
-- infix
4 `div` 2 ⟹ 2
```

# Lists

## Lists
`[a]` is a list of elements of type `a`

The type variable `a` is an example of polymorphism.

## List examples
```haskell
-- empty list
[] :: [a]

[True, False, True] :: [Bool]

[1..5] :: [Int]

["Haskell", "is", "good"] :: [String]

```

Note `['a', 1, True]` is not a valid Haskell list!

## List append

```haskell
(++) :: [a] -> [a] -> [a]

[1, 2] ++ [3, 4] ⟹ [1, 2, 3, 4]
```

## Polymorphic list functions
```haskell
head :: [a] -> a
tail :: [a] -> [a]
length :: [a] -> Integer
```

```
head [1, 2, 3] ⟹ 1
tail [1, 2, 3] ⟹ [2, 3]
length [1, 2, 3] ⟹ 3

length $ show $ 2^2^2^2^2 -1 ⟹ 19729
```

## Cons constructor
```
a : as

1 : [2, 3] ⟹ [1, 2, 3]
```

## map

```haskell
map :: (a -> b) -> [a] -> [b]

map (add 3) [1, 2, 3] ⟹ [4, 5, 6]
```

# Laziness

## Laziness
Lazy evaluation allows infinite lists:

```haskell
naturals = [0..]
```

Fibonacci series

```haskell
fib :: Int -> Int
-- naive definition
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

# Let and with

## let vs with

```haskell
let x = 2 in
  x*x + 3*x + 1

sumsquare x y = square x + square y
  where
  square u = u * u
```

# Comprehensions

## Comprehensions
```haskell
squares = [ x * x | x <- [0..] ]

-- "nested 'for' loop"
pairs = [(x,y) | x <- [1..10], y <- [1..10]]
```

Python's comprehensions come from Haskell

## Primes example

```haskell
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
```

<div align="right">
<small>(from <http://haskell.org>)</small>
</div>

# Tuples

## Tuples
- `(a, b)` is a pair
- `(a, b ,c)` is a triple
- etc

```haskell
("Life", 42) :: (String, Int)
(False, 'a', 1.1) :: (Bool, Char, Float)
```

# Pattern matching

## Pattern matching
```haskell
take :: Int -> [a] -> [a]
take  0     _           =  []
take  _     []          =  []
take  n     (x:xs)      =  x : take (n-1) xs
```

## Case statement

```haskell
data Arch = X86 | X86_64 | ARMv7

case arch of
   X86 -> "/usr/lib"
   X86_64 -> "/usr/lib64"
   otherwise -> "/usr/lib"
```

# Types

## Static type safety

vs

- Python backtraces
- Java NullPointerException

## Type inference

type annotations are generally optional

## Data types
```haskell
data Maybe a = Just a | Nothing

data Bool = False | True

-- record types
data Point = Pt Float Float
pt = Pt 1.0 2.0

-- recursive datatype
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

# Monads and IO

## IO Monad
```haskell
putStrLn :: String -> IO ()

getLine :: IO String
```

### do blocks
```haskell
do
let greet = "Hello"
name <- getLine
putStrLn $ greet ++ " Your name is " ++ name
```

## Hello World
```
$ cat > test.hs
main = putStrLn "hello"
$ ghc test.hs
$ ./test
hello
$ 
```

# Concurrency

## Lightweight Threads
in GHC runtime

```haskell
forkIO :: IO () -> IO ThreadId
```

## async
<https://hackage.haskell.org/package/async>

```haskell
(page1, page2) <- concurrently (getURL url1) (getURL url2)
```

## Software Transactional Memory

concurrency control mechanism for shared memory,\
analogous to database translations

# Shell

## turtle shell
- <https://hackage.haskell.org/package/turtle>

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

## pandoc

markup conversion tool

<http://pandoc.org/>

eg markdown to html, etc


These slides created with pandoc: <http://github.com/juhp/presentations>

## xmonad

X tiling Window Manager

<http://xmonad.org/>

## Stackage and stack
<http://stackage.org>

Stable consistent version subsets of Hackage packages

## shake

Build system library to replace Make

## hledger

accounting

## git-annex

manage files across systems with git\
without checking them in

## gtk2hs

gtk stack bindings

## Agda and idris

Dependently-typed programming languages

## hakyll

Static website generator

## yi

Extensible customizable editor (like Emacs)

## Web frameworks
- [Yesod](http://www.yesodweb.com/)
- Snap
- Happstack
- Scotty

- Warp webserver can\
["provide performance on a par with nginx"](http://www.aosabook.org/en/posa/warp.html)\
and "three times faster than Node".

<div class="notes">
Thanks to the concurrency of GHC's IO manager,
scales linearly to 32 cores/threads.
</div>

# Fedora Haskell

## Fedora Haskell SIG
- <https://fedoraproject.org/wiki/Haskell_SIG>

- IRC: \#fedora-haskell
- <haskell@lists.fedoraproject.org>

## Fedora Haskell Packaging
- \> 300 source packages
- cabal-rpm packaging tool
- copr repos

# Resources

## Resources
- <http://haskell.org>
- <https://wiki.haskell.org/Tutorials>
- [School of Haskell](https://www.fpcomplete.com/school)
- [Haskell Communities and Activities Report](https://wiki.haskell.org/Haskell_Communities_and_Activities_Report)
- StackOverflow
- Haskell Reddit

- mailing-lists: `beginners` and `haskell-cafe`
- irc: #haskell-beginners and #haskell

# Thank you

## Questions?

![](https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png "Haskell Logo")

- <http://haskell.org>
- <http://wiki.haskell.org>
- <https://www.fpcomplete.com/school>

Contact: petersen@fedoraproject.org (juhp)
