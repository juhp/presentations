% Haskell Shell Scripting workshop
% Jens Petersen (juhp)
% FossAsia 2017 March 19th

# Requisites

To follow along please install _`ghc`_ from:

- <https://www.haskell.org/ghc/>

# About me:

Jens Petersen

- started using Haskell in 1998
- maintain the Fedora Haskell packages
- Haskell Stackage curator


# About you

How many people are familiar with Shell Scripting?

How many people have done some programming in Haskell?

How many people have GHC installed?

# What is shell script?

```bash
#!/bin/sh

LANG=en_US.utf8 LC_COLLATE=C XMODIFIERS=@im=none /usr/bin/emacs -g 167x51 $@
```

Traditional shell good for short quick scripting.

Portable

but hard to maintain as script grows/snowballs.

# Motivation

```bash
#!/bin/sh

echo "Hello!" +1
```

Does it run?

* Shell quoting and newline handling complicated:

`ls` vs `echo $(ls)`

ShellCheck!

# Why Functional Programming?

- Pure function composition
- Referential transparency
- Immutable predictability

- scalabilty and maintainability

# Haskell

In this workshop you will just learn enough Haskell required for basic scripting.

# Why Haskell?

Haskell is a

- Functional
- Pure
- Lazy
- *Statically typed*

programming language.

Typed lambda calculus!

# Functional

- functions are first-class
- evaluation of expressions rather than
  executing instructions
  (functional vs imperative)

# Pure

- variables and data structures are immutable
- expressions do not have “side effects”
- calling the same function with same args gives same result every time: *deterministic*

⇓

- Equational reasoning
- Easy refactoring
- Parallelism

# Lazy
- evaluation by need

⇓

- can easily define control structures
- allows handling of infinite data structures

# Statically typed

- Every Haskell expression has a type
- Types are type-checked at compile-time
- Programs with type errors or mismatches\
  will not compile

# Haskell 25 years old
designed by a committee of CS academics\
in the late 80's

to create a standard lazy\
functional programming language

![](HaskellLogoStyPreview-1.png "Haskell Logo")

# Haskell uses indentation heavily like Python

example here!

# Why ghc?

Compiled and interpreted

Standard Haskell compiler and libraries

# Haskell Basic types

`::` = "has type"


```haskell
True :: Bool

1 :: Int                 -- machine integers

'a' :: Char
"Hello!" :: String

() :: ()
```

# Lists

`[a]` is a list of elements of type `a`

The type variable `a` is an example of polymorphism.

List examples
```haskell
-- empty list
[] :: [a]

[True, False, True] :: [Bool]

[1..5] :: [Int]

["Haskell", "is", "good"] :: [String]

```

Note `['a', 1, True]` is not a valid Haskell list!

# List append

```haskell
(++) :: [a] -> [a] -> [a]

[1, 2] ++ [3, 4] ⟹ [1, 2, 3, 4]
```

# Laziness

Lazy evaluation allows infinite lists:

```haskell
naturals = [0..]
```

# Monads and IO

# IO Monad

```haskell
putStrLn :: String -> IO ()

getLine :: IO String
```

# do blocks
```haskell
do
let greet = "Hello"
name <- getLine
putStrLn $ greet ++ " Your name is " ++ name
```

# Exercise

Implement simple `ls` command in Haskell using:


```
import System.Directory
```

# HSH
```haskell
runIO :: ShellCommand a => a -> IO ()

runIO $ "ls" -|- length
```

# Lazy IO vs Streaming

# async

# Shell Monads

- [turtle](https://hackage.haskell.org/package/turtle)

- [Shelly](https://hackage.haskell.org/package/shelly)

- [shell-conduit](https://hackage.haskell.org/package/shell-conduit)

- shake [Development.Shake.Command](https://hackage.haskell.org/package/shake/docs/Development-Shake-Command.html)


# Resources

- <http://haskell.org>
- <http://www.seas.upenn.edu/~cis194/>
- <http://haskellbook.com>
- <https://www.schoolofhaskell.com/>


# Slides

<http://code.haskell.org/~juhp/talks/fossasia2017-haskell-shell/>

generated with [Pandoc](http://pandoc.org) and [Slidy2](https://www.w3.org/Talks/Tools/Slidy2)

from <https://github.com/juhp/presentations/tree/master/fossasia2017-haskell-shell/>
