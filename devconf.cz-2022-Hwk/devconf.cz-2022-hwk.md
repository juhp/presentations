---
title: Text processing with Hwk @ Devconf.cz
author: Jens Petersen (@juhp) - 2022
patat:
    wrap: true
    margins:
        left: 10
        right: 10
...

hwk is a text processing tool.

It munges input using pure functions.

# Short history

hwk was [started](https://github.com/lukasmartinelli/hwk)
by Lukas Martinelli in 2016.

There is also [Hawk](https://github.com/gelisam/hawk) (Haskell awk)
which is similar but has a more opinionated CLI.

I started hacking on hwk in 2020:
it seemed an easier starting point being less mature.

The naming is also slightly misleading hwk/hawk is not really
awk or sed or grep, but something a bit different or more general.

# Getting started

Install the package or build from source.

```shellsession
$ hwk --help
A Haskell awk/sed like tool

Usage: hwk [--version]
           [(-l|--line) | (-w|--words) | (-a|--all) | (-t|--typecheck) |
             (-e|--eval) | (-r|--run) | (-s|--shell)] [-c|--config-dir DIR]
           FUNCTION [FILE...]
  Simple shell text processing with Haskell

Available options:
  -h,--help                Show this help text
  --version                Show version
  -l,--line                Apply function to each line
  -w,--words               Apply function to list of words per line
  -a,--all                 Apply function once to the whole input
  -t,--typecheck           Print out the type of the given function
  -e,--eval                Evaluate a Haskell expression
  -r,--run                 Run Haskell IO
  -s,--shell               Haskell Shell
  -c,--config-dir DIR      Override the config dir
```

# Example 0: Identity

The identity function
```haskell
id :: forall a . a -> a
```

```shellsession
$ seq 3 | hwk id
1
2
3
```

# Example 1: echo $(cat)

```haskell
unwords :: [String] -> String
```

```shellsession
$ seq 3 | hwk unwords
1 2 3
```

# Example 2: match

```haskell
filter (== "needle") :: [String] -> [String]
```

```shellsession
$ seq 5 | hwk 'filter (== "2")'
2
```

# Example 3: infix match

```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

```shellsession
$ seq 12 | hwk 'filter (isInfixOf "2")'
1
12
```

# Example 4: wc -l

```haskell
length :: [a] -> Int
```

```shellsession
$ seq 12 | hwk length
12
```

```shellsession
$ seq 12 | hwk --line length | hwk unwords
1 1 1 1 1 1 1 1 1 2 2 2
```

# Example 5: wc -w

```haskell
words :: String -> [String]
```

```shellsession
$ seq 12 | hwk --all '(length . words)'
12
```

# Example 6: sed s/old/new/g

```haskell
replace :: String -> String -> String -> String
```

```shellsession
$ seq 3 | hwk -l 'replace "2" "6"'
1
6
3
```

# More interesting: numbers

`hwk -w "filter ((\i -> i<5 && i>0) . length)`

lines with 1-4 words


```shellsession
$ seq 5 | hwk 'filter ((== 0) . (`mod` 2) . int)'
2
4
```

# Example 7: sed /word/d

```shellsession
$ seq 3 | hwk 'filter (not . isInfixOf "2")'
1
3
```

# Example 8: wc -c

`hwk -a length`

# Example 9: version sort

```haskell
sortOn :: Ord b => (a -> b) -> [a] -> [a]
```

```shellsession
$ echo -e "1.10\n1.9" | hwk 'sortOn readVersion'
1.9
1.10
```

# Example 10: reverse

```haskell
reverse :: [a] -> [a]
```

```shellsession
$ seq 3 | hwk reverse
3
2
1
```

```shellsession
$ seq 10 12 | hwk -l reverse
01
11
21
```
