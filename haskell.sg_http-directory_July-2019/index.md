% http-directory package
% written by juhp
% haskell.sg, July 2019

## Sometimes you need to browse a http directory...

# Maybe you want to check the latest ghc release?

```
Index of https://downloads.haskell.org/~ghc/

../
:
8.0.1/                                             15-Feb-2019 07:14                   -
8.0.1-rc1/                                         15-Feb-2019 10:38                   -
8.0.1-rc2/                                         15-Feb-2019 10:54                   -
8.0.1-rc3/                                         15-Feb-2019 13:38                   -
8.0.1-rc4/                                         15-Feb-2019 10:13                   -
8.0.2/                                             15-Feb-2019 05:58                   -
8.0.2-rc1/                                         15-Feb-2019 12:31                   -
8.0.2-rc2/                                         15-Feb-2019 14:56                   -
8.2-latest/                                        15-Feb-2019 16:24                   -
8.2.1/                                             15-Feb-2019 12:46                   -
8.2.1-rc1/                                         15-Feb-2019 06:51                   -
8.2.1-rc2/                                         15-Feb-2019 16:17                   -
8.2.1-rc3/                                         15-Feb-2019 13:35                   -
8.2.2/                                             15-Feb-2019 16:24                   -
8.2.2-rc1/                                         15-Feb-2019 08:40                   -
8.2.2-rc2/                                         15-Feb-2019 10:59                   -
8.2.2-rc3/                                         15-Feb-2019 12:35                   -
8.4-latest/                                        15-Feb-2019 06:39                   -
8.4.1/                                             15-Feb-2019 08:46                   -
8.4.1-alpha1/                                      15-Feb-2019 08:59                   -
8.4.1-alpha2/                                      15-Feb-2019 09:33                   -
8.4.1-alpha3/                                      15-Feb-2019 15:07                   -
8.4.1-rc1/                                         15-Feb-2019 07:47                   -
8.4.2/                                             15-Feb-2019 10:06                   -
8.4.2-rc1/                                         15-Feb-2019 09:23                   -
8.4.3/                                             15-Feb-2019 09:11                   -
8.4.4/                                             15-Feb-2019 06:39                   -
8.6-latest/                                        05-Mar-2019 19:29                   -
8.6.1/                                             15-Feb-2019 06:57                   -
8.6.1-alpha1/                                      15-Feb-2019 09:57                   -
8.6.1-alpha2/                                      15-Feb-2019 09:50                   -
8.6.1-beta1/                                       15-Feb-2019 06:45                   -
8.6.2/                                             22-Feb-2019 23:49                   -
8.6.3/                                             15-Feb-2019 11:11                   -
8.6.4/                                             05-Mar-2019 19:29                   -
8.6.5/                                             24-Jun-2019 22:55                   -
```

## <https://hackage.haskell.org/package/http-directory>

- `http-directory` is a small simple library

- A bit like `directory` "for HTTP"

It uses:

- http-client for transport
- html-conduit and xml-conduit to parse index.html files for links

# http-directory example

```haskell
import Network.HTTP.Directory
import qualified Data.Text as T
import Data.Char

main :: IO ()
main = do
  let url = "https://downloads.haskell.org/~ghc/"
  dirs <- httpDirectory' url
  let vs = filter (isDigit . T.head) dirs
  putStrLn $ url <> T.unpack (last vs)
```

This currently outputs:

<https://downloads.haskell.org/~ghc/8.8.1-alpha2/>

# http-directory API

## Listing

```haskell
httpDirectory :: Manager -> String -> IO [Text]
```
List the file links (hrefs) in an http directory

## Properties

```haskell
httpExists :: Manager -> String -> IO Bool
```
Test if an file (url) exists

<hr/>
```haskell
httpFileSize :: Manager -> String -> IO (Maybe Integer)
```
Try to get the filesize (Content-Length field) of an http file

<hr/>
```haskell
httpLastModified :: Manager -> String -> IO (Maybe UTCTime)
```
Try to get the modification time (Last-Modified field) of an http file

# HTTP Redirects
```haskell
httpRedirect :: Manager -> String -> IO (Maybe ByteString)
```
Return final redirect for an url

<hr/>
```haskell
httpRedirects :: Manager -> String -> IO [ByteString]
```
Returns the list of http redirects for an url in reverse order

# Problems

Actually there is no standard format for http "directories"

- Different httpd's produce different output!
  - eg some use `href="dir/"` others just `href="dir"`
  - some have column sorting
  - etc

Redirects to mirrors can invalidate paths

# Dependents

<http://packdeps.haskellers.com/reverse/http-directory>

A few (of my) packages are using http-directory:

- dl-fedora
- findhttp
- pkgtreediff
- stackage-query

Thanks, EOF

<hr/>
*(slides made with pandoc/Slidy and weasyprint)*
