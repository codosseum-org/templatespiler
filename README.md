# Templatespiler

A transpiler from a template language to many popular programming languages

## Motivation

For coding challenges, it's useful to be able to provide a template solution that handles the stdio boilerplate.
It would be reasonable to expect challenge authors to write a template in *every* language, so an alternative is needed.

Templatespiler provides a DSL for describing stdio which can then be transpiled into many popular programming languages, as idiomatically as possible. Note that Templatespiler is *not* a fully-fledged programming language, and so only provides the basic stdio functionality.

For example, suppose we have a challenge to return the sum of all the prime numbers in a list.
We can describe the inputs using Templatespiler like so 
```
count = readln Integer
inputs = for count as i { 
    num = readln Integer
}

println "solution"
```

which we can turn into Python: 
```py
count = int(input())
for i in range(0, count):
  num = int(input())

print("solution")
```,

C: 
```c
#include <stdio.h>

int main() {
    int count;
    scanf("%d\n", &count);

    for (int i = 0; i < count; i++) {
        int num;
        scanf("%d\n", &num);
    }
    printf("%s\n", "solution");
}
```

or Haskell:
```hs
import Control.Monad

main :: IO ()
main = do
    count <- readLn @Int
    inputs <- replicateM count $ do
        num <- readLn @Int
        pure num
  
    putStrLn "solution"
```

Notice that the parameter names are not always needed - for example, Python and C do not put the inputs into a list, since it's usually more idiomatic to do the processing in the for loop in imperative languages. On the other hand, we do put the inputs into a list for Haskell, since it usually enables more idiomatic usage.

For a more advanced example, suppose we have a challenge where a set of coordinate translations are given, and we need to find the final position of a point after applying all the translations. 

We can describe the inputs using Templatespiler like so 
```
start = readln Integer Integer
count = readln Integer

inputs = for count as i { 
    x, y = readln Integer Integer
    yield x, y
}

println "(x, y)"
```

which we can turn into Python: 
```py
start = input().split()
start = (int(start[0]), int(start[1]))

count = int(input())

for i in range(0, count):
  x, y = input().split()
  x, y = int(x), int(y)

print("(x, y)")
``` or Haskell:
```hs
import Control.Monad



main :: IO ()
main = do
  start <- fmap read . words <$> getLine
  count <- readLn @Int

  inputs <- replicateM count $ do
    (x, y) <- fmap read . words <$> getLine
    pure (x, y)
  
  putStrLn "(x, y)"
```

The `yield` keyword is primarily for functional languages and will often be ignored in imperative languages. It is used to describe the "return value" of the for loop that is added to the list of inputs.
If no `yield` is present, the last expression in the for loop will be implicitly yielded. 

## Getting Started

*tldr: Install Nix, enable Flakes, open in VSCode and run `just run`.*

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `nix run github:srid/nixci` to build _all_ outputs.
- Run `just fmt` in nix shell to autoformat the project. This uses [treefmt](https://github.com/numtide/treefmt).
- Run `just docs` to start Hoogle with packages in your cabal file.
- Run the application without installing: `nix run github:srid/haskell-template` (or `nix run .` from checkout)
- Common workflows
  - Adding library dependencies in Nix: https://zero-to-flakes.com/haskell-flake/dependency
  - Adding tests: https://srid.ca/haskell-template/tests