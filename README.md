# Templatespiler

A transpiler from a template language to many popular programming languages

## Motivation

For coding challenges, it's useful to be able to provide a template solution that handles the stdio boilerplate.
It would be reasonable to expect challenge authors to write a template in *every* language, so an alternative is needed.

Templatespiler provides a DSL for describing stdio which can then be transpiled into many popular programming languages, as idiomatically as possible. Note that Templatespiler is *not* a fully-fledged programming language, and so only provides the basic stdio functionality.

Templatespiler also does not intend to produce error-safe code - the code produced will assume that the inputs are valid, and will not check for errors. This is because the code is intended to be used for coding challenges, where the inputs are guaranteed to be valid.

For example, suppose we have a challenge to return the sum of all the prime numbers in a list.
We can describe the inputs using Templatespiler like so 
```js
count : Integer
inputs: array count (num : Integer)
```

which we can turn into Python: 
```py
count = int(input())
for i in range(0, count):
  num = int(input())

print("solution")
```

, C:

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

Templatespiler is defined as a dictionary of variable names to inputs, which may be combined using *combinators*. For example, we have the `array` combinator, which takes a length, a type, and then parses that many inputs into an array, each on its own line.

Notice that the parameter names are not always needed - for example, Python and C do not put the inputs into a list, since it's usually more idiomatic to do the processing in the for loop in imperative languages. On the other hand, we do put the inputs into a list for Haskell, since it usually enables more idiomatic usage.

For a more advanced example, suppose we have a challenge where a set of coordinate translations are given, and we need to find the final position of a point after applying all the translations. 

For example, an input might look like this: 
```
0 0
3
3 2
-1 1
2 0
```

We can describe the inputs using Templatespiler like so 
```js
start: sep-by " " [x : Integer, y : Integer]
inputs: list (sep-by " " [x : Integer, y : Integer])
```


The `sep-by` combinator combines inputs separated by a separator, in this case `" "`. 
The `list` combinator is a dynamic version of `array`, where the length is specified as the first input

which we can turn into Python: 
```py
start = input().split()
start = (int(start[0]), int(start[1]))

count = int(input())

for i in range(0, count):
  x, y = input().split()
  x, y = int(x), int(y)

print("(x, y)")
``` 
or Haskell:

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

### Complex Example
Let's do an example with heavily nested inputs. For this challenge, we have a list of item names and prices, and then a list of people, who all have their own shopping list. We need to find the total cost of all the items on all the shopping lists.


An example input will look like this: 
```
5
apple 1.0
milk 2.0
bread 2.5
cheese 3.0
chocolate 4.0
3
Alice
2
2 apple
1 milk
Bob
4
1 apple
1 milk
3 bread
2 cheese
Charlie
1
1 chocolate
```

We can describe the inputs using Templatespiler like so 
```js
prices: list (sep-by " " [item : String, price : Float])
orders: list [
  name: String,
  order: list (sep-by " " [quantity : Integer, item : String])
]
```

### Formal Definition

A Templatespiler program is a dictionary of variable names to *inputs*, denoted by `<name> : <input>` and separated by newlines.

Inputs may be either a *terminal type* or a *combinator*, and always describe at least 1 line of input (from stdin). 
Terminal types are:
- `Integer` - Any integer
- `Float` - Any float
- `String` - Any string that spans 1 line (i.e. doesn't contain '`\n`')

Combinators combine multiple inputs into a single input. 

The simplest combinator does not actually combine anything, and merely assigns a name to an input. This uses the same syntax as the top-level: `<name> : <input>`. However, to prevent parser ambiguity, this must often be wrapped in parentheses.

The name combinator can be very useful for generating more descriptive code, but is not required. Referring to the above example, we could modify the first line to be 
```
prices : (list (item-info: sep-by " " [item : String, price : Float]))
```
and then the generated Python code would be
```py
prices_len = int(input())
prices = []
for i in range(0, prices_len):
  item, price = input().split()
  price = float(price)
  item_info = (item, price)
  prices.append(item_info)
```

In this example, the benefits are minimal, so it's recommended to use this sparingly depending on the context.


The simplest combinator is the group combinator, which is denoted by square brackets (`[]`). This creates a "sub-dictionary" and is written using identical syntax to the top-level dictionary. Group combinators attempt to put inputs into a tuple or struct depending on the target language. By default, they also parse 1 input per line, but this can be changed using the `sep-by` combinator.
For example, writing
```
info : [
  name : String,
  age : Integer
]
```
will produce the following Python code:
```py
name = input()
age = int(input())
info = (name, age)
```
This is not especially useful on its own, so is usually combined with other combinators.

The `sep-by` combinator overrides the default behaviour of the group combinator, and instead parses inputs separated by a separator. For example, writing
```
info : sep-by " " [name : String, age : Integer]
```
will produce the following Python code:
```py
name, age = input().split()
age = int(age)
info = (name, age)
```
Separators may be any string that does **not** contain a newline. Typically this will be spaces and/or commas. 

The `array` combinator takes a length, a type, and then parses that many inputs into an array, each on its own line. For example, writing
```
info : array 3 (name : String)
```
will produce the following Python code:
```py
info = []
for i in range(0, 3):
  name = input()
  info.append(name)
```

The length must be a statically-known integer constant, variables are not allowed. 

Since we only use `name` to create the array, we can omit the variable name and write
```
info : array 3 String
```
Similar code will be generated, with an undefined variable name instead of `name`. Where possible, it is recommended to specify the variable name to make generated code more readable.


Finally, the `list` combinator is a dynamic version of `array`, where the length is specified from stdin.
Most templates will use this combinator at least once, since it is the only way to parse a variable number of inputs. For example, writing
```
info : list (name : String)
```
will produce the following Python code:
```py
info_count = int(input())
info = []
for i in range(0, info_count):
  name = input()
  info.append(name)
```

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