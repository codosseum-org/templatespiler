default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -c "cabal v2-repl exe:converter" --warnings -T :main

bnfc:
    rm -rf templatespiler-bnf/src && bnfc --haskell --text-token --functor --generic -d  -p Language -o templatespiler-bnf/src templatespiler.cf && mv templatespiler-bnf/src/Language/Templatespiler/Test.hs templatespiler-bnf/src/Main.hs # stupid dumb hack to make cabal work
