# klammeraffe

> an IRC bot to CLOSE ALL THE UNCLOSED PARENS!

This IRC bot processes each message and checks if there are unclosed parentheses. If there are some then it closes them.

## Usage
### sandbox
```
git clone https://github.com/lukasepple/klammeraffe.git
cd klammeraffe
cabal sandbox init
cabal install -j2 --only-dependencies
cabal build
cabal run klammeraffe
```
