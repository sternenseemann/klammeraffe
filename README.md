# klammeraffe

![](./close-all-the-parens.jpg)

This IRC bot processes each message and checks if there are unclosed parentheses. If there are some then it closes them. Because klammeraffe recognizes smile like `:(` and `:}` and feels with the people using them, it allows the violation of the holy rules in those cases.

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
