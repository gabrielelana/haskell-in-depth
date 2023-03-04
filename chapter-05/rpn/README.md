# RPN Exercise

Chapter 05 of "Haskell in Depth" exercise: rewrite the example of an RPN
expression evaluator with a parser combinator library.

```console
stack run rpn-exe '5 + 5'
stack run rpn-exe '5 + 5 5'
stack run rpn-exe '5'
stack run rpn-exe '2 * (10 + 1) + 20'
```

```console
stack test
```
