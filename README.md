# Study Haskell Repository
## function.hs
- find -> simple finder function for list
- sort -> simple qsort function for list
- solveEightQueenProblem -> 8-queen solver
  - check' -> check if queen is able to set to target place
- solveKnapsackProblem -> solve knapsack problem
- solveSubsetSumProblem -> solve subset sum problem

## HuffmanCoding.hs
```haskell
let x = ["a", "b", "c", "a"]
let x1 = countElm x
let x2 = tuplesToTreeNode x1
let x3 = sortNodes x2
let tree = huffmanCoding x3
getCodes "" tree
```