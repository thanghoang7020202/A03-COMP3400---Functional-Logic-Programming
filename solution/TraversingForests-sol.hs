module TraversingForests 
  ( Tree (..)
  , Forest
  , forests
  ) where

-- This task is worth 20 POINTS
-- Do NOT modify anything above this line.
-- Do NOT use any imports.

{--
Let a Tree denote a connected undirected graph without cycles.
Each Node (vertex in the graph) in a tree holds a value and stores the list of its children.

Let a Forest denote a list of Trees.

Encoded in Haskell, these structures will look as follows:
--}
--              The value that the Node holds
--                 v
data Tree a = Tree a [Tree a] deriving Show
--                      ^
--                    Subtrees (children) of the node

type Forest a = [Tree a]

{--
We can traverse all values stored in a tree by performing preorder traversal.
We’ll start in the root, then traverse its leftmost children in order they appear in the list.
We’ll save all the values we encountered during this traversal in a list in the exact same order.
Let’s denote this list as a traversal.

For the following tree,
                           3
                         / | \
                        4  1  7
                       /  / \
                      2  1   8

Its traversal or the list of its values will be [3, 4, 2, 1, 1, 8, 7]
                                           root  ^ ‘~~~~‘‘~~~~~~~’ ^ right subtree
                                                      ^       ^ central subtree
                                                      left subtree


To traverse all values of a Forest, we sequentially traverse the values of its trees in the order they appear in the list.

The task:
Your task is the inverse. For a given traversal, return all trees that would produce this traversal.

forests :: [a] -> [Forest a]

For example, there are 5 forests which will produce the list [2, 1, 3] if we traverse their values:

	Forest 1: [Tree 2 [Tree 1 [Tree 3 []] ]]

            	2
               /
              1
             /
            3

	Forest 2: [Tree 2  [Tree 1 [], Tree 3 []]]
	          2
             / \
            1   3

	Forest 3: [Tree 2 [], Tree 1 [Tree 3 []]]
              2                             1
                                           /
                                          3

	Forest 4: [Tree 2 [Tree 1 []], Tree 3 []]
              2                             3
             /
           1

	Forest 5: [Tree 2 [], Tree 1 [], Tree 3 []]
            2                      1                       3

The order of Forests in your answer does not matter.

--}

-- NOTE TO STAFF: The number of forests for a list of length N is the N-th Catalan number. 
-- The forests are constructed exactly the same as Catalan numbers. 
-- This knowledge is not required to solve this problem, it is OK if students don't know about Catalan numbers.
forests :: [a] -> [Forest a]
forests [] = [[]] -- NOTE TO STAFF: The students should be able to figure out this case by themselves because it's Indunction base. 
                  -- I ask you not to answer their questions what `forests []` should be. 
forests (x:xs) = concatMap 
    (\i -> (:) 
       <$> trees x (take i xs) 
       <*> forests (drop i xs)  -- NOTE TO STAFF: Using Applicative instance for lists is not required here but significantly shortens the solution
    ) [0 .. length xs]

trees :: a -> [a] -> [Tree a]
trees root = fmap (Tree root) . forests
