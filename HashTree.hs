module HashTree where
import Hashable32 (Hash, Hashable (hash))


data Tree a = Leaf {treeHash::Hash, value::a} 
            | Node {treeHash::Hash, left::Tree a, right::Tree a}
            | Twig {treeHash::Hash, son::Tree a}

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig t = Twig (hash (h, h)) t where h = treeHash t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node left right = Node (hash ((treeHash left), (treeHash right)) left right


-- buildTree :: Hashable a => [a] -> Tree a
-- treeHash :: Tree a -> Hash
-- drawTree :: Show a => Tree a -> String