module HashTree where
import Hashable32 (Hash, Hashable (hash), showHash)
import Data.Tree (drawTree)


data Tree a =
            Leaf {treeHash::Hash, value::a}
            | Node {treeHash::Hash, left::Tree a, right::Tree a}
            | Twig {treeHash::Hash, son::Tree a}

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig t = Twig (hash (h, h)) t where h = treeHash t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node left right = Node (hash (treeHash left, treeHash right)) left right

buildTree :: Hashable a => [a] -> Tree a
buildTree = buildTreeFromTree . map leaf
    where
        buildTreeFromTree :: Hashable a => [Tree a] -> Tree a
        buildTreeFromTree [x] = x
        buildTreeFromTree x = buildTreeFromTree (reverse (compress [] x))
        compress :: Hashable a => [Tree a] -> [Tree a] -> [Tree a]
        compress r [] = r
        compress r [x] = twig x : r
        compress r (x:xs) = compress (node x (head xs) : r) $ tail xs


drawTree :: Show a => Tree a -> String
drawTree = drawTreeHelp "  " where
    drawTreeHelp :: Show a => String -> Tree a -> String
    drawTreeHelp indent (Leaf h a) = indent ++ " " ++ showHash h ++ " " ++ show a ++ "\n"
    drawTreeHelp indent (Twig h x) = indent ++ " " ++ showHash h  ++ " - \n" ++ drawTreeHelp ("  " ++ indent) x
    drawTreeHelp indent (Node h l r) = indent ++ " " ++ showHash h ++ " + \n" ++ drawTreeHelp ("  " ++ indent) l ++ drawTreeHelp ("  " ++indent) r