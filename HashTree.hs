{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
        compress r (x : z : xs) = compress (node x z : r) $ xs

-- TODO przepisać na ShowString
drawTree :: Show a => Tree a -> String
drawTree = drawTreeHelp "  " where
    drawTreeHelp :: Show a => String -> Tree a -> String
    drawTreeHelp indent (Leaf h a) = indent ++ " " ++ showHash h ++ " " ++ show a ++ "\n"
    drawTreeHelp indent (Twig h x) = indent ++ " " ++ showHash h  ++ " - \n" ++ drawTreeHelp ("  " ++ indent) x
    drawTreeHelp indent (Node h l r) = indent ++ " " ++ showHash h ++ " + \n" ++ drawTreeHelp ("  " ++ indent) l ++ drawTreeHelp ("  " ++indent) r
type MerklePath = [Either Hash Hash]

data MerkleProof a = MerkleProof a MerklePath

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x (Leaf h a)
  | hash x == h = Just $ MerkleProof x []
  | otherwise = Nothing
buildProof x (Twig h s) = append (Left $ treeHash s) <$> buildProof x s
buildProof x (Node h l r) = case buildProof x l of
  Just ret -> Just $ append (Left $ treeHash r) ret
  Nothing -> append (Right $ treeHash l) <$> buildProof x r

append :: Either Hash Hash -> MerkleProof a -> MerkleProof a
append x (MerkleProof a path) = MerkleProof a (x : path)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths a (Leaf h val)
  | h == hash a = [[]]
  | otherwise = []
merklePaths a (Twig h s) = map ((Left $ treeHash s) :) $ merklePaths a s
merklePaths a (Node h l r) = map ((Left $ treeHash r) :) (merklePaths a l) ++ map ((Right $ treeHash l) :) (merklePaths a r)


showEither :: Either Hash Hash -> String
showEither (Left h) = "<" ++ showHash h
showEither (Right h) = ">" ++ showHash h

showsMerklePath :: MerklePath -> ShowS
showsMerklePath = showString . concatMap showEither

showMerklePath :: MerklePath -> String
showMerklePath p = showsMerklePath p ""

instance Show a => Show (MerkleProof a) where
  showsPrec d (MerkleProof x p) = showParen (d > 10) $
        showString "MerkleProof " .
        showsPrec 11 x .
        showString " " .
        showsMerklePath p

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof hToCheck (MerkleProof a proof) = foldr g (hash a) proof == hToCheck where 
    g (Left h) hacc = hash (hacc, h)
    g (Right h) hacc = hash (h, hacc)


-- instance Foldable Tree where
--     foldMap f $ Leaf h a = f a
--     foldMap f $ Twig h s = `mappend`
