-- mn418323

module HashTree where
import Hashable32 (Hash, Hashable (hash), showHash)

data Tree a
  = Leaf {treeHash :: Hash, value :: a}
  | Node {treeHash :: Hash, left :: Tree a, right :: Tree a}
  | Twig {treeHash :: Hash, son :: Tree a}

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig t = Twig (hash (h, h)) t where h = treeHash t

node :: Hashable a => Tree a -> Tree a -> Tree a
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
    compress r (x : z : xs) = compress (node x z : r) xs

drawTree :: Show a => Tree a -> String
drawTree s = drawTreeS "" s ""
  where
    drawTreeS :: Show a => String -> Tree a -> ShowS
    drawTreeS indent (Leaf h a) = showString $ indent ++ showHash h ++ " " ++ show a ++ "\n"
    drawTreeS indent (Twig h x) =
      showString (indent ++ showHash h ++ " + \n")
        . drawTreeS ("  " ++ indent) x
    drawTreeS indent (Node h l r) =
      showString (indent ++ showHash h ++ " - \n")
        . drawTreeS ("  " ++ indent) l
        . drawTreeS ("  " ++ indent) r

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

showsEither :: Either Hash Hash -> ShowS 
showsEither (Left h) = showString "<" . showString (showHash h)
showsEither (Right h) = showString ">" . showString (showHash h)

showsMerklePath :: MerklePath -> ShowS
showsMerklePath = foldl (.) id . map showsEither 

showMerklePath :: MerklePath -> String
showMerklePath p = showsMerklePath p ""

instance Show a => Show (MerkleProof a) where
  showsPrec d (MerkleProof element path) =
    showParen (d > 10) $
      showString "MerkleProof "
        . showsPrec 11 element
        . showString " "
        . showsMerklePath path

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof hToCheck (MerkleProof a proof) = foldr g (hash a) proof == hToCheck
  where
    g (Left h) hacc = hash (hacc, h)
    g (Right h) hacc = hash (h, hacc)
