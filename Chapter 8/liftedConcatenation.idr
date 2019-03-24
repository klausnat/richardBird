infixr 5 <>
infixr 5 <++>

Layout : Type
Layout = String

Doc : Type
Doc = List Layout

%hint
nil : Doc 
nil = [""]

line : Doc
line = ["\n"]

text : String -> Doc 
text s = [s]

(<++>) : Doc -> Doc -> Doc
(<++>) xss yss = [xs ++ ys | xs <- xss, ys <- yss]

(<>) : Doc -> Doc -> Doc
(<>) x y = x <++> y

flattenl : Layout -> Layout
flattenl "" = ""
flattenl sx = pack $ concat (delIfNewLine chars) 
                      where chars = unpack sx
                            delIfNewLine : List Char -> List (List Char)
                            delIfNewLine [] = []
                            delIfNewLine (c :: cs) = case c of '\n' => delIfNewLine (dropWhile ( == ' ') cs)
                                                               _ => [c] :: delIfNewLine cs
nestl : Int -> Layout -> Layout
nestl i lt = pack (concat $ map (indent i) (unpack lt)) where
                                  indent : Int -> Char -> List Char
                                  indent n c = case c of '\n' => c :: replicate (cast n) ' '
                                                         _    => [c]

nest : Int -> Doc -> Doc
nest i = map (nestl i)

flatten : Doc -> Doc
flatten [] = nil
flatten (x :: xs) = flattenl x :: []

-- adds at the head of list of layouts one with no linebreaks
group : Doc -> Doc
group x = flatten x ++ x

layouts : Doc -> Doc
layouts = id

shape : Layout -> List Int
shape lx = map (cast . length) $ lines lx

doc1 : Doc
doc1 = group (text "Hello " <> line <> text "World!")

doc2 : Doc
doc2 = group $ text "11" <> line <> text "22"

data CExpr = Expr String | If String CExpr CExpr

ex1 : CExpr
ex1 = If "Wealthy " (If "happy " (Expr "lucky you ") (Expr "tough ")) 
                   (If "in love " (Expr "content ") (Expr "miserable "))

cexpr : CExpr -> Doc
cexpr (Expr x) = text x
cexpr (If str x y) = group (text "if " <> text str <> line <> 
                     group ( text "then " <> nest 5 (cexpr x) <> line <> text "else " <> nest 5 (cexpr y)))


pretty : Int -> Doc -> Layout
pretty w = fst . foldr1 choose . map augment where
           augment : Layout -> (Layout, List Int)
           augment lx = (lx, shape lx)
           better : List Int -> List Int -> Bool
           better [] ys = True
           better xs [] = False
           better (i :: is) (j :: js) = case (i == j) of True => better is js
                                                         False => (i <= w)
           
           choose : (Layout, List Int) -> (Layout, List Int) -> (Layout, List Int)
           choose (l1,ns) (l2,ms) = if better ns ms then (l1,ns) else (l2,ms)
           

{-
Foldable Doc where

cvt : List Doc -> Doc 
cvt [] = nil
cvt (y :: ys) = y <> (foldr (<>) nil zs) where 
                    zs = [group (line <> x) | x <- ys]

para : String -> Doc
para = cvt . map text . words
-}
