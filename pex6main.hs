-- pex6.hs 
-- unKnot Haskell

-- name: Jarod Chan

{- DOCUMENTATION: C2C Tanner Woodring noted that instead of having to use forbidden functions
   such as 'take' in order to extract a value from the tail, I could use head(tail). I also used
   modified versions of the listAfterKey and listLength functions from Homework 6 as helper
   functions.
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | typeIknot tripCode /= tripCode = unKnot (typeIknot tripCode)
   | typeIknot (wrap tripCode) /= (wrap tripCode) = unKnot (typeIknot(wrap tripCode))
   | typeIIknot tripCode /= tripCode = unKnot (typeIIknot tripCode)
   | typeIIknot (wrap tripCode) /= (wrap tripCode) = unKnot (typeIIknot(wrap tripCode))
   | typeIIknot (wrap (wrap tripCode)) /= (wrap (wrap tripCode)) = unKnot (typeIIknot(wrap (wrap tripCode)))
   | typeIIknot (wrap (wrap (wrap tripCode))) /= (wrap (wrap (wrap tripCode))) = unKnot (typeIIknot(wrap (wrap (wrap tripCode))))
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

typeIknot :: [(Char, Char)] -> [(Char, Char)]
typeIknot tripCode = if null tripCode || (listLength tripCode) <= 1 
   then tripCode
   else if fst(head tripCode) == fst(head(tail tripCode))
      then tail(tail tripCode)
      else [(head tripCode)] ++ typeIknot (tail tripCode)

typeIIknot :: [(Char, Char)] -> [(Char, Char)]
typeIIknot tripCode = if null tripCode || (listLength tripCode) <= 2
   then tripCode
   else if snd(head tripCode) == snd(head(tail tripCode))
      then if fst(head(nullSafeListAfterKey (fst(head tripCode)) (tail tripCode))) == fst(head(tail tripCode))
         then returnUntilKey (fst(head tripCode)) (tail(tail tripCode)) ++ listAfterKey (fst(head(tail tripCode))) (tail(tail tripCode))
         else [(head tripCode)] ++ typeIIknot (tail tripCode)
      else [(head tripCode)] ++ typeIIknot (tail tripCode)
         
nullSafeListAfterKey :: Char -> [(Char, Char)] -> [(Char, Char)]
nullSafeListAfterKey key list = if null list || (listLength list) <= 1
   then [('.', '.')]
   else if (key == fst(head list))
      then (tail list)
      else nullSafeListAfterKey key (tail list)

listAfterKey :: Char -> [(Char, Char)] -> [(Char, Char)]
listAfterKey key list = if null list
   then []
   else if (key == fst(head list))
      then (tail list)
      else listAfterKey key (tail list)

returnUntilKey :: Char -> [(Char, Char)] -> [(Char, Char)]
returnUntilKey key list = if null list
   then []
   else if (key == fst(head list))
      then []
      else [(head list)] ++ returnUntilKey key (tail list)

listLength :: [(Char, Char)] -> Int
listLength list = if null list
   then 0
   else 1 + listLength(tail list)

wrap :: [(Char, Char)] -> [(Char, Char)]
wrap list = if null list
   then []
   else (tail list) ++ [(head list)]


main :: IO ()
main = do
   let t01 = [('a','o'), ('e','u'), ('f','o'), ('g','o'), ('g','u'), ('b','u'), ('a','u'), ('f','u'), ('c','o'), ('d','o'), ('b','o'), ('c','u'), ('d','u'), ('e','o')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

   let t02 = [('a','o'), ('b','o'), ('b','u'), ('c','o'), ('d','o'), ('c','u'), ('a','u'), ('d','u')]
   print("   test case t02 - tripcode: " )
   print(t02)
   print("   result:" ++ unKnot t02)

   let t03 = [('a','o'), ('b','u'), ('b','o'), ('c','u'), ('d','u'), ('c','o'), ('d','o'), ('a','u')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03)

   let t04 = [('f','u'), ('a','o'), ('b','o'), ('e','u'), ('b','u'), ('e','o'), ('c','o'), ('d','o'), ('c','u'), ('d','u'), ('a','u'), ('f','o')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnot t04)

   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnot t05)

   let t06 = [('c','o'),('a','u'),('f','o'),('e','o'),('d','u'),('c','u'),('b','u'),('a','o'),('b','o'),('f','u'),('e','u'),('d','o')]
   print("   test case t06 - tripcode: " )
   print(t06)
   print("   result:" ++ unKnot t06)  