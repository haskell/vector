{-# LANGUAGE ParallelListComp #-}
module Main where

import Text.PrettyPrint

import System.Environment ( getArgs )

main = do
         [s] <- getArgs
         let n = read s
         mapM_ (putStrLn . render . generate) [2..n]

generate :: Int -> Doc
generate n =
  vcat [ data_instance "MVector s" "MV"
       , data_instance "Vector" "V"
       , class_instance "Unbox"
       , class_instance "M.MVector MVector" <+> text "where"
       , nest 2 $ vcat $ map method methods_MVector
       , class_instance "G.Vector Vector" <+> text "where"
       , nest 2 $ vcat $ map method methods_Vector
       ]

  where
    vars  = map char $ take n ['a'..]
    varss = map (<> char 's') vars
    tuple f = parens $ hsep $ punctuate comma $ map f vars
    vtuple f = parens $ sep $ punctuate comma $ map f vars
    con s = text s <> char '_' <> int n

    data_instance ty c
      = hang (hsep [text "data instance", text ty, tuple id])
             4
             (hsep [char '=', con c, text "{-# UNPACK #-} !Int"
                   , vcat $ map (\v -> parens (text ty <+> v)) vars])

    class_instance cls
      = text "instance" <+> vtuple (text "Unbox" <+>)
                        <+> text "=>" <+> text cls <+> tuple id


    pat c = parens $ con c <+> char 'n' <+> sep varss
    patn c n = parens $ con c <+> (char 'n' <> int n)
                              <+> sep [v <> int n | v <- varss]

    gen_length c = (pat c, char 'n')

    gen_unsafeSlice mod c
      = (pat c <+> char 'i' <+> char 'm',
         con c <+> char 'm'
               <+> vcat [parens $ text mod <> char '.' <> text "unsafeSlice"
                                  <+> vs <+> char 'i' <+> char 'm'
                                        | vs <- varss])


    gen_overlaps = (patn "MV" 1 <+> patn "MV" 2,
                    vcat $ r : [text "||" <+> r | r <- rs])
      where
        r : rs = [text "M.overlaps" <+> v <> char '1' <+> v <> char '2'
                        | v <- vars]

    gen_unsafeNew
      = (char 'n',
         mk_do [v <+> text "<- M.unsafeNew n" | v <- varss]
               $ text "return $" <+> con "MV" <+> sep varss)

    gen_unsafeNewWith
      = (char 'n' <+> tuple id,
         mk_do [vs <+> text "<- M.unsafeNewWith n" <+> v | v  <- vars
                                                         | vs <- varss]
               $ text "return $" <+> con "MV" <+> sep varss)

    gen_unsafeRead
      = (pat "MV" <+> char 'i',
         mk_do [v <+> text "<- M.unsafeRead" <+> vs <+> char 'i' | v  <- vars
                                                                 | vs <- varss]
               $ text "return" <+> tuple id)

    gen_unsafeWrite
      = (pat "MV" <+> char 'i' <+> tuple id,
         mk_do [text "M.unsafeWrite" <+> vs <+> char 'i' <+> v | v  <- vars
                                                               | vs <- varss]
               empty)

    gen_clear
      = (pat "MV", mk_do [text "M.clear" <+> vs | vs <- varss] empty)

    gen_set
      = (pat "MV" <+> tuple id,
         mk_do [text "M.set" <+> vs <+> v | vs <- varss | v <- vars] empty)

    gen_unsafeCopy
      = (patn "MV" 1 <+> patn "MV" 2,
         mk_do [text "M.unsafeCopy" <+> vs <> char '1' <+> vs <> char '2'
                        | vs <- varss] empty)

    gen_unsafeGrow
      = (pat "MV" <+> char 'm',
         mk_do [text "M.unsafeGrow" <+> vs <+> char 'm' | vs <- varss]
               $ text "return $" <+> con "MV" <+> text "(m+n)"
                                 <+> sep varss)

    gen_unsafeFreeze
      = (pat "MV",
         mk_do [vs <> char '\'' <+> text "<- G.unsafeFreeze" <+> vs
                        | vs <- varss]
               $ text "return $" <+> con "V" <+> char 'n'
                                 <+> sep [vs <> char '\'' | vs <- varss])

    gen_basicUnsafeIndexM
      = (pat "V" <+> char 'i',
         mk_do [v <+> text "<- G.basicUnsafeIndexM" <+> vs <+> char 'i'
                        | vs <- varss | v <- vars]
               $ text "return" <+> tuple id)

    
         

    mk_do cmds ret = hang (text "do")
                          2
                          $ vcat $ cmds ++ [ret]

    method (s, (p,e)) = text "{-# INLINE" <+> text s <+> text " #-}"
                     $$ hang (text s <+> p)
                             4
                             (char '=' <+> e)
                             

    methods_MVector = [("length",            gen_length "MV")
                      ,("unsafeSlice",       gen_unsafeSlice "M" "MV")
                      ,("overlaps",          gen_overlaps)
                      ,("unsafeNew",         gen_unsafeNew)
                      ,("unsafeNewWith",     gen_unsafeNewWith)
                      ,("unsafeRead",        gen_unsafeRead)
                      ,("unsafeWrite",       gen_unsafeWrite)
                      ,("clear",             gen_clear)
                      ,("set",               gen_set)
                      ,("unsafeCopy",        gen_unsafeCopy)
                      ,("unsafeGrow",        gen_unsafeGrow)]

    methods_Vector  = [("unsafeFreeze",      gen_unsafeFreeze)
                      ,("basicLength",       gen_length "V")
                      ,("unsafeSlice",       gen_unsafeSlice "G" "V")
                      ,("basicUnsafeIndexM", gen_basicUnsafeIndexM)]
