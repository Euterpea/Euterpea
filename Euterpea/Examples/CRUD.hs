-- Filename: crud.hs
-- Created by: Daniel Winograd-Cort
-- Created on: 11/21/2012
-- Last Modified by: Daniel Winograd-Cort
-- Last Modified on: 12/15/2012

-- -- DESCRIPTION --
-- This code was inspired by a blog post by Heinrich Apfelmus on 
-- bidirectional data flow in GUIs:
-- http://apfelmus.nfshost.com/blog/2012/03/29-frp-three-principles-bidirectional-gui.html
-- 
-- Here we use the GUI from Euterpea (a.k.a. MUI) to create a similar 
-- example using arrowized FRP.


{-# LANGUAGE Arrows, DoRec #-}
module Crud where
import Euterpea

import Data.List (isInfixOf)
import Data.Char (toLower)


-- First we create types for the database and the entries for it

type Database a = [a]
data NameEntry = NameEntry {firstName :: String, lastName :: String}

instance Show NameEntry where
    show (NameEntry f l) = l ++ ", " ++ f

instance Eq NameEntry where
    (NameEntry f1 l1) == (NameEntry f2 l2) = f1 == f2 && l1 == l2


-- defaultnames is a default database for our example
defaultnames :: Database NameEntry
defaultnames = [
    NameEntry "Paul" "Hudak",
    NameEntry "Dan" "Winograd-Cort",
    NameEntry "Donya" "Quick"]


-- | This function will run the crud GUI with the default names.
crud = runUIEx (350, 400) "CRUD" (crudUISF defaultnames)
main = crud

-- | This is the main function that creates the crud GUI.  It takes an 
--   initial database of names as an argument.
--   See notes below on the use of banana brackets and nested do blocks.
crudUISF :: Database NameEntry -> UISF () ()
crudUISF initnamesDB = proc _ -> do
  rec
    fStr <- leftRight $ label "Filter text: " >>> textboxE "" -< Nothing
    (i, db, fdb, nameStr, surnStr) <- (| leftRight (do
        (i, db, fdb) <- (| topDown (do
            rec i <- listbox -< (fdb, i')
                db <- delay initnamesDB -< db'
                let fdb = filter (filterFun fStr) db
            returnA -< (i, db, fdb)) |)
        (nameStr, surnStr) <- (| topDown (do
            rec nameStr <- leftRight $ label "Name:    " >>> textboxE "" -< nameStr'
                surnStr <- leftRight $ label "Surname: " >>> textboxE "" -< surnStr'
                let nameStr' = if previ == i then Nothing else Just $ firstName ((filter (filterFun fStr) db') `at` i')
                    surnStr' = if previ == i then Nothing else Just $ lastName  ((filter (filterFun fStr) db') `at` i')
            returnA -< (nameStr, surnStr)) |)
        returnA -< (i, db, fdb, nameStr, surnStr)) |)
    buttons <- leftRight $ (edge <<< button "Create") &&& 
                           (edge <<< button "Delete") -< ()
    previ <- delay 0 -< i
    let (db', i') = case buttons of
            (Just _, Nothing) -> (db ++ [NameEntry nameStr surnStr], length fdb)
            (Nothing, Just _) -> (deleteElem (filterFun fStr) i db,
                              if i == length fdb - 1 then length fdb - 2 else i)
            _ -> (db, i)
  returnA -< ()
  where
    deleteElem _ _ [] = []
    deleteElem f i (x:xs) = case (f x, i == 0) of
        (True, True)    -> xs
        (True, False)   -> x:deleteElem f (i-1) xs
        (False, _)      -> x:deleteElem f i xs
    filterFun str name = and (map (\s -> isInfixOf s (map toLower $ show name)) (words (map toLower str)))
    lst `at` index = if index >= length lst || index < 0 then NameEntry "" "" else lst!!index


-- If we don't care about formatting, this code simplifies a huge amount to:
-- crudUISF initnamesDB = proc _ -> do
--   rec
--     (fStr,fi) <- leftRight $ label "Filter text: " >>> cursoredTextbox False ("",0) -< (fStr,fi)
--     i <- listbox -< (fdb, i')
--     db <- delay initnamesDB -< db'
--     let fdb = filter (filterFun fStr) db
--     (nameStr, ni) <- leftRight $ label "Name:    " >>> cursoredTextbox False "" -< (nameStr', ni)
--     (surnStr, si) <- leftRight $ label "Surname: " >>> cursoredTextbox False "" -< (surnStr', si)
--     let nameStr' = if previ == i' then nameStr else firstName ((filter (filterFun fStr) db') `at` i')
--         surnStr' = if previ == i' then surnStr else lastName ((filter (filterFun fStr) db') `at` i')
--     buttons <- leftRight $ (edge <<< button "Create") &&& 
--                            (edge <<< button "Delete") -< ()
--     previ <- delay 0 -< i
--     let (db', i') = case buttons of
--             (True, False) -> (db ++ [NameEntry nameStr surnStr], length fdb)
--             (False, True) -> (deleteElem (filterFun fStr) i db,
--                               if i == length fdb - 1 then length fdb - 2 else i)
--             _ -> (db, i)
--   returnA -< ()
--   where
--     ...
-- 
-- Clearly, this is much easier to read and clearer as to what is going on. 
-- However, to keep the style entirely arrow-based, we are forced to inject 
-- arrow transformers (here leftRight and topDown) to modify chunks of the 
-- code.  The banana brackets (| |) allow us to refrain from retyping the 
-- "proc do" syntax, but in order to give other parts of the program access 
-- to the variables created in the banana bracketed chunks, we require 
-- extra (seemingly excessive) returnA commands at the end of each.


