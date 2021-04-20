{-# LANGUAGE DeriveGeneric , GADTs , OverloadedStrings , FlexibleContexts , FlexibleInstances , TypeFamilies , TypeApplications , DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving , TypeSynonymInstances , MultiParamTypeClasses #-}
{-#  LANGUAGE ImpredicativeTypes , NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Learnbeam where
import Database.Beam
import Database.PostgreSQL.Simple
import Database.Beam.Postgres
import Database.Beam.Sqlite
import GHC.Int
import qualified Data.Vector as V

import Data.Text (Text)
-- import Lens.Micro
import Control.Lens

import Database.SQLite.Simple

-- mainInsert = do 
--     conn <- open "shoppingcart1.db"
--     runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
--         insert (_shoppingCartUsers shoppingCartDb) $
--         insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
--                     , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
--                     , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
--     return ()



-- https://haskell-beam.github.io/beam/tutorials/tutorial1/

data UserT f
    = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving (Generic)


--https://williamyaoh.com/posts/2019-09-27-figuring-out-beam-migrations.html how to do this automatically
-- CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));
    
deriving instance Show User
deriving instance Eq User
instance Beamable UserT


type User = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UserId . _userEmail


data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT) }
                        deriving (Generic, Database be)



-- insertExamplePSQL = do 
--     conn <- connect defaultConnectInfo { connectPort = 5433}
--     runBeamPostgresDebug putStrLn {- for debug output -} conn $ runInsert $
--         insert (_transactions transactionDb) $
--         insertValues [  transactionExample {- james -}]


-- mainPSQL = do
--     conn <- connect defaultConnectInfo { connectPort = 5433}
--     file <- readFile "src/someCSVexample.csv" 
--     let parsed = case (decode NoHeader file :: Either String (V.Vector TransactionCSV)) of 
--                         (Right values) -> toList values
--                         -- if something goes wrong, return an empty list to shorcircuit the following logic
--                         (Left err) ->  [] 
--     -- conn <- open "transactionDb1.db"
--     let converted = take 1000 $ map convertTransaction parsed
--     runBeamPostgres conn $ runInsert $
--         insert (_transactions transactionDb) $
--         insertValues converted
--     return ()

-- mainInsert = do 
--     conn <- open "shoppingcart2.db"
--     runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
--         insert (_shoppingCartUsers shoppingCartDb) $
--         insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
--                     , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
--                     , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
--     return ()


addMoreUseres = do 
    conn <- open "shoppingcart2.db"
    runBeamSqliteDebug putStrLn conn $
        runInsert $
        insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                    , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

data AddressT f = Address
                { _addressId    :: C f Int32
                , _addressLine1 :: C f Text
                , _addressLine2 :: C f (Maybe Text)
                , _addressCity  :: C f Text
                , _addressState :: C f Text
                , _addressZip   :: C f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving (Generic, Beamable)
type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

-- https://wiki.haskell.org/Monomorphism_restriction

--  • Couldn't match type ‘QGenExpr ctxt0 be2 s0 a0’ with ‘Int32’
--       Expected type: C Identity Int32
--         Actual type: QGenExpr ctxt0 be2 s0 a0
--     • In the first argument of ‘Address’, namely ‘(default_)’

-- These lens generating functions are awesome but if you use them in a compiled Haskell module (rather than GHC), 
-- GHC may give you odd compile errors about ambiguous types. These occur due to what's known as the monomorphism restriction.
--  You can turn it off using the NoMonomorphismRestriction extension.

-- The monomorphism restriction is part of the Haskell standard, but there has been talk about removing it in future language versions.
-- Basically, it requires GHC to not automatically infer polymorphic types for global definitions.
--  In this case though, polymorphic global definitions is exactly what we want.

workingWithRelations = do 
    conn <- open "shoppingcart2.db"
    let james = User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
        betty = User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
        sam = User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ james, betty, sam ]
    let addresses = [ Address ( default_ ) (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (pk james)
                , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (pk betty)
                , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (pk betty) ]
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_shoppingCartUserAddresses shoppingCartDb) $
        insertExpressions addresses


shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddresses =
                     setEntityName "addresses" <>
                     modifyTableFields
                       tableModification {
                         _addressLine1 = fieldNamed "address1",
                         _addressLine2 = fieldNamed "address2"
                       }
                 }


-- notice ->
-- A) because of overloaded strings, what looks like strings are actually text
-- B) we need to an explicit type signature
myUser = User "john@example.com" "John" "Smith" "password!" :: User
-- or with type application 
myUser' = User @Identity "john@example.com" "John" "Smith" "password!"


checkDb = do 
    conn <- open "shoppingcart1.db"
    let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
    runBeamSqliteDebug putStrLn conn $ do
        users <- runSelectReturningList $ select allUsers
        mapM_ (liftIO . putStrLn . show) users

getOrdered = do 
    conn <- open "shoppingcart1.db"
    let sortUsersByFirstName = orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) (all_ (_shoppingCartUsers shoppingCartDb))
    runBeamSqliteDebug putStrLn conn $ do
        users <- runSelectReturningList $ select sortUsersByFirstName
        mapM_ (liftIO . putStrLn . show) users

takeAndDropEquivalent = do 
    conn <- open "shoppingcart1.db"
    let boundedQuery = limit_ 1 $ offset_ 1 $
                   orderBy_ (asc_ . _userFirstName) $
                   all_ (_shoppingCartUsers shoppingCartDb)
    runBeamSqliteDebug putStrLn conn $ do
        users <- runSelectReturningList (select boundedQuery)
        mapM_ (liftIO . putStrLn . show) users


countUsers = do
    conn <- open "shoppingcart1.db"
    let userCount = aggregate_ (\u -> as_ @GHC.Int.Int32 countAll_) (all_ (_shoppingCartUsers shoppingCartDb))
    runBeamSqliteDebug putStrLn conn $ do
        Just c <- runSelectReturningOne $ select userCount
        liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")


usersByName = do 
    conn <- open "shoppingcart1.db"
    let numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int32 countAll_)) $
                          all_ (_shoppingCartUsers shoppingCartDb)
    runBeamSqliteDebug putStrLn conn $ do
        countedByName <- runSelectReturningList $ select numberOfUsersByName
        mapM_ (liftIO . putStrLn . show) countedByName


-- https://haskell-beam.github.io/beam/tutorials/tutorial2/

-- Above, we used the C constructor instead of Columnar for each column. C is a type synonym for Columnar,
--  and some find it reduces the syntactic overhead of model declaration.



instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int32) deriving (Generic, Beamable)
    primaryKey = AddressId . _addressId
type AddressId = PrimaryKey AddressT Identity -- For convenience

Address (LensFor addressId)    (LensFor addressLine1)
        (LensFor addressLine2) (LensFor addressCity)
        (LensFor addressState) (LensFor addressZip)
        (UserId (LensFor addressForUserId)) =
        tableLenses

-- User (LensFor userEmail)    (LensFor userFirstName)
--      (LensFor userLastName) (LensFor userPassword) =
--      tableLenses

-- ShoppingCartDb (TableLens shoppingCartUsers)
--                (TableLens shoppingCartUserAddresses) =
--                dbLenses

-- getAColum = do
--     conn <- open "shoppingcart2.db"
--     addresses <- runBeamSqliteDebug putStrLn conn $
--                 runSelectReturningList $
--                 select (all_ (shoppingCartDb ^. shoppingCartUserAddresses))
--     mapM_ print addresses

myexample :: String
myexample = "hello world"