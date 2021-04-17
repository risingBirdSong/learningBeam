{-# LANGUAGE DeriveGeneric , GADTs , OverloadedStrings , FlexibleContexts , FlexibleInstances , TypeFamilies , TypeApplications , DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving , TypeSynonymInstances , MultiParamTypeClasses #-}
module Learnbeam where
import Database.Beam
import Database.Beam.Sqlite
import GHC.Int
import Data.Text (Text)
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
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving (Generic, Database be)



mainInsert = do 
    conn <- open "shoppingcart1.db"
    runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
        insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
    return ()

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

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


addMoreUseres = do 
    conn <- open "shoppingcart1.db"
    runBeamSqliteDebug putStrLn conn $
        runInsert $
        insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                    , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]


usersByName = do 
    conn <- open "shoppingcart1.db"
    let numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int32 countAll_)) $
                          all_ (_shoppingCartUsers shoppingCartDb)
    runBeamSqliteDebug putStrLn conn $ do
    countedByName <- runSelectReturningList $ select numberOfUsersByName
    mapM_ (liftIO . putStrLn . show) countedByName