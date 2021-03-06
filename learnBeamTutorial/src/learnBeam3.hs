
{-# LANGUAGE DeriveGeneric , GADTs , OverloadedStrings , FlexibleContexts , FlexibleInstances , TypeFamilies , TypeApplications , DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving , TypeSynonymInstances , MultiParamTypeClasses #-}
{-#  LANGUAGE ImpredicativeTypes , NoMonomorphismRestriction, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}


{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Learnbeam where
import Database.Beam
-- import Database.PostgreSQL.Simple
import Database.Beam.Postgres
import Database.Beam.Sqlite
import GHC.Int
import Database.Beam.Backend.SQL

import Data.Char

import qualified Data.Vector as V

import Data.Text
-- import Lens.Micro
import Control.Lens
import Data.Time


import Database.SQLite.Simple



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


data ProductT f = Product
                { _productId          :: C f Int32
                , _productTitle       :: C f Text
                , _productDescription :: C f Text
                , _productPrice       :: C f Int32 {- Price in cents -} }
                  deriving (Generic, Beamable)
type Product = ProductT Identity
deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int32)
                               deriving (Generic, Beamable)
  primaryKey = ProductId . _productId

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




instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int32) deriving (Generic, Beamable)
    primaryKey = AddressId . _addressId
type AddressId = PrimaryKey AddressT Identity -- For convenience

Address (LensFor addressId)    (LensFor addressLine1)
        (LensFor addressLine2) (LensFor addressCity)
        (LensFor addressState) (LensFor addressZip)
        (UserId (LensFor addressForUserId)) =
        tableLenses

User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userPassword) =
     tableLenses


deriving instance Show (PrimaryKey AddressT Identity)

data OrderT f = Order
              { _orderId      :: Columnar f Int32
              , _orderDate    :: Columnar f LocalTime
              , _orderForUser :: PrimaryKey UserT f
              , _orderShipToAddress :: PrimaryKey AddressT f
              , _orderShippingInfo :: PrimaryKey ShippingInfoT (Nullable f) }
                deriving (Generic, Beamable)
type Order = OrderT Identity
deriving instance Show Order

instance Table OrderT where
    data PrimaryKey OrderT f = OrderId (Columnar f Int32)
                               deriving (Generic, Beamable)
    primaryKey = OrderId . _orderId

data ShippingCarrier = USPS | FedEx | UPS | DHL
                       deriving (Show, Read, Eq, Ord, Enum)

data ShippingInfoT f = ShippingInfo
                     { _shippingInfoId             :: Columnar f Int32
                     , _shippingInfoCarrier        :: Columnar f ShippingCarrier
                     , _shippingInfoTrackingNumber :: Columnar f Text }
                       deriving (Generic, Beamable)
type ShippingInfo = ShippingInfoT Identity
deriving instance Show ShippingInfo

instance Table ShippingInfoT where
    data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f Int32)
                                      deriving (Generic, Beamable)
    primaryKey = ShippingInfoId . _shippingInfoId

deriving instance Show (PrimaryKey ShippingInfoT (Nullable Identity))


deriving instance Show (PrimaryKey OrderT Identity)
deriving instance Show (PrimaryKey ProductT Identity)

data LineItemT f = LineItem
                 { _lineItemInOrder    :: PrimaryKey OrderT f
                 , _lineItemForProduct :: PrimaryKey ProductT f
                 , _lineItemQuantity   :: Columnar f Int32 }
                   deriving (Generic, Beamable)
type LineItem = LineItemT Identity
deriving instance Show LineItem

instance Table LineItemT where
    data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
                                  deriving (Generic, Beamable)
    primaryKey = LineItemId <$> _lineItemInOrder <*> _lineItemForProduct


-- Some convenience lenses

LineItem _ _ (LensFor lineItemQuantity) = tableLenses
Product (LensFor productId) (LensFor productTitle) (LensFor productDescription) (LensFor productPrice) = tableLenses

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT)
                      , _shoppingCartProducts      :: f (TableEntity ProductT)
                      , _shoppingCartOrders        :: f (TableEntity OrderT)
                      , _shoppingCartShippingInfos :: f (TableEntity ShippingInfoT)
                      , _shoppingCartLineItems     :: f (TableEntity LineItemT) }
                        deriving (Generic, Database be)

ShoppingCartDb (TableLens shoppingCartUsers) (TableLens shoppingCartUserAddresses)
               (TableLens shoppingCartProducts) (TableLens shoppingCartOrders)
               (TableLens shoppingCartShippingInfos) (TableLens shoppingCartLineItems) = dbLenses

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddresses =
                     setEntityName "addresses" <>
                     modifyTableFields tableModification {
                       _addressLine1 = "address1",
                       _addressLine2 = "address2"
                     },
                   _shoppingCartProducts = setEntityName "products",
                   _shoppingCartOrders = setEntityName "orders" <>
                                         modifyTableFields tableModification {
                                           _orderShippingInfo = ShippingInfoId "shipping_info__id"
                                         },
                   _shoppingCartShippingInfos = setEntityName "shipping_info" <>
                                                modifyTableFields tableModification {
                                                  _shippingInfoId = "id",
                                                  _shippingInfoCarrier = "carrier",
                                                  _shippingInfoTrackingNumber = "tracking_number"
                                                },
                   _shoppingCartLineItems = setEntityName "line_items"
                 }

executeNewTables = do 
    conn <- open "shoppingcart3.db"
    execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
    execute_ conn "CREATE TABLE addresses ( id INTEGER PRIMARY KEY AUTOINCREMENT, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );"
    execute_ conn "CREATE TABLE products ( id INTEGER PRIMARY KEY AUTOINCREMENT, title VARCHAR NOT NULL, description VARCHAR NOT NULL, price INT NOT NULL );"
    execute_ conn "CREATE TABLE orders ( id INTEGER PRIMARY KEY AUTOINCREMENT, date TIMESTAMP NOT NULL, for_user__email VARCHAR NOT NULL, ship_to_address__id INT NOT NULL, shipping_info__id INT);"
    execute_ conn "CREATE TABLE shipping_info ( id INTEGER PRIMARY KEY AUTOINCREMENT, carrier VARCHAR NOT NULL, tracking_number VARCHAR NOT NULL);"
    execute_ conn "CREATE TABLE line_items (item_in_order__id INTEGER NOT NULL, item_for_product__id INTEGER NOT NULL, item_quantity INTEGER NOT NULL)"
    return ()

insertA = do 
    conn <- open "shoppingcart3.db"
    let users@[james, betty, sam] =
          [ User "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c" {- james -}
          , User "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
          , User "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
    let addresses = [ Address default_ (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (pk james)
                , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (pk betty)
                , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (pk betty) ]

    let products = [ Product default_ (val_ "Red Ball") (val_ "A bright red, very spherical ball") (val_ 1000)
               , Product default_ (val_ "Math Textbook") (val_ "Contains a lot of important math theorems and formulae") (val_ 2500)
               , Product default_ (val_ "Intro to Haskell") (val_ "Learn the best programming language in the world") (val_ 3000)
               , Product default_ (val_ "Suitcase") "A hard durable suitcase" 15000 ]

    bettyShippingInfo <- runBeamSqliteDebug putStrLn conn $ do
        [bettyShippingInfo] <- runInsertReturningList $
            insertReturning (shoppingCartDb ^. shoppingCartShippingInfos) $
            insertExpressions [ ShippingInfo default_ (val_ USPS) (val_ "12345790ABCDEFGHI") ]
        pure bettyShippingInfo

    (jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase) <- runBeamSqliteDebug putStrLn conn $ do
            runInsert $ insert (shoppingCartDb ^. shoppingCartUsers) $
                        insertValues users

            [jamesAddress1, bettyAddress1, bettyAddress2] <-
                runInsertReturningList $
                insertReturning (shoppingCartDb ^. shoppingCartUserAddresses) $ insertExpressions addresses

            [redBall, mathTextbook, introToHaskell, suitcase] <-
                runInsertReturningList $
                insertReturning (shoppingCartDb ^. shoppingCartProducts) $ insertExpressions products
            pure ( jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase )
    
    [ jamesOrder1, bettyOrder1, jamesOrder2 ] <- runBeamSqliteDebug putStrLn conn $ do
        runInsertReturningList $ insertReturning (shoppingCartDb ^. shoppingCartOrders) $
            insertExpressions $
                [ Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
                , Order default_ currentTimestamp_ (val_ (pk betty)) (val_ (pk bettyAddress1)) (just_ (val_ (pk bettyShippingInfo)))
                , Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_ ]


    let lineItems = [ LineItem (pk jamesOrder1) (pk redBall) 10
                  , LineItem (pk jamesOrder1) (pk mathTextbook) 1
                  , LineItem (pk jamesOrder1) (pk introToHaskell) 4

                  , LineItem (pk bettyOrder1) (pk mathTextbook) 3
                  , LineItem (pk bettyOrder1) (pk introToHaskell) 3

                  , LineItem (pk jamesOrder2) (pk mathTextbook) 1 ]

    runBeamSqliteDebug putStrLn conn $ do
        runInsert $ insert (shoppingCartDb ^. shoppingCartLineItems) $
          insertValues lineItems
        return ()
    

-- Marshalling a custom type

bettyShippingInfoA = do
    conn <- open "shoppingcart3.db"

    return ()

-- ### expected error with bettyShippingInfoA HasSqlValueSyntax before we include HasSqlValueSyntax be ShippingCarrier instance ###

-- <interactive>:845:7: error:
--     ??? No instance for (FromBackendRow Sqlite ShippingCarrier)
--         arising from a use of ???runInsertReturningList???
--     ??? In a stmt of a 'do' block:
--         [bettyShippingInfo] <- runInsertReturningList
--                                  $ insertReturning (shoppingCartDb ^. shoppingCartShippingInfos)
--                                      $ insertExpressions
--                                          [ShippingInfo
--                                             default_ (val_ USPS) (val_ "12345790ABCDEFGHI")]
-- ...

-- <interactive>:847:50: error:
--     ??? No instance for (Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
--                          Database.Beam.Sqlite.Syntax.SqliteValueSyntax ShippingCarrier)



instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax



instance FromBackendRow Sqlite ShippingCarrier where
  fromBackendRow = read . unpack <$> fromBackendRow

-- insertB = do
  
--     return ()
-- print jamesOrder1
-- print bettyOrder1
-- print jamesOrder2

-- deleting 


-- ??? Found hole:
--         _ :: (forall s'. UserT (QExpr Sqlite s')) 
-- -> QExpr Sqlite s Bool

bettySelect =  (all_ (shoppingCartDb ^. shoppingCartUsers))
selectAll  =  (all_ (shoppingCartDb ^. shoppingCartUsers))


selectOneUser email = do 
    -- let bettyId = UserId "betty@example.com" :: UserId
    conn <- open "shoppingcart3.db"
    [user'] <- runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select $ do 
                users <- all_ (shoppingCartDb ^. shoppingCartUsers)
                guard_ (_userEmail users ==. val_ email) 
                pure users
    return user'

-- With values: [SQLText "betty@example.com"]
printUsersAddress email = do
    conn <- open "shoppingcart2.db"
    search <- selectOneUser (pack email)
    usersAndRelatedAddresses <-  runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $ select $ do
            user <- all_ (shoppingCartDb ^. shoppingCartUsers)
            address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
            guard_ (address ^. addressForUserId ==. user ^. userEmail)
            guard_ (user ^. userEmail ==. (val_ (search ^. userEmail)))
            pure (user, address)
    mapM_ print usersAndRelatedAddresses

getUserFirstAddress email = do 
    conn <- open "shoppingcart2.db"
    search <- selectOneUser (pack email)
    usersAndRelatedAddresses <-  runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $ select $ do
            user <- all_ (shoppingCartDb ^. shoppingCartUsers)
            address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
            guard_ (address ^. addressForUserId ==. user ^. userEmail)
            guard_ (user ^. userEmail ==. (val_ (search ^. userEmail)))
            pure (user, address)
    return (snd (Prelude.head usersAndRelatedAddresses))

getUserAndAddress email = do 
    conn <- open "shoppingcart2.db"
    search <- selectOneUser (pack email)
    usersAndRelatedAddresses <-  runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $ select $ do
            user <- all_ (shoppingCartDb ^. shoppingCartUsers)
            address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
            guard_ (address ^. addressForUserId ==. user ^. userEmail)
            guard_ (user ^. userEmail ==. (val_ (search ^. userEmail)))
            pure (user, address)
    return (Prelude.head usersAndRelatedAddresses)

getUserAndRelated email = do
  conn <- open "shoppingcart2.db"
  search <- selectOneUser (pack email)
  -- this pattern match fails! [stuff] <- runBeamSqliteDebug putStrLn conn $ runSelectReturningList $  select $ do
  stuff <- runBeamSqliteDebug putStrLn conn $ runSelectReturningList $  select $ do
    user' <- all_ (shoppingCartDb ^. shoppingCartUsers)
    address' <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
    -- product' <- all_ (shoppingCartDb ^. shoppingCartProducts)
    guard_ (user' ^. userEmail ==. (val_ (search ^. userEmail)))
    -- print address'
    return (user', address')
  return (stuff)

-- [User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},
-- User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"}]


  -- return ()
-- { _shoppingCartUsers         :: f (TableEntity UserT)
--                       , _shoppingCartUserAddresses :: f (TableEntity AddressT)
--                       , _shoppingCartProducts      :: f (TableEntity ProductT)
--                       , _shoppingCartOrders        :: f (TableEntity OrderT)
--                       , _shoppingCartShippingInfos :: f (TableEntity ShippingInfoT)
--                       , _shoppingCartLineItems     :: f (TableEntity LineItemT) }


-- selectOneAddress email = do 
--   conn <- open "shoppingcart3.db"
--   runBeamSqliteDebug putStrLn conn $ do
--       user' <- runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select $ do 
--             users <- all_ (shoppingCartDb ^. shoppingCartUsers)
--             guard_ (_userEmail users ==. val_ email) 
--       addresses <- all_ (shoppingCartDb ^. shoppingCartUserAddresses) 
--       guard_ (addresses ^. addressForUserId ==. user ^. userEmail)
--       return addresses



-- a good suggestion from the channel 
-- Never used beam before, but would const (val_ True) fit your hole?


deleteUsers = do
    conn <- open "shoppingcart3.db"
    runBeamSqliteDebug putStrLn conn $ runDelete $ 
      delete (shoppingCartDb ^. shoppingCartUsers) (const (val_ True))
    return ()

deleteAddresses = do 
  conn <- open "shoppingcart3.db"
  runBeamSqliteDebug putStrLn conn $ runDelete $ 
    delete (shoppingCartDb ^. shoppingCartUserAddresses) (const (val_ True))
  return ()
-- great and simple answer... i need to study forall

deleteProducts = do 
  conn <- open "shoppingcart3.db"
  runBeamSqliteDebug putStrLn conn $ runDelete $ 
    delete (shoppingCartDb ^. shoppingCartProducts) (const (val_ True))
  return ()

deleteOrders = do 
  conn <- open "shoppingcart3.db"
  runBeamSqliteDebug putStrLn conn $ runDelete $
    delete (shoppingCartDb ^. shoppingCartOrders) (const (val_ True))
  return ()

deleteShippingInfo = do
  conn <- open "shoppingcart3.db"
  runBeamSqliteDebug putStrLn conn $ runDelete $
    delete (shoppingCartDb ^. shoppingCartShippingInfos) (const (val_ True))
  return ()

deleteLineItems = do 
  conn <- open "shoppingcart3.db"
  runBeamSqliteDebug putStrLn conn $ runDelete $
    delete (shoppingCartDb ^. shoppingCartLineItems) (const (val_ True))
  return ()

deleteAll = mapM_ (id) [deleteUsers, deleteAddresses, deleteProducts, deleteOrders, deleteShippingInfo, deleteLineItems]

-- shoppingCartShippingInfos , shoppingCartLineItems

--great article on forall
-- https://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do

putInList x = [x]
-- liftTup :: (t -> b) -> (t, t) -> (b, b)
-- liftTup liftFunc (a, b) = (liftFunc a, liftFunc b)

-- liftTup putInList (1,"a")

-- <interactive>:4:20: error:
--     ??? No instance for (Num [Char]) arising from the literal ???1???
--     ??? In the expression: 1
--       In the second argument of ???liftTup???, namely ???(1, "a")???
--       In the expression: liftTup putInList (1, "a")

-- deleteAddresses

-- this is an example of rankNType use case which means x can be any suitable type
liftTup :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
liftTup liftFunc (t, v) = (liftFunc t, liftFunc v)

-- *Learnbeam> liftTup putInList (3,"a")
-- ([3],["a"])


-- getShippingInfo 


-- newOrders = do
--   conn <- open "shoppingcart3.db" 
--   (betty, bettyAddress1) <- getUserAndAddress "betty@example.com"
--   (james, jamesAddress1) <- getUserAndAddress "james@example.com"
--   -- [betty] <- runSelectReturningList $ selectOneUser "betty@example.com"
--   -- [james] <- runSelectReturningList $ selectOneUser "james@example.com"
--   [ jamesOrder1 ] <- runBeamSqliteDebug putStrLn conn $ do
--       -- where to put this?
--       -- [jamesAddress1] <- getUserFirstAddress (val_ (james ^. userEmail))
--       runInsertReturningList $
--         insertReturning (shoppingCartDb ^. shoppingCartOrders) $
--         insertExpressions $
--         [ Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) (nothing_ ) ]
--           -- , Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk _)) nothing_ ]
--           -- [ Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
--           -- , Order default_ currentTimestamp_ (val_ (pk betty)) (val_ (pk bettyAddress1)) (just_ (val_ (pk bettyShippingInfo)))
--           -- , Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_ ]
--   print jamesOrder1
-- --   print jamesOrder1
-- --   print bettyOrder1
-- --   print jamesOrder2


userAndOrders email = do
  conn <- open "shoppingcart3.db" 
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $ do
      user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
      guard_ (user ^. userEmail ==. (val_ (pack email)))
      order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
      pure (user, order)

usersAndOrders = do
  conn <- open "shoppingcart3.db" 
  runBeamSqlite conn $
    runSelectReturningList $
    select $ do
      user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
      -- `references_` user observation in the line below
      -- it in the order table, there are no direct references to User, the closest is reference to address, which directly 
      -- references User, so I think Beam is able to see this indirect relationship and do the right thing... it is interesting
      order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
      pure (user, order)

usersWithNoOrdersDo = do 
  conn <- open "shoppingcart3.db" 
  usersWithNoOrders <-
    runBeamSqlite conn $
      runSelectReturningList $
      select $ do
        user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
        order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
        guard_ (isNothing_ order)
        pure user
  mapM_ print usersWithNoOrders

ordersWithCostOrderedDo = do
  conn <- open "shoppingcart3.db" 
  ordersWithCostOrdered <- runBeamSqlite conn $
    runSelectReturningList $
    select $
    orderBy_ (\(order, total) -> desc_ total) $
    aggregate_ (\(order, lineItem, product) ->
                    (group_ order, sum_ (lineItem ^. lineItemQuantity * product ^. productPrice))) $
    do lineItem <- all_ (shoppingCartDb ^. shoppingCartLineItems)
       order   <- related_ (shoppingCartDb ^. shoppingCartOrders) (_lineItemInOrder lineItem)
       product <- related_ (shoppingCartDb ^. shoppingCartProducts) (_lineItemForProduct lineItem)
       pure (order, lineItem, product)
  mapM_ print ordersWithCostOrdered


allUsersAndTotalsDo = do
  conn <- open "shoppingcart3.db"  
  allUsersAndTotals <-
    runBeamSqlite conn $
      runSelectReturningList $
      select $
      orderBy_ (\(user, total) -> desc_ total) $
      aggregate_ (\(user, lineItem, product) ->
                    (group_ user, sum_ (maybe_ 0 id (_lineItemQuantity lineItem) * maybe_ 0 id (product ^. productPrice)))) $
      do  user     <- all_ (shoppingCartDb ^. shoppingCartUsers)
          order    <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                                (\order -> _orderForUser order `references_` user)
          lineItem <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartLineItems))
                                (\lineItem -> maybe_ (val_ False) (\order -> _lineItemInOrder lineItem `references_` order) order)
          product  <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartProducts))
                                (\product -> maybe_ (val_ False) (\lineItem -> _lineItemForProduct lineItem `references_` product) lineItem)
          pure (user, lineItem, product)

  mapM_ print allUsersAndTotals

-- more efficient 
allUsersAndTotals2Do = do
  conn <- open "shoppingcart3.db"   
  allUsersAndTotals2 <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
      select $
      orderBy_ (\(user, total) -> desc_ total) $
      aggregate_ (\(user, lineItem, product) ->
                    (group_ user, sum_ (maybe_ 0 id (_lineItemQuantity lineItem) * maybe_ 0 id (product ^. productPrice)))) $
      do  user     <- all_ (shoppingCartDb ^. shoppingCartUsers)
          order    <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                                (\order -> _orderForUser order `references_` user)
          lineItem <- leftJoin_' (all_ (shoppingCartDb ^. shoppingCartLineItems))
                                  (\lineItem -> just_ (_lineItemInOrder lineItem) ==?. pk order)
          product  <- leftJoin_' (all_ (shoppingCartDb ^. shoppingCartProducts))
                                  (\product -> _lineItemForProduct lineItem ==?. just_ (pk product))
          pure (user, lineItem, product)

  mapM_ print allUsersAndTotals2

allUnshippedOrdersDo = do
  conn <- open "shoppingcart3.db"    
  allUnshippedOrders <- runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $
    filter_ (isNothing_ . _orderShippingInfo) $
    all_ (shoppingCartDb ^. shoppingCartOrders)
  mapM_ print allUnshippedOrders
shippingInformationByUserDo = do
  conn <- open "shoppingcart3.db"   
  shippingInformationByUser <-
    runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select $
      do  user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          (userEmail, unshippedCount) <-
            aggregate_ (\(userEmail, order) -> (group_ userEmail, as_ @Int32 countAll_)) $
            do    user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
                  order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                                    (\order -> _orderForUser order `references_` user &&. isNothing_ (_orderShippingInfo order))
                  pure (pk user, order)

          guard_ (userEmail `references_` user)

          (userEmail, shippedCount) <-
            aggregate_ (\(userEmail, order) -> (group_ userEmail, as_ @Int32 countAll_)) $
            do  user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
                order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                                  (\order -> _orderForUser order `references_` user &&. isJust_ (_orderShippingInfo order))
                pure (pk user, order)
          guard_ (userEmail `references_` user)

          pure (user, unshippedCount, shippedCount)

  mapM_ print shippingInformationByUser