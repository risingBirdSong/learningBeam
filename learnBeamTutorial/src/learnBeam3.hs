
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