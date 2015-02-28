module MyPersistTypes where

import Prelude
import Database.Persist.TH

data UserType = Admin | Manager | Worker
    deriving (Show, Read, Enum, Bounded, Eq)
$(derivePersistField "UserType")


managerRights :: UserType -> Bool
managerRights Admin = True
managerRights Manager = True
managerRights _ = False

adminRights :: UserType -> Bool
adminRights Admin = True
adminRights _ = False

