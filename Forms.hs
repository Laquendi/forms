module Forms where

import Import
import Yesod.Form.Bootstrap3
import FormType
import Data.Default

adminForms, managerForms, workerForms :: [FormDef]
adminForms = 
    [
      FormDef "test1" MsgTestForm1 (atest TestForm1)
    , FormDef "testDate" MsgTestWithDateControls dtest
    ]
managerForms = 
    [
      FormDef "test2" MsgTestForm2 (atest TestForm2)
    ]
workerForms = 
    [
      FormDef "test3" MsgTestForm3 (atest TestForm3)
    , FormDef "testW" MsgTestWithWorker wtest
    ]

atest dc = formWrapper $ \ uname time -> (dc uname time)
    <$> areq intField (bfs MsgCount) Nothing
    <*> areq textField (bfs MsgInfo) Nothing

wtest = formWrapper $ \ uname time -> (TestWithWorker uname time)
    <$> areq (selectField workers) (bfs MsgWorker) Nothing
    <*> areq intField (bfs MsgRating) (Just 4)
  where 
    workers = do
        us <- runDB $ selectList [] []
        optionsPairs $ map (\(Entity _ User{userName=name}) -> (name, name)) us

dtest = formWrapper $ \ uname time -> (TestDateControls uname time)
    <$> areq (jqueryDayField def) (bfs MsgDate) Nothing
    <*> areq timeFieldTypeText (bfs MsgTime) Nothing
