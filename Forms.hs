module Forms where

import Import
import Yesod.Form.Bootstrap3
import FormType
import Data.Default

-- three arguments: unique id string, localized name, form definition
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
    <*> areq intField (bfs MsgRating) Nothing 
  where 
    workers = do
        users <- runDB $ selectList [] [] -- query all users from database
        let workerNames = map (\(Entity _ User{userName=name}) -> (name, name)) users --map each user like this: user -> (userName, userName)
        optionsPairs $ workerNames --optionsPairs makes "option list" form <select> element

dtest = formWrapper $ \ uname time -> (TestDateControls uname time)
    <$> areq (jqueryDayField def) (bfs MsgDate) Nothing
    <*> areq timeFieldTypeText (bfs MsgTime) Nothing
