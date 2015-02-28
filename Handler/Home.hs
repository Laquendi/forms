{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth.HashDB (setPassword, validatePass)
import Yesod.Auth (requireAuth)
import Yesod.Form.Bootstrap3
import Control.Monad
import Data.Text (pack)
import Forms
import FormType
-- import qualified Data.Text as T

getHomeR :: Handler Html
getHomeR = do
    (Entity _ user) <- requireAuth
    let isAdmin = adminRights . userType $ user
        isManager = managerRights . userType $ user
    defaultLayout $(widgetFile "homepage")

requireRights :: (User -> Bool) -> Handler a -> Handler a
requireRights f m = do
    (Entity _ user) <- requireAuth
    case f user of
        True -> m
        False -> permissionDenied "Insufficient access rights"

requireAdmin :: Handler a -> Handler a
requireAdmin = requireRights (adminRights . userType)

requireManager :: Handler a -> Handler a
requireManager = requireRights (managerRights . userType)

userForm :: Form User
userForm = renderBootstrap3 formLayout $ User
    <$> areq textField (bfs MsgName) Nothing
    <*> (Just <$> areq textField (bfs MsgPassword) Nothing)
    <*> pure Nothing
    <*> areq (selectFieldList types) (bfs MsgUserType) Nothing
  where
    types = zip (map (pack.show) t) t
    t :: [UserType]
    t = reverse [minBound..maxBound]

passwordForm :: Form (Text, Text)
passwordForm = renderBootstrap3 formLayout $ (,)
    <$> areq passwordField (bfs MsgOldPassword) Nothing
    <*> areq passwordField (bfs MsgNewPassword) Nothing

forcePasswordForm :: Form (Text, Text)
forcePasswordForm = renderBootstrap3 formLayout $ (,)
    <$> areq textField (bfs MsgUsername) Nothing
    <*> areq textField (bfs MsgPassword) Nothing

getChangePasswordR :: Handler Html
getChangePasswordR = do
    (formWidget, formEnctype) <- generateFormPost passwordForm
    let submitRoute = ChangePasswordR
        title = MsgChangePassword
    defaultLayout $(widgetFile "generic-form")

postChangePasswordR :: Handler Html
postChangePasswordR = do
    ((result, formWidget), formEnctype) <- runFormPost passwordForm
    (Entity uid user) <- requireAuth
    let renderForm = do 
        let submitRoute = ChangePasswordR
            title = MsgChangePassword
        defaultLayout $(widgetFile "generic-form")
    case result of
        FormSuccess (old, new) -> do 
            case validatePass user old of
                Just True -> do
                    newU <- setPassword new user 
                    runDB $ replace uid newU
                    setMessageI MsgPasswordUpdated
                    redirect HomeR
                _ -> do 
                    setMessageI MsgWrongOld
                    renderForm
        _ -> do
            setMessageI MsgError
            renderForm

getChangeUserPasswordR :: Handler Html
getChangeUserPasswordR = requireAdmin $ do
    (formWidget, formEnctype) <- generateFormPost forcePasswordForm
    let submitRoute = ChangeUserPasswordR
        title = MsgChangeUserPassword
    defaultLayout $(widgetFile "generic-form")

postChangeUserPasswordR :: Handler Html
postChangeUserPasswordR = requireAdmin $ do
    ((result, formWidget), formEnctype) <- runFormPost forcePasswordForm
    let renderForm = do 
        let submitRoute = ChangeUserPasswordR
            title = MsgChangeUserPassword
        defaultLayout $(widgetFile "generic-form")
    case result of
        FormSuccess (name, pass) -> do 
            muser <- runDB $ getBy (UniqueUser name)
            case muser of 
                Just (Entity uid user) -> do
                    newU <- setPassword pass user 
                    runDB $ replace uid newU
                    setMessageI MsgPasswordUpdated
                    redirect HomeR
                Nothing -> do
                    setMessageI MsgNoSuchUser
                    redirect ChangeUserPasswordR
        _ -> do
            setMessageI MsgError
            renderForm

getAddUserR :: Handler Html
getAddUserR = requireAdmin $ do
    (formWidget, formEnctype) <- generateFormPost userForm
    let submitRoute = AddUserR
        title = MsgAddUser
    defaultLayout $(widgetFile "generic-form")

postAddUserR :: Handler Html
postAddUserR = requireAdmin $ do
    ((result, _), _) <- runFormPost userForm
    case result of
        FormSuccess u@User{userPassword = pass} -> do 
            uWithPass <- setPassword (maybe "" id pass) u
            _ <- runDB $ insert uWithPass
            setMessageI MsgUserAdded
            redirect AddUserR
        _ -> redirect HomeR

getFirstUserR :: Text -> Text -> Handler ()
getFirstUserR name pass = do
    users <- runDB $ selectList [] []
    when (length (users :: [Entity User]) == 0) $ do
        let u = User name Nothing Nothing Admin
        uWithPass <- setPassword pass u
        void (runDB $ insert uWithPass)
    redirect HomeR

findFormByName :: Text -> [(Handler a -> Handler a, FormDef)]
findFormByName name = adm ++ mng ++ wrk
  where
    adm = f requireAdmin adminForms
    mng = f requireManager managerForms
    wrk = f id workerForms
    f m xs = zip (repeat m) . filter (\x -> formDefName x == name) $ xs

forceUnique :: Text -> (FormDef -> Handler a) -> Handler a
forceUnique name m = do
    let forms = findFormByName name
    case length forms of
        0 -> setMessage "No survey with that name" >> redirect HomeR
        1 -> do 
            let (req, h) = head forms
            req $ m h
        _ -> setMessage "Multiple surveys with the same name" >> redirect HomeR

getFormR :: Text -> Handler Html
getFormR name = forceUnique name $ \(FormDef _ title mform) -> do
    form <- mform
    (formWidget, formEnctype) <- generateFormPost form
    let submitRoute = FormR name
    defaultLayout $(widgetFile "generic-form")

postFormR :: Text -> Handler Html
postFormR name = forceUnique name $ \(FormDef _ title mform) -> do
    form <- mform
    ((result, formWidget), formEnctype) <- runFormPost form 
    let renderForm = do 
        let submitRoute = FormR name
        defaultLayout $(widgetFile "generic-form")
    case result of
        FormSuccess res -> do 
            res
            setMessageI MsgFormSuccess
            redirect HomeR
        _ -> do
            setMessageI MsgError
            renderForm

getSetLanguageR :: Text -> Handler ()
getSetLanguageR lang = do
    (Entity uid _) <- requireAuth
    setLanguage lang
    runDB $ update uid [UserLanguage =. Just lang]
    redirect HomeR
