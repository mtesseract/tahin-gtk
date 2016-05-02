-- Tahin (Gtk)
-- Copyright (C) 2016 Moritz Schulte <mtesseract@silverratio.net>

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad.Reader
import qualified Crypto.Hash.SHA256 as SHA256
import           Crypto.Tahin
import           Data.ByteString (ByteString)
import           Data.FileEmbed
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text (null)
import           Data.Version
import           Graphics.UI.Gtk
import           Paths_tahin_gtk
  
defaultPasswordLength :: Int
defaultPasswordLength = 20

defaultHashFunction :: ByteString -> ByteString
defaultHashFunction = SHA256.hash



-- | These are the actions supported by this program.
data AppAction = Quit
               | Initialize
               | Start
               | Restart
               | MasterPasswordChanged
               | MasterPasswordOk
               | MasterPasswordRegister Text
               | ServiceIdentifierChanged
               | ServiceIdentifierRegister Text
               | PasswordCopy
               | ShowAbout
               | HideAbout
               deriving (Show, Eq)

-- | The 'State' type holds the program state.
data State = State { stateMasterPassword    :: Text
                   , stateServiceIdentifier :: Text
                   , stateBuilder           :: Builder
                   }

-- | Initializer function for 'State' values.
initState :: Builder -> State
initState builder =
  State { stateMasterPassword    = ""
        , stateServiceIdentifier = ""
        , stateBuilder           = builder
        }

-- | Values of this type are returned by the update function, which
-- defines the supported program flow of this program.
data Update = Update { updateState  :: State
                     , updateView   :: Maybe UpdateView
                     , updateAction :: IO (Maybe AppAction)
                     }

-- | Type for a function updating the user interface.
type UpdateView = State -> IO ()

-- | Monad extending the IO Monad with support for reading a shared
-- 'IORef' 'State'.
type TahinGtk = ReaderT (IORef State) IO

--------------------------------------------
-- Widget Names and Gtk related constants --
--------------------------------------------

uiDefinition :: String
uiDefinition = $(embedStringFile "Tahin.glade")

windowMainName :: String
windowMainName = "window"

windowAboutName :: String
windowAboutName = "windowAbout"

menuProgramQuitName :: String
menuProgramQuitName = "menuProgramQuit"

menuHelpAboutName :: String
menuHelpAboutName = "menuHelpAbout"

masterPasswordEntry :: String
masterPasswordEntry = "masterPasswordEntry"

tahinPasswordEntry :: String
tahinPasswordEntry = "tahinPasswordEntry"

tahinUIStack :: String
tahinUIStack = "windowStack"

pageMasterPassword :: String
pageMasterPassword = "pageMasterPassword"

pageServiceIdentifier :: String
pageServiceIdentifier = "pageServiceIdentifier"

serviceIdEntryName :: String
serviceIdEntryName = "serviceIdentifierEntry"

statusBarName :: String
statusBarName = "statusBar"

statusBarDefaultContext :: String
statusBarDefaultContext = "default"

buttonMasterPasswordOk :: String
buttonMasterPasswordOk = "masterPasswordButtonOk"

buttonBackName :: String
buttonBackName = "buttonBack"

statusMsgMasterPasswordRegistered :: Text
statusMsgMasterPasswordRegistered = "Got Master Password"

statusMsgTahinPasswordCopied :: Text
statusMsgTahinPasswordCopied = "Copied Password to Clipboard"

statusMessageShow :: Builder -> Text -> IO ()
statusMessageShow builder msg =
  do statBar <- builderGetObject builder castToStatusbar statusBarName
     ctxId <- statusbarGetContextId statBar statusBarDefaultContext
     -- FIXME. Looks like a bug in the API exposed by Gtk2hs:
     statusbarRemoveAll statBar (fromIntegral ctxId) 
     void $ statusbarPush statBar ctxId msg
  
statusMessageClear :: Builder -> IO ()
statusMessageClear builder = do
  statBar <- builderGetObject builder castToStatusbar statusBarName
  ctxId <- statusbarGetContextId statBar statusBarDefaultContext
     -- FIXME. Looks like a bug in the API exposed by Gtk2hs:
  statusbarRemoveAll statBar (fromIntegral ctxId)

buttonPasswordCopyName :: String
buttonPasswordCopyName = "buttonPasswordCopy"

-- | The update function, defining the main program logic.

update :: AppAction -> State -> Update

update Initialize state =
  Update { updateState = state
         , updateView  = Just $ \ state -> do
             let builder = stateBuilder state
             winMain <- builderGetObject builder castToWindow      windowMainName
             widgetShowAll winMain
         , updateAction = return $ Just Start
         }

update Start state =
  Update { updateState = state
         , updateView = Just $ \ state -> do
             let builder = stateBuilder state
             button <- builderGetObject builder castToButton buttonMasterPasswordOk
             widgetSetSensitivity button False
         , updateAction = return Nothing
         }
  
update Quit state =
  Update { updateState = state
         , updateView = Nothing
         , updateAction = return (Just Quit)
         }

update Restart state =
  Update { updateState = state { stateMasterPassword    = ""
                               , stateServiceIdentifier = "" }
         , updateView = Just $ \ state -> do
             let builder = stateBuilder state
             stack <- getStack builder
             stackSetVisibleChildName stack pageMasterPassword
             statusMessageClear builder
         , updateAction = return Nothing
         }

update MasterPasswordChanged state =
  Update { updateState = state
         , updateView = Just $ \ state -> do
             let builder = stateBuilder state
             masterPw <- builderGetObject builder castToEntry masterPasswordEntry
               >>= entryGetText :: IO Text
             button <- builderGetObject builder castToButton buttonMasterPasswordOk
             widgetSetSensitivity button (not (Data.Text.null masterPw))
         , updateAction = return Nothing
         }

update MasterPasswordOk state =
  Update { updateState = state
         , updateView = Nothing
         , updateAction = do
             let builder = stateBuilder state
             masterPw <- builderGetObject builder castToEntry masterPasswordEntry
               >>= entryGetText
             return $ Just $ MasterPasswordRegister masterPw
         }

update (MasterPasswordRegister masterPw) state =
  Update { updateState = state { stateMasterPassword = masterPw }
         , updateView = Just $ \ state -> do
             let builder = stateBuilder state
             stack <- getStack builder
             stackSetVisibleChildName stack pageServiceIdentifier
             statusMessageShow builder statusMsgMasterPasswordRegistered
         , updateAction = return Nothing
         }

update ServiceIdentifierChanged state =
  Update { updateState = state
         , updateView = Nothing
         , updateAction = do
             let builder = stateBuilder state
             serviceId <- builderGetObject builder castToEntry serviceIdEntryName
               >>= entryGetText
             return $ Just $ ServiceIdentifierRegister serviceId
         }

update (ServiceIdentifierRegister serviceIdentifier) state =
  Update { updateState = state { stateServiceIdentifier = serviceIdentifier }
         , updateView = Just $ \ state -> do
             let builder      = stateBuilder state
                 masterPw     = stateMasterPassword state
                 serviceId    = stateServiceIdentifier state
                 pwLength     = defaultPasswordLength
                 hash         = defaultHashFunction
                 tahinPw      = tahin hash pwLength masterPw serviceId
             tahinPassword <- builderGetObject builder castToEntry tahinPasswordEntry
             entrySetText tahinPassword tahinPw
         , updateAction = return Nothing
         }

update PasswordCopy state =
  Update { updateState = state
         , updateView = Just $ \ state -> do
             let builder = stateBuilder state
             tahinPw <- builderGetObject builder castToEntry tahinPasswordEntry
               >>= entryGetText :: IO Text
             clipboard <- clipboardGet selectionClipboard
             clipboardSetText clipboard tahinPw
             statusMessageShow builder statusMsgTahinPasswordCopied
         , updateAction = return Nothing
         }

update ShowAbout state =
  Update { updateState = state
         , updateView = Just $ \ state -> do
             let builder = stateBuilder state
             about   <- builderGetObject builder castToAboutDialog windowAboutName
             set about [ aboutDialogVersion := showVersion version ]
             winMain <- builderGetObject builder castToWindow windowMainName
             set about [ windowTransientFor := winMain ]
             void $ dialogRun about
         , updateAction = return Nothing
         }
           
update HideAbout state =
  Update { updateState = state
         , updateView = Just $ \ state -> do
             let builder = stateBuilder state
             winAbout <- builderGetObject builder castToWindow windowAboutName
             widgetHide winAbout
         , updateAction = return Nothing
         }

-- | Execute an Action.
runAction :: AppAction -> TahinGtk ()
runAction action = do
  -- liftIO $ putStrLn $ "[runUpdate] processing action = " ++ show action
  state' <- ask
  currentState <- liftIO $ readIORef state'
  let updateRes        = update action currentState
      newState         = updateState updateRes
      maybeNewView     = updateView updateRes
      ioMaybeNewAction = updateAction updateRes
  liftIO $ writeIORef state' newState
  case maybeNewView of
    Just newView -> liftIO $ newView newState
    Nothing      -> return ()
  maybeNewAction <- liftIO $ ioMaybeNewAction
  case maybeNewAction of
    Just Quit -> if action == Quit
                    then liftIO $ mainQuit
                    else return ()
    Just newAction -> runAction newAction
    Nothing        -> return ()

----------------------
-- Main Entry Point --
----------------------

main :: IO ()
main = do
  void $ initGUI
  builder <- builderNew
  builderAddFromString builder uiDefinition
  state' <- newIORef (initState builder)
  installHandlers state'
  runReaderT (runAction Initialize) state'
  mainGUI

---------------------------
-- Signals and Callbacks --
---------------------------

signalSpecifications :: IORef State -> [IO ()]
signalSpecifications state' = 
  [ connectSignal windowMainName castToWindow deleteEvent $
    action Quit >> return False
  , connectSignal menuProgramQuitName castToMenuItem menuItemActivated $
    action Quit
  , connectSignal menuHelpAboutName castToMenuItem menuItemActivated $
    action ShowAbout
  , connectSignal windowAboutName castToWindow deleteEvent $
    action HideAbout >> return False
  , connectSignal buttonMasterPasswordOk castToButton buttonActivated $
    action MasterPasswordOk
  , connectSignal masterPasswordEntry castToEntry entryActivated $
    action MasterPasswordOk
  , connectSignal buttonBackName castToButton buttonActivated $
    action Restart
  , connectSignal serviceIdEntryName castToEntry editableChanged $
    action ServiceIdentifierChanged
  , connectSignal masterPasswordEntry castToEntry editableChanged $
    action MasterPasswordChanged
  , connectSignal buttonPasswordCopyName castToButton buttonActivated $
    action PasswordCopy
  ]
    
  where action a = liftIO $ (runReaderT (runAction a) state')

        connectSignal :: GObjectClass cls
                      => String -> (GObject -> cls) -> Signal cls cb -> cb -> IO ()
        connectSignal name cast signal callback = do
          builder <- stateBuilder <$> readIORef state'
          object  <- builderGetObject builder cast name
          void $ on object signal callback

installHandlers :: IORef State -> IO ()
installHandlers = void . sequence . signalSpecifications

----------------------
-- Helper Functions --
----------------------

getStack :: Builder -> IO Stack
getStack builder = builderGetObject builder castToStack tahinUIStack
