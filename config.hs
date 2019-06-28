
-- IMPORT                                                                    {{{
--------------------------------------------------------------------------------
import XMonad

import XMonad.Config.Desktop (desktopConfig)
import XMonad.Actions.DynamicProjects (Project(..), dynamicProjects, switchProjectPrompt, shiftToProjectPrompt)

import XMonad.Hooks.DynamicLog (PP, ppOutput, ppCurrent, ppVisible, ppUrgent, ppHidden, ppWsSep, ppSep, ppTitle, dynamicLogWithPP, wrap, shorten)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..), manageDocks, docksEventHook)
import XMonad.Hooks.ManageHelpers (composeOne, isFullscreen, (-?>), doFullFloat)
import XMonad.Hooks.Minimize (minimizeEventHook)
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..))

import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.NamedActions (addDescrKeys, xMessage, subtitle, addName)

import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed (renamed, Rename(..))
import XMonad.Layout.Spacing (Border(Border), spacingRaw, incScreenWindowSpacing, setScreenWindowSpacing)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Util.NamedScratchpad ( namedScratchpadAction , namedScratchpadManageHook , NamedScratchpad(NS), customFloating)
import qualified XMonad.StackSet as StackSet
import XMonad.Prompt (font, bgColor, fgColor, fgHLight, bgHLight, borderColor, promptBorderWidth, height, position, XPPosition(..))

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.Layout.BoringWindows as B

import qualified Codec.Binary.UTF8.String as UTF8

-----------------------------------------------------------------------------}}}
-- MAIN                                                                      {{{
--------------------------------------------------------------------------------
--TODO: move some programs automatically to workspaces
main :: IO ()
main = do
  dbus <- D.connectSession
  _ <- D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  _ <- spawn $ "feh --bg-scale " ++ bg_image
  -- Enables shared clipboard under VMWare
  _ <- spawn "vmware-user"

  xmonad
    $ dynamicProjects projects
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ addDescrKeys ((myModMask, xK_F1), xMessage) myAdditionalKeys
    -- $ addDescrKeys ((myModMask, xK_F1), showKeybindings) myAdditionalKeys
    $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }

-----------------------------------------------------------------------------}}}
-- GLOBAL VARIABLES                                                          {{{
--------------------------------------------------------------------------------
-- General config
myTerminal = "urxvt"
myModMask = mod4Mask
myBorderWidth = 1
myBrowser = "firefox"
mySpacing = Border 3 3 3 3
prompt = 20

-- Colours
_fg        = "#ebdbb2"
bg        = "#282828"
_gray      = "#a89984"
_bg1       = "#3c3836"
_bg2       = "#505050"
_bg3       = "#665c54"
_bg4       = "#7c6f64"

_green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
_purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"

-- Font
myFont = "xft:SpaceMono Nerd Font Mono:" ++ "fontformat=truetype:size=10:antialias=true"

-- Background
bg_image :: FilePath
bg_image = "~/Downloads/wallpaper.jpg"

-----------------------------------------------------------------------------}}}
-- LAYOUT                                                                    {{{
--------------------------------------------------------------------------------
myLayouts = renamed [CutWordsLeft 1] . avoidStruts . minimize . B.boringWindows $ perWS

-- layout per workspace
perWS = onWorkspace wsGEN my3FT $
        onWorkspace wsWRK myAll $
        onWorkspace wsSYS myFT  $
        onWorkspace wsMED my3FT $
        onWorkspace wsTMP myFT  $
        onWorkspace wsGAM myFT  $
        myAll -- all layouts for all other workspaces


myFT  = myTile ||| myFull
my3FT = myTile ||| myFull ||| my3cmi
myAll = myTile ||| myFull ||| my3cmi

myDefaultSpacing = spacingRaw True mySpacing True mySpacing True

myFull = renamed [Replace "Full"] $ myDefaultSpacing $ noBorders Full
myTile = renamed [Replace "Main"] $ myDefaultSpacing $ Tall 1 (3/100) (1/2)
my3cmi = renamed [Replace "3Col"] $ myDefaultSpacing $ ThreeColMid 1 (3/100) (1/2)

-----------------------------------------------------------------------------}}}
-- THEMES                                                                    {{{
--------------------------------------------------------------------------------
-- Prompt themes
myPromptTheme = def
  { font              = myFont
  , bgColor           = darkgreen
  , fgColor           = white
  , fgHLight          = white
  , bgHLight          = pur2
  , borderColor       = pur2
  , promptBorderWidth = 0
  , height            = prompt
  , position          = Top
  }

warmPromptTheme = myPromptTheme
  { bgColor           = yellow
  , fgColor           = darkred
  , position          = Top
  }

coldPromptTheme = myPromptTheme
  { bgColor           = aqua
  , fgColor           = darkgreen
  , position          = Top
  }

-----------------------------------------------------------------------------}}}
-- WORKSPACES                                                                {{{
--------------------------------------------------------------------------------
wsGEN = "\xf269"
wsWRK = "\xf02d"
wsSYS = "\xf300"
wsMED = "\xf001"
wsTMP = "\xf2db"
wsGAM = "\xf11b"

myWorkspaces :: [String]
myWorkspaces = [wsGEN, wsWRK, wsSYS, wsMED, wsTMP, wsGAM, "7", "8", "9"]

-----------------------------------------------------------------------------}}}
-- SCRATCHPADS                                                               {{{
--------------------------------------------------------------------------------
scratchpads :: [NamedScratchpad]
scratchpads =
    [ scratch "term"   ""
    , scratch "term2"  ""
    ]
    where
      scratchpadSize = StackSet.RationalRect (1/6) (1/6) (2/3) (2/3)
      mySPFloat      = customFloating scratchpadSize
      scratch label command = NS label (myTerminal ++ " -name " ++ label ++ command) (resource =? label) mySPFloat

-----------------------------------------------------------------------------}}}
-- PROJECTS                                                                  {{{
--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "code"
            , projectDirectory = "~/code/"
            , projectStartHook = Just $ do spawn myBrowser
                                           spawn myTerminal
            }
  ]

-----------------------------------------------------------------------------}}}
-- KEYBINDINGS                                                               {{{
--------------------------------------------------------------------------------
myAdditionalKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
  myProgramKeys ++ myScratchpadKeys ++ myWindowManagerKeys

myProgramKeys =
  [ ("M-f"        , addName "Open firefox" $ spawn myBrowser)
  , ("M-e"        , addName "Open Emacs" $ spawn "emacs")
  , ("M-t"        , addName "Open terminal" $ spawn myTerminal)
  , ("M-d"        , addName "Open rofi" $ spawn "rofi -show run")
  ]

myScratchpadKeys =
  [ ("M-<Return>"  , addName "Open SP1" $ namedScratchpadAction scratchpads "term")
  , ("M-S-<Return>", addName "Open SP2" $ namedScratchpadAction scratchpads "term2")
  ]

myWindowManagerKeys =
  [ ("M-b"        , addName "Do (not) respect polybar" $ sendMessage ToggleStruts)
  , ("M-S-b"      , addName "Increase spacing between windows" $ incScreenWindowSpacing 5)
  , ("M-v"        , addName "Set default spacing between windows" $ setScreenWindowSpacing 5)
  , ("M-S-v"      , addName "Decrease spacing between windows" $ incScreenWindowSpacing (-5))
  , ("M-u"        , addName "Switch view to project" $ switchProjectPrompt warmPromptTheme)
  , ("M-S-u"      , addName "Send current window to project" $ shiftToProjectPrompt coldPromptTheme)
  ]

-----------------------------------------------------------------------------}}}
-- MANAGEHOOK                                                                {{{
--------------------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "feh"              --> doFloat
    , role      =? "pop-up"           --> doFloat
    , namedScratchpadManageHook scratchpads
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

-----------------------------------------------------------------------------}}}
-- LOGHOOK                                                                   {{{
--------------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = myAddSpaces 25
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

-----------------------------------------------------------------------------}}}
-- STARTUPHOOK                                                               {{{
--------------------------------------------------------------------------------
myStartupHook = do
  setWMName "LG3D"
  spawn "$HOME/.xmonad/polybar.sh"
  -- spawn "dropbox"

-----------------------------------------------------------------------------}}}
-- CONFIG                                                                    {{{
--------------------------------------------------------------------------------
myConfig = desktopConfig
  { terminal            = myTerminal
  , layoutHook          = myLayouts
  , manageHook          = placeHook(smart(0.5, 0.5))
      <+> manageDocks
      <+> myManageHook
      <+> myManageHook'
      <+> manageHook def
  , handleEventHook     = docksEventHook
      <+> minimizeEventHook
      <+> fullscreenEventHook
  , startupHook         = myStartupHook
  , focusFollowsMouse   = False
  , clickJustFocuses    = False
  , borderWidth         = myBorderWidth
  , normalBorderColor   = bg
  , focusedBorderColor  = pur2
  , workspaces          = myWorkspaces
  , modMask             = myModMask
  }
-----------------------------------------------------------------------------}}}
