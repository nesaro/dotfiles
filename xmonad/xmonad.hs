import XMonad hiding ((|||) )
import XMonad.Layout.LayoutCombinators   -- use the one from LayoutCombinators instead
import System.Exit
import Graphics.X11.Xlib 
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell
import XMonad.ManageHook
import XMonad.Actions.CycleWS


-- tags
import XMonad.Actions.TagWindows
-- to use tagPrompt
import XMonad.Prompt 
import XMonad.Prompt.Ssh
import XMonad.Prompt.RunOrRaise

--Dynamiclog
import XMonad.Hooks.DynamicLog  -- ( PP(..), dynamicLogWithPP, dzenColor, wrap, defaultPP )
import XMonad.Util.Run
import System.IO
import XMonad.Operations
import XMonad.Actions.DwmPromote
import XMonad.Layout hiding ( (|||)) -- Usando el operador de layoutcomibnators
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Prompt.Window
import XMonad.Prompt.Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.SimpleFloat
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.ManageDocks -- Sustituye defaultgaps
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ShowWName
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn
import XMonad.Layout.Roledex 
import XMonad.Layout.IM
import XMonad.Layout.MosaicAlt
import qualified XMonad.Layout.HintedGrid as HG
import qualified XMonad.Layout.Grid as SG
import XMonad.Layout.MagicFocus
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spacing
import XMonad.Actions.TopicSpace
import XMonad.Actions.GridSelect
import XMonad.Layout.BorderResize
import XMonad.Layout.Magnifier
import XMonad.Util.Replace
import XMonad.Actions.WindowGo
import XMonad.Layout.SimpleDecoration

import XMonad.Util.Loggers


import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S


-- import XMonad.Prompt             ( XPConfig(..), XPPosition(..) )
--import XMonad.Prompt.Shell       ( shellPrompt )
-- import XMonad.Hooks.ManageDocks  -- Da error en casa
 
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Ratio
-- import Graphics.X11  -- Da error en casa

--import XMonad.Actions.WindowBringer -- Da error en casa


import XMonad.Hooks.XPropManage
import Data.List


import System.IO

-- Necesario para los keybindings:
import qualified XMonad.StackSet as W 

import Data.Traversable (traverse)
-- Fool java
import XMonad.Hooks.SetWMName


myModMask = mod4Mask -- Tecla windows
modWinMask = mod4Mask


 -- The list of all topics/workspaces of your xmonad configuration.
 -- The order is important, new topics must be inserted
 -- at the end of the list if you want hot-restarting
 -- to work.


logTag :: Logger
logTag = withWindowSet $ traverse (fmap show . getTags) . W.peek


--statusBarCmd= "dzen2 -bg '#2c2c32' -fg 'grey70' -w 620 -sa c -fn '-*-profont-*-*-*-*-11-*-*-*-*-*-iso8859' -e '' -xs 1 -ta l"
statusBarCmd = "dzen2 -e 'onstart=lower' -p -ta r -bg '#2e3436' -fg '#babdb6' -h 28 -w 780"

main = do xmproc <- spawnPipe "xmobar /home/nesaro/.xmobarrc"
          xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $  defaultConfig
                     { borderWidth        = 2
                     , normalBorderColor  = "grey30"
                     , focusedBorderColor = "#ff0000" 
                     , workspaces         = ["nav", "adm", "agenda", "chat", "downloads", "mail", "mus", "tr", "tv"] 
                     , terminal           = "myterm"
                     --, logHook            = do dynamicLogWithPP $ robPP din
                     --                          myLogHook
                     , logHook            = dynamicLogWithPP $ xmobarPP
                                                { ppOutput = hPutStrLn xmproc
                                                , ppTitle = xmobarColor "green" "" . shorten 50
                                                , ppExtras = [logTag]
                                                }
                     , manageHook         = manageSpawn <+> myManageHook2 <+> myManageHook3 <+> manageDocks <+> manageHook defaultConfig -- El ultimo termino viene del modulo de area de paneles
                     , keys               = newKeys 
                     , layoutHook         = myLayout
                     , startupHook        = setWMName "LG3D"
                     , focusFollowsMouse  = False
                     }
                     where
                       tiled = Tall 1 (3%100) (1%2)


lall =  spacing 3 (MosaicAlt M.empty) ||| tiled ||| Roledex ||| Mirror tiled ||| noBorders Full ||| magicFocus(noBorders Circle) ||| HG.Grid False ||| SG.Grid ||| borderResize (simpleFloat) ||| simpleTabbed ||| mySplit
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     mySplit = magnifiercz' 1.4 $ Tall nmaster delta2 ratio2
     -- Percent of screen to increment by when resizing panes
     delta2   = 3/100
     -- Default proportion of screen occupied by master pane
     ratio2   = 60/100
lchat = withIM (1%7) (Role "buddy_list") (spacing 2 (tiled))
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
     mySplit = magnifiercz' 1.4 $ Tall nmaster delta2 ratio2
     delta2   = 3/100
     ratio2   = 60/100
ladm =  spacing 3 (MosaicAlt M.empty) ||| tiled ||| Roledex ||| Mirror tiled ||| noBorders Full |||  HG.Grid False ||| SG.Grid ||| simpleTabbed ||| mySplit
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
     mySplit = magnifiercz' 1.4 $ Tall nmaster delta2 ratio2
     delta2   = 3/100
     ratio2   = 60/100

myLayout = avoidStruts $ smartBorders(onWorkspace "chat" lchat $ onWorkspace "adm" ladm lall)

toAdd x =
    --TAGS
    --((modMask x, xK_f  ), withFocused (addTag "abc"))
    --((modMask x, xK_g  ), tagPrompt defaultXPConfig (\s -> withTaggedGlobal s float)) -- Hace las ventanas con tag flotantes
    --, ((modWinMask .|. shiftMask, xK_g  ), tagPrompt defaultXPConfig (\s -> withWindowSet (addTags s))) -- a;adir un tag de golpe TODO
    --, ((modWinMask , xK_g  ), tagPrompt defaultXPConfig (\s -> withTaggedP s (W.shiftWin "2"))) -- ni idea
    [((modWinMask, xK_t), SM.submap . M.fromList $ 
                [ ((modWinMask, xK_t), tagPrompt defaultXPConfig (\s -> focusUpTaggedGlobal s ))-- Enfoca a la siguiente ventana
                , ((modWinMask, xK_s), tagPrompt defaultXPConfig (\s -> withFocused (addTag s)))
                , ((modWinMask, xK_d  ), tagDelPrompt defaultXPConfig) -- Borra el tag de la ventana
                , ((modWinMask, xK_g  ), tagPrompt defaultXPConfig (\s -> withTaggedGlobalP s shiftHere)) -- Cambia las ventanas con el tag al escritorio actual

                ])

    --WINDOWS
    , ((modWinMask, xK_w), SM.submap . M.fromList $ 
                [ ((modWinMask, xK_w     ), windowPromptGoto defaultXPConfig)
                , ((modWinMask, xK_i     ), windowPromptBring defaultXPConfig)
                , ((modWinMask, xK_e), goToSelected defaultGSConfig)
                , ((modWinMask, xK_c), withWorkspace defaultXPConfig (windows . copy))
                , ((modWinMask, xK_i), withWorkspace defaultXPConfig (windows . W.shift))

                ])

    --WORKSPACES
    , ((modWinMask, xK_e), SM.submap . M.fromList $ 
                [ ((modWinMask, xK_d), removeWorkspace)
                , ((modWinMask, xK_e), selectWorkspace defaultXPConfig)
                , ((modWinMask, xK_r), renameWorkspace defaultXPConfig)
                ])

    , ((modWinMask .|. shiftMask, xK_Right), nextWS) 
    , ((modWinMask .|. shiftMask, xK_Left), prevWS) 
    , ((modWinMask , xK_Right), moveTo Next NonEmptyWS) 
    , ((modWinMask , xK_Left), moveTo Prev NonEmptyWS) 

    --TASKS
    , ((modWinMask, xK_k), SM.submap . M.fromList $ 
                [ ((modWinMask, xK_a), (windows $ W.greedyView "agenda") >> (sendMessage $ JumpToLayout "MosaicAlt") >> spawnHere ("zim") >> spawnHere ("gnome-terminal -t task"))
                , ((modWinMask, xK_b), (windows $ W.greedyView "blog") >> (sendMessage $ JumpToLayout "Tall")  >> spawn ("firefox -P blogger -no-remote http://blogger.com"))
                , ((modWinMask, xK_c), (windows $ W.greedyView "chat") >> (sendMessage $ JumpToLayout "IM Spacing 3 Tall") >> spawn ("pidgin"))
                , ((modWinMask, xK_e), (windows $ W.greedyView "mus") >> (sendMessage $ JumpToLayout "Circle") >> spawn ("exaile"))
                , ((modWinMask, xK_g), (windows $ W.greedyView "gentoo") >> (sendMessage $ JumpToLayout "Full") >> spawn ("firefox -P gentoo -no-remote http://bugs.gentoo.org"))
                , ((modWinMask, xK_m), (windows $ W.greedyView "mail") >> (sendMessage $ JumpToLayout "Tall") >> spawnHere ("claws-mail") >> spawn ("uzbl.browser -n gmail gmail.com") >> spawn ("twitux"))
                , ((modWinMask, xK_s), (windows $ W.greedyView "social") >> (sendMessage $ JumpToLayout "Tall") >> spawnHere ("uzbl-browser -n twitter --gtk-name=twitter twitter.com"))
                , ((modWinMask, xK_n), (windows $ W.greedyView "natural") >> (sendMessage $ JumpToLayout "Tall") >> spawnHere ("uzbl-browser --gtk-name=natural 127.0.0.1:8000") >> spawnHere ("gnome-terminal -t naturaldevelop --profile=coding --working-directory=/home/nesaro/proyectos/naturalcloud.net/") >> spawnHere ("gnome-terminal -t djangoserver --working-directory=/home/nesaro/repo/naturalcloud.net/"))
                , ((modWinMask, xK_p), (windows $ W.greedyView "natural") >> (sendMessage $ JumpToLayout "Tall") >> spawnHere ("planner"))
                , ((modWinMask, xK_i), (windows $ W.greedyView "simple") >> (sendMessage $ JumpToLayout "Tabbed Simplest") >> spawn ("firefox -P simple -no-remote") >> spawn ("myterm") >> spawn("zim simple"))
                , ((modWinMask, xK_o), (windows $ W.greedyView "colony") >> (sendMessage $ JumpToLayout "Tabbed Simplest") >> spawn ("gnome-terminal -t colony --profile=coding --working-directory=/home/nesaro/proyectos/colony/github"))
                , ((modWinMask, xK_v), (windows $ W.greedyView "tv") >> (sendMessage $ JumpToLayout "Full") >> spawn ("kaffeine"))
                ])

    --LAYOUTS
    , ((modWinMask, xK_l), SM.submap . M.fromList $ 
                [ ((modWinMask, xK_l), layoutPrompt defaultXPConfig) --Pregunta por el layout
                , ((modWinMask, xK_c), sendMessage $ JumpToLayout "Circle") 
                , ((modWinMask, xK_m), sendMessage $ JumpToLayout "MosaicAlt") 
                , ((modWinMask, xK_t), sendMessage $ JumpToLayout "Tall") 
                , ((modWinMask .|. shiftMask, xK_t), sendMessage $ JumpToLayout "Magnifier NoMaster Tall") 
                , ((modWinMask, xK_f), sendMessage $ JumpToLayout "Full") 
                , ((modWinMask, xK_g), sendMessage $ JumpToLayout "Grid False") 
                , ((modWinMask, xK_b), sendMessage $ JumpToLayout "Tabbed Simplest") 
                , ((modWinMask, xK_r), sendMessage $ JumpToLayout "Roledex") 
                , ((modWinMask, xK_o), sendMessage $ JumpToLayout "Simple Float") 
                ])

    --MOSAICALT
    , ((modWinMask, xK_m), SM.submap . M.fromList $ 
                [ ((modWinMask, xK_a    ), withFocused (sendMessage . expandWindowAlt))
                , ((modWinMask, xK_z    ), withFocused (sendMessage . shrinkWindowAlt))
                , ((modWinMask, xK_s    ), withFocused (sendMessage . tallWindowAlt))
                , ((modWinMask, xK_d    ), withFocused (sendMessage . wideWindowAlt))
                , ((modWinMask, xK_space), sendMessage resetAlt)
                ])

    --WEBAPPS
    , ((modWinMask, xK_a), SM.submap . M.fromList $
            [ ((modWinMask, xK_a),    spawn "uzbl-browser bbs.archlinux.org")
            , ((modWinMask .|. shiftMask, xK_a),    spawn "uzbl-browser wiki.archlinux.org")
            , ((modWinMask, xK_m),    spawn "uzbl-browser -n gmail --gtk-name=gmail gmail.com")
            , ((modWinMask, xK_e),    spawn "google-chrome --app=http://evernote.com")
            , ((modWinMask, xK_x),    spawn "uzbl-browser xmonad.org")
            , ((modWinMask, xK_r),    spawn "uzbl-browser --gtk-name=reddit reddit.com")
            , ((modWinMask, xK_f),    spawn "uzbl-browser --gtk-name=reader reader.google.com")
            , ((modWinMask, xK_u),    spawn "uzbl-browser uzbl.org")
            , ((modWinMask, xK_w),    spawn "uzbl-browser wikipedia.org")
            ])
    , ((modMask x , xK_q), spawn("killall dzen2") >> restart "xmonad" True)
    , ((modWinMask , xK_4), windows $W.greedyView("chat"))
    , ((modWinMask , xK_3), windows $W.greedyView("mail"))
    , ((modWinMask , xK_2), windows $W.greedyView("adm"))
    , ((modWinMask , xK_1), windows $ W.greedyView("nav"))
    , ((modWinMask .|. shiftMask, xK_b), spawn "xscreensaver-command --lock") --Bloquea el escritorio
    , ((modWinMask .|. shiftMask, xK_s), spawn "xterm -bg black -fg white")
    , ((modWinMask .|. shiftMask, xK_d), spawn "gnome-terminal --profile=coding")
    , ((modWinMask .|. shiftMask, xK_k), spawn "myterm")
    , ((modWinMask .|. shiftMask, xK_o), spawn "uzbl-browser")
    , ((modWinMask .|. shiftMask, xK_i), runOrRaisePrompt defaultXPConfig)
    , ((modMask x, xK_BackSpace), focusUrgent) -- Ultima ventana urgente. TODO: Pensar algo mejor para las ventanas blink, tipo añadir tag
    , ((modWinMask, xK_b), sendMessage ToggleStruts) -- Toggle area de paneles
    , ((modWinMask .|. shiftMask, xK_m), sshPrompt defaultXPConfig) -- Toggle area de paneles
    , ((modMask x, xK_F4), kill1)
    , ((modMask x .|. shiftMask , xK_F4), kill1)

    --SEARCH ENGINES
    , ((modWinMask, xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.defaultXPConfig)
    , ((modMask x .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)
    , ((modMask x, xK_p), spawn "exe=`dmenu_run` && eval \"exec $exe\"") -- %! Launch dmenu
    , ((modMask x .|. shiftMask, xK_p), spawn "exe=`dmenu_run` && eval \"exec xterm -e $exe\"") -- %! Launch dmenu
    ]

toRemove x =
     [(shiftMask .|. modMask x, k) | k <- [xK_1 .. xK_9]]++
     [(modMask x, k) | k <- [xK_1 .. xK_9]]++
     [(shiftMask .|. modMask x, k) | k <- [xK_c]]
     

defKeys = keys defaultConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
newKeys x = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
--newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

myManageHook2 :: ManageHook
myManageHook2 = composeAll 
    [
    className   =? "Amarok"             --> doF(W.shift "mus" ),
    className   =? "Pidgin"             --> doF(W.shift "chat" ),
    className   =? "Kopete"             --> doF(W.shift "chat" ),
    className   =? "kopete"             --> doF(W.shift "chat" ),
    className   =? "Claws-mail"           --> doF(W.shift "mail" ),
    className   =? "claws-mail"           --> doF(W.shift "mail" ),
    className   =? "Korganizer"           --> doF(W.shift "agenda" ),
    title   =? "Downloads"           --> doF(W.shift "downloads" ),
    className   =? "zim"           --> doF(W.shift "agenda" ),
    --className   =? "sylpheed"           --> doF(withFocused (addTag "mail")), -- Pendiente, doF espera un Winset
    title       =? "MPlayer"            --> doFloat,
    className   =? "stalonetray"        --> doIgnore,
    className   =? "trayer"             --> doIgnore,
    className   =? "fbpanel"            --> doIgnore,
    className   =? "Gkrellm"            --> doIgnore,
    className   =? "Gkrellm2"            --> doIgnore ] 

--managehook alternativo
--
myManageHook3 :: ManageHook
myManageHook3 = xPropManageHook xPropMatches 

xPropMatches :: [XPropMatch]
xPropMatches = [ ([ (wM_CLASS, any ("gimp"==))], (\w -> float w >> return (W.shift "2")))
               , ([ (wM_COMMAND, any ("screen" ==)), (wM_CLASS, any ("xterm" ==))], pmX (addTag "screen"))
               , ([ (wM_CLASS, any ("xterm" ==))], pmX (addTag "term"))
               , ([ (wM_CLASS, any ("Sylpheed" ==))], pmX (addTag "mail"))
               , ([ (wM_CLASS, any ("Firefox" ==))], pmX (addTag "nav"))
               , ([ (wM_CLASS, any ("Opera" ==))], pmX (addTag "nav"))
               -- , ([ (wM_NAME, any ("Iceweasel" `isInfixOf`))], pmP (W.shift "nav"))
               -- , ([ (wM_NAME, any ("Opera" `isInfixOf`))], pmP (W.shift "nav"))
               ]

-- Submap de searchengines
searchEngineMap method = M.fromList $
                         [ ((modWinMask , xK_g), method S.google)
                         , ((modWinMask , xK_h), method S.hoogle)
                         , ((modWinMask , xK_w), method S.wikipedia)
                         , ((modWinMask , xK_z), S.promptSearchBrowser defaultXPConfig "zimlauncher" zim)
                         ]


zim = S.searchEngine "zim" "/home/nesaro/agenda/zim/ "


-- , ((modm .|. shiftMask, xK_g), raise (className =? "Firefox"))
--  , ((modm .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))
--
