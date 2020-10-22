-- xmonad config used by K
-- Author: K
-- https://github.com/klobes5/xmonad-config

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Actions.WorkspaceNames as SWAP
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow (copy, copyToAll, killAllOtherCopies)
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Layout.Spacing as SP
import XMonad.Hooks.ManageHelpers (composeOne, isFullscreen, isDialog,  doFullFloat, doCenterFloat)

------------------------------------------------------------------------
---- Notes:
----
---- mod+q will reload the xmonad --recompile configuration
---- mod+shift+space will reset layout if I've messed it up with mod+period mod+comma
---- Get classname with xprop and then click on window
----
----


myManageHook = composeAll
    [   (className =? "Emoji-keyboard"  --> doCenterFloat)
      , (className =? "feh"  --> doCenterFloat)
      , (className =? "mpv"  --> doCenterFloat)
      , (className =? "Gimp"  --> doCenterFloat)
      , (className =? "Nautilus"  --> doCenterFloat)
      , (className =? "Thunar"  --> doCenterFloat)
      , (className =? "Vimb"  --> doCenterFloat)
      , (className =? "Gvim"  --> doCenterFloat)
      , (className =? "Spek"  --> doCenterFloat)
      , (className =? "Org.gnome.Maps"  --> doCenterFloat)
      , (className =? "Org.gnome.Weather.Application"  --> doCenterFloat)
      , (className =? "Display-im6.q16"  --> doCenterFloat)
      , (className =? "Vmware-netcfg"  --> doCenterFloat)
      , (className =? "Gnome-terminal"  --> doCenterFloat)
    ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig 
        --, layoutHook = avoidStrutsOn [U] $  SP.spacing 5 $ layoutHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
        , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppSort = DO.getSortByOrder
                        , ppWsSep = " "
                        , ppSep = " "
                        , ppUrgent = xmobarColor "orange" ""
                        , ppVisible = xmobarColor "orange" "" . wrap "(" ")"
                        , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                        , ppLayout   = xmobarColor "yellow" "" .
                                  (\ x -> pad $ case x of
                                            "Spacing Tall" -> "TTT"
                                            "Spacing Mirror Tall"   -> "[]="
                                            "Spacing Full"          -> "[ ]"
                                            _                      -> x
                                  )
                        }
        , focusFollowsMouse = False
        , clickJustFocuses = False
        }`additionalKeys`   -- left winkey = mod4Mask, left alt = mod1Mask
        [ ((mod1Mask, xK_BackSpace), DW.removeWorkspace)
        , ((mod1Mask, xK_a      ), DW.addWorkspacePrompt def)
         -- jump to workspace by name
        , ((mod1Mask, xK_v      ), DW.selectWorkspace def)
        , ((mod1Mask, xK_m ), windows copyToAll) -- Make focused window always visible
        , ((mod1Mask .|. shiftMask, xK_v ),  killAllOtherCopies) -- Toggle window state back
        --, ((mod1Mask, xK_m      ), DW.withWorkspace def (windows . copy)) --copy tile to another workspace
        , ((mod1Mask, xK_g), sendMessage ToggleGaps)  -- toggle all gaps
        , ((mod1Mask, xK_r), DW.renameWorkspace def)
        , ((mod1Mask, xK_Down), windows W.swapDown)
        , ((mod1Mask, xK_Up), windows W.swapUp)
        -- move to named workspace
        , ((mod1Mask .|. shiftMask, xK_m      ), DW.withWorkspace def (windows . W.shift)) 
        --, ((mod1Mask .|. shiftMask, xK_Left  ), SWAP.swapTo Prev) -- move entire workspace left or right 
        --, ((mod1Mask .|. shiftMask, xK_Right ), SWAP.swapTo Next)
        --, ((mod1Mask .|. shiftMask, xK_Left ),   DO.shiftTo Prev AnyWS)
        --, ((mod1Mask .|. shiftMask, xK_Right),   DO.shiftTo Next AnyWS)
        , ((mod1Mask .|. shiftMask, xK_Left ),   DO.swapWith Prev NonEmptyWS)
        , ((mod1Mask .|. shiftMask, xK_Right),   DO.swapWith Next NonEmptyWS)
        , ((mod1Mask .|. shiftMask, xK_p ),   DO.shiftTo Prev NonEmptyWS) -- move tile to left screen
        , ((mod1Mask .|. shiftMask, xK_n),   DO.shiftTo Next NonEmptyWS) --move tile to right screen
        , ((mod1Mask, xK_Left),  DO.moveTo Prev HiddenNonEmptyWS)
        , ((mod1Mask, xK_Right),  DO.moveTo Next HiddenNonEmptyWS)
        , ((mod1Mask , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
        , ((mod1Mask , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
        , ((mod1Mask, xK_b), spawn "/usr/local/bin/firefox")
        , ((mod1Mask, xK_n), spawn "/usr/bin/terminator")
        , ((mod1Mask, xK_s), sendMessage ToggleStruts)
--        , ((0, xK_F1), spawn "xmobar -x 1 .xmobarrc2 2>&1 &")
--        , ((0, xK_F11), spawn "/usr/local/bin/volume_down.sh")
--        , ((0, xK_F12), spawn "/usr/local/bin/volume_up.sh")
        -- custom scripts:
        --- translate clipboard
        , ((0, 0x1008ff4b), spawn "/usr/local/bin/notitrans") -- F4 
        -- dictionary lookup clipboard
        , ((0, 0x1008ff4a), spawn "/usr/local/bin/notidict")  -- F3
	, ((0, 0x1008ff13), spawn "/usr/local/bin/volume_up.sh")
	, ((0, 0x1008ff11), spawn "/usr/local/bin/volume_down.sh")
	, ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
    	, ((0, 0x1008ff03), spawn "/usr/local/bin/brightnessdown")
    	, ((0, 0x1008ff02), spawn "/usr/local/bin/brightnessup")
	, ((0, 0x1008fff2), spawn "eject /dev/sr0")
--        , ((0, xK_F5), spawn "/usr/local/bin/vimbopen 2>&1 &")
        , ((0, 0x1008ff16), spawn "/usr/local/bin/searchyoutube") -- F7 --  search youtube using clipboard and watch 1st video in floating mplayer window
        , ((0, 0x1008ff17), spawn "/usr/local/bin/watchyoutube") -- F9  -- watch youtube video from url clipboard in floating mplayer window
        , ((0, 0x1008ff14), spawn "/usr/bin/i3lock -i $HOME/.screenlayout/mars-curiosity.png -t") -- lockscreen
--        , ((0, xK_F2), spawn "/usr/local/bin/gnome-lynx.sh") -- open a floating lynx window from clipboard
--        , ((0 .|. controlMask, xK_k), spawn "emoji-keyboard -k")
            -- Media commands (use xev to display keysyms)
	]
