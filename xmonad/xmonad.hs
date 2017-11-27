import Data.Monoid
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Map as M
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D
import qualified XMonad.StackSet as W

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm              , xK_Return), spawn "dmenu_run -fn 'Monospace-18'") -- launch dmenu
    , ((modm              , xK_y     ), windowGo L False) -- actually I press Option+h; remapped to modm+y by Karabiner
    , ((modm              , xK_l     ), windowGo R False)
    , ((modm              , xK_j     ), windowGo D False)
    , ((modm              , xK_k     ), windowGo U False)
    , ((modm              , xK_period), moveTo Next NonEmptyWS)
    , ((modm              , xK_Right ), moveTo Next NonEmptyWS)
    , ((modm              , xK_comma ), moveTo Prev NonEmptyWS)
    , ((modm              , xK_Left  ), moveTo Prev NonEmptyWS)
    , ((modm .|. shiftMask, xK_c     ), kill)                                 -- close focused window
    ]

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))    -- mod-button1, Set the window to floating mode and move by dragging
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio -- default tiling algorithm partitions the screen into two panes
    nmaster = 1                        -- The default number of windows in the master pane
    ratio   = 1/2                      -- Default proportion of screen occupied by master pane
    delta   = 3/100                    -- Percent of screen to increment by when resizing panes

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook = composeAll
               [ className =? "Searchbox" --> doFloat ]

------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
navigation2DConfig = def { layoutNavigation = [("Full", centerNavigation)] }

defaults = ewmh def
  {
  -- simple stuff
    modMask            = mod4Mask -- Use super (Windows key)
  , terminal           = "lilyterm"
  , focusFollowsMouse  = True
  , clickJustFocuses   = False
  , workspaces         = ["1","2","3","4","5","6","7","8","9"]
  , borderWidth        = 1
  , normalBorderColor  = "#dddddd"
  , focusedBorderColor = "#0000ff"

  -- key bindings
  , keys               = myKeys
  , mouseBindings      = myMouseBindings

  -- hooks, layouts
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook
  , startupHook        = myStartupHook
  }

main = do
  xconf <- xmobar defaults
  let xconf' = withNavigation2DConfig navigation2DConfig xconf
  xmonad xconf'
