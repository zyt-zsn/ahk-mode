;;; ahk-mode.el --- Major mode for editing AutoHotKey -*- lexical-binding: t -*-

;; Copyright (C) conosuba~

;; Author: tu10ng
;; URL: https://github.com/tu10ng/ahk-mode
;; Version: 1.0.0
;; Keywords: ahk, AutoHotkey, hotkey, keyboard shortcut, automation
;; Package-Requires: ((emacs "30.0"))

;; Based on work from
;; xahk-mode - Author:   Xah Lee ( http://xahlee.org/ ) - 2012
;; ahk-mode - Author:   Robert Widhopf-Fenk
;; ahk-mode - Author:   Rich Alesi (https://github.com/ralesi/ahk-mode)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or version 2.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing AutoHotkey (AHK) script.  You should use this mode with eglot/lsp-mode and ahk2-lsp.

;;; Code:

;;; Requirements

(require 'thingatpt)
(require 'rx)
(require 'smie)

;;; Customization
(defgroup ahk nil
  "Major mode for editing AutoHotkey script."
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/tu10ng/ahk-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ahk"))

(defcustom ahk-path nil
  "Custom path for AutoHotkey executable and related files."
  :type 'string)


;;; Bindings

(defvar ahk-mode-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "C-c d") #'ahk-lookup-web)
    (define-key map (kbd "C-c h") #'ahk-lookup-chm)
    (define-key map (kbd "C-c C-c") #'ahk-run-script)
    (easy-menu-define ahk-menu map "AHK Mode menu"
      '("AHK"
        ["Lookup webdocs on command" ahk-lookup-web]
        ["Lookup local documentation on command" ahk-lookup-chm]
        ["Execute script" ahk-run-script]
        ))
    map)
  "Keymap for `ahk-mode'.")


;;; Font-lock and syntax

(defvar ahk-commands
  '("Abort" "AboveNormal" "Add" "All" "Alnum" "Alpha" "AltSubmit" "AlwaysOnTop" "And" "Asc" "AutoSize" "AutoTrim" "Background" "BackgroundTrans" "BelowNormal" "Between" "BitAnd" "BitNot" "BitOr" "BitShiftLeft" "BitShiftRight" "BitXOr" "BlockInput" "Border" "Bottom" "Break" "Button" "Buttons" "ByRef" "Cancel" "Capacity" "Caption" "Catch" "Ceil" "Center" "Check" "Check3" "Checkbox" "Checked" "CheckedGray" "Checks" "Choose" "ChooseString" "Chr" "Click" "ClipWait" "Close" "Color" "ComboBox" "Contains" "Continue" "Control" "ControlClick" "ControlFocus" "ControlGet" "ControlGetFocus" "ControlGetPos" "ControlGetText" "ControlList" "ControlMove" "ControlSend" "ControlSendRaw" "ControlSetText" "CoordMode" "Count" "Critical" "DDL" "Date" "DateTime" "Days" "Default" "Delete" "DeleteAll" "Delimiter" "Deref" "Destroy" "DetectHiddenText" "DetectHiddenWindows" "Digit" "Disable" "Disabled" "Displays" "Drive" "DriveGet" "DriveSpaceFree" "DropDownList" "Edit" "Eject" "Else" "Enable" "Enabled" "EnvAdd" "EnvDiv" "EnvGet" "EnvMult" "EnvSet" "EnvSub" "EnvUpdate" "Error" "ExStyle" "Exist" "Exit" "ExitApp" "Exp" "Expand" "FileAppend" "FileCopy" "FileCopyDir" "FileCreateDir" "FileCreateShortcut" "FileDelete" "FileEncoding" "FileGetAttrib" "FileGetShortcut" "FileGetSize" "FileGetTime" "FileGetVersion" "FileInstall" "FileMove" "FileMoveDir" "FileOpen" "FileRead" "FileReadLine" "FileRecycle" "FileRecycleEmpty" "FileRemoveDir" "FileSelectFile" "FileSelectFolder" "FileSetAttrib" "FileSetTime" "FileSystem" "Finally" "First" "Flash" "Float" "FloatFast" "Floor" "Focus" "Font" "For" "Format" "FormatTime" "GetKeyState" "Gosub" "Goto" "Grid" "Group" "GroupActivate" "GroupAdd" "GroupBox" "GroupClose" "GroupDeactivate" "Gui" "GuiClose" "GuiContextMenu" "GuiControl" "GuiControlGet" "GuiDropFiles" "GuiEscape" "GuiSize" "HKCC" "HKCR" "HKCU" "HKEY_CLASSES_ROOT" "HKEY_CURRENT_CONFIG" "HKEY_CURRENT_USER" "HKEY_LOCAL_MACHINE" "HKEY_USERS" "HKLM" "HKU" "HScroll" "Hdr" "Hidden" "Hide" "High" "Hotkey" "Hours" "ID" "IDLast" "Icon" "IconSmall" "If" "IfEqual" "IfExist" "IfGreater" "IfGreaterOrEqual" "IfInString" "IfLess" "IfLessOrEqual" "IfMsgBox" "IfNotEqual" "IfWinActive" "IfWinExist" "IfWinNotActive" "IfWinNotExist" "Ignore" "ImageList" "ImageSearch" "In" "IniDelete" "IniRead" "IniWrite" "Input" "InputBox" "Integer" "IntegerFast" "Interrupt" "Is" "Join" "KeyHistory" "KeyWait" "LTrim" "Label" "LastFound" "LastFoundExist" "Left" "Limit" "Lines" "List" "ListBox" "ListHotkeys" "ListLines" "ListVars" "ListView" "Ln" "Lock" "Log" "Logoff" "Loop" "Low" "Lower" "Lowercase" "MainWindow" "Margin" "MaxSize" "Maximize" "MaximizeBox" "Menu" "MinMax" "MinSize" "Minimize" "MinimizeBox" "Minutes" "Mod" "MonthCal" "Mouse" "MouseClick" "MouseClickDrag" "MouseGetPos" "MouseMove" "Move" "MsgBox" "Multi" "NA" "No" "NoActivate" "NoDefault" "NoHide" "NoIcon" "NoMainWindow" "NoSort" "NoSortHdr" "NoStandard" "NoTab" "NoTimers" "Normal" "Not" "Number" "Off" "Ok" "On" "OnExit" "Or" "OutputDebug" "OwnDialogs" "Owner" "Parse" "Password" "Pause" "Pic" "Picture" "Pixel" "PixelGetColor" "PixelSearch" "Pos" "PostMessage" "Pow" "Priority" "Process" "ProcessName" "Progress" "REG_BINARY" "REG_DWORD" "REG_EXPAND_SZ" "REG_MULTI_SZ" "REG_SZ" "RGB" "RTrim" "Radio" "Random" "Range" "Read" "ReadOnly" "Realtime" "Redraw" "RegDelete" "RegRead" "RegWrite" "Region" "Relative" "Reload" "Rename" "Report" "Resize" "Restore" "Retry" "Return" "Right" "Round" "Run" "RunAs" "RunWait" "Screen" "Seconds" "Section" "See" "Send" "SendInput" "SendLevel" "SendMessage" "SendMode" "SendPlay" "SendRaw" "Serial" "SetBatchLines" "SetCapslockState" "SetControlDelay" "SetDefaultMouseSpeed" "SetEnv" "SetFormat" "SetKeyDelay" "SetLabel" "SetMouseDelay" "SetNumlockState" "SetRegView" "SetScrollLockState" "SetStoreCapslockMode" "SetTimer" "SetTitleMatchMode" "SetWinDelay" "SetWorkingDir" "ShiftAltTab" "Show" "Shutdown" "Sin" "Single" "Sleep" "Slider" "Sort" "SortDesc" "SoundBeep" "SoundGet" "SoundGetWaveVolume" "SoundPlay" "SoundSet" "SoundSetWaveVolume" "SplashImage" "SplashTextOff" "SplashTextOn" "SplitPath" "Sqrt" "Standard" "Status" "StatusBar" "StatusBarGetText" "StatusBarWait" "StatusCD" "StringCaseSense" "StringGetPos" "StringLeft" "StringLen" "StringLower" "StringMid" "StringReplace" "StringRight" "StringSplit" "StringTrimLeft" "StringTrimRight" "StringUpper" "Style" "Submit" "Suspend" "SysGet" "SysMenu" "Tab" "Tab2" "TabStop" "Tan" "Text" "Theme" "Thread" "Throw" "Tile" "Time" "Tip" "ToggleCheck" "ToggleEnable" "ToolTip" "ToolWindow" "Top" "Topmost" "TransColor" "Transform" "Transparent" "Tray" "TrayTip" "TreeView" "Trim" "Try" "TryAgain" "Type" "UnCheck" "Unicode" "Unlock" "Until" "UpDown" "Upper" "Uppercase" "UrlDownloadToFile" "UseErrorLevel" "VScroll" "Var" "Vis" "VisFirst" "Visible" "Wait" "WaitClose" "WantCtrlA" "WantF2" "WantReturn" "While" "WinActivate" "WinActivateBottom" "WinClose" "WinGet" "WinGetActiveStats" "WinGetActiveTitle" "WinGetClass" "WinGetPos" "WinGetText" "WinGetTitle" "WinHide" "WinKill" "WinMaximize" "WinMenuSelectItem" "WinMinimize" "WinMinimizeAll" "WinMinimizeAllUndo" "WinMove" "WinRestore" "WinSet" "WinSetTitle" "WinShow" "WinWait" "WinWaitActive" "WinWaitClose" "WinWaitNotActive" "Wrap" "Xdigit" "Yes" "ahk_class" "ahk_group" "ahk_id" "ahk_pid" "bold" "global" "italic" "local" "norm" "static" "strike" "underline" "xm" "xp" "xs" "ym" "yp" "ys")
  "AHK keywords.")

(defvar ahk-directives
  '("#ClipboardTimeout" "#CommentFlag" "#ErrorStdOut" "#EscapeChar" "#HotkeyInterval" "#HotkeyModifierTimeout" "#Hotstring" "#If" "#IfTimeout" "#IfWinActive" "#IfWinExist" "#Include" "#InputLevel" "#InstallKeybdHook" "#InstallMouseHook" "#KeyHistory" "#LTrim" "#MaxHotkeysPerInterval" "#MaxMem" "#MaxThreads" "#MaxThreadsBuffer" "#MaxThreadsPerHotkey" "#MenuMaskKey" "#NoEnv" "#NoTrayIcon" "#Persistent" "#SingleInstance" "#UseHook" "#Warn" "#WinActivateForce")
  "AHK directives")

(defvar ahk-functions
  '("ACos" "ASin" "ATan" "Abs" "Asc" "Ceil" "Chr" "ComObjActive" "ComObjArray" "ComObjConnect" "ComObjCreate" "ComObjEnwrap" "ComObjError" "ComObjFlags" "ComObjGet" "ComObjMissing" "ComObjParameter" "ComObjQuery" "ComObjType" "ComObjUnwrap" "ComObjValue" "Cos" "DllCall" "Exp" "FileExist" "Floor" "Func" "Functions" "GetKeyName" "GetKeySC" "GetKeyState" "GetKeyVK" "IL_Add" "IL_Create" "IL_Destroy" "InStr" "IsByRef" "IsFunc" "IsLabel" "IsObject" "LV_Add" "LV_Delete" "LV_DeleteCol" "LV_GetCount" "LV_GetNext" "LV_GetText" "LV_Insert" "LV_InsertCol" "LV_Modify" "LV_ModifyCol" "LV_SetImageList" "Ln" "Log" "Mod" "NumGet" "NumPut" "OnMessage" "RegExMatch" "RegExReplace" "RegisterCallback" "Round" "SB_SetIcon" "SB_SetParts" "SB_SetText" "Sin" "Sqrt" "StrGet" "StrLen" "StrPut" "SubStr" "TV_Add" "TV_Delete" "TV_Get" "TV_GetChild" "TV_GetCount" "TV_GetNext" "TV_GetParent" "TV_GetPrev" "TV_GetSelection" "TV_GetText" "TV_Modify" "Tan" "VarSetCapacity" "WinActive" "WinExist")
  "AHK functions.")

(defvar ahk-variables
  '("A_AhkPath" "A_AhkVersion" "A_AppData" "A_AppDataCommon" "A_AutoTrim" "A_BatchLines" "A_CaretX" "A_CaretY" "A_ComputerName" "A_ControlDelay" "A_Cursor" "A_DD" "A_DDD" "A_DDDD" "A_DefaultMouseSpeed" "A_Desktop" "A_DesktopCommon" "A_DetectHiddenText" "A_DetectHiddenWindows" "A_EndChar" "A_EventInfo" "A_ExitReason" "A_FileEncoding" "A_FormatFloat" "A_FormatInteger" "A_Gui" "A_GuiControl" "A_GuiControlEvent" "A_GuiEvent" "A_GuiHeight" "A_GuiWidth" "A_GuiX" "A_GuiY" "A_Hour" "A_IPAddress1" "A_IPAddress2" "A_IPAddress3" "A_IPAddress4" "A_ISAdmin" "A_IconFile" "A_IconHidden" "A_IconNumber" "A_IconTip" "A_Index" "A_Is64bitOS" "A_IsAdmin" "A_IsCompiled" "A_IsCritical" "A_IsPaused" "A_IsSuspended" "A_IsUnicode" "A_KeyDelay" "A_Language" "A_LastError" "A_LineFile" "A_LineNumber" "A_LoopField" "A_LoopFileAttrib" "A_LoopFileDir" "A_LoopFileExt" "A_LoopFileFullPath" "A_LoopFileLongPath" "A_LoopFileName" "A_LoopFileName," "A_LoopFileShortName" "A_LoopFileShortPath" "A_LoopFileSize" "A_LoopFileSizeKB" "A_LoopFileSizeMB" "A_LoopFileTimeAccessed" "A_LoopFileTimeCreated" "A_LoopFileTimeModified" "A_LoopReadLine" "A_LoopRegKey" "A_LoopRegName" "A_LoopRegName," "A_LoopRegSubkey" "A_LoopRegTimeModified" "A_LoopRegType" "A_MDAY" "A_MM" "A_MMM" "A_MMMM" "A_MSec" "A_Min" "A_Mon" "A_MouseDelay" "A_MyDocuments" "A_Now" "A_NowUTC" "A_NumBatchLines" "A_OSType" "A_OSVersion" "A_PriorHotkey" "A_PriorKey" "A_ProgramFiles" "A_Programs" "A_ProgramsCommon" "A_PtrSize" "A_RegView" "A_ScreenDPI" "A_ScreenHeight" "A_ScreenWidth" "A_ScriptDir" "A_ScriptFullPath" "A_ScriptHwnd" "A_ScriptName" "A_Sec" "A_Space" "A_StartMenu" "A_StartMenuCommon" "A_Startup" "A_StartupCommon" "A_StringCaseSense" "A_Tab" "A_Temp" "A_ThisFunc" "A_ThisHotkey" "A_ThisLabel" "A_ThisMenu" "A_ThisMenuItem" "A_ThisMenuItemPos" "A_TickCount" "A_TimeIdle" "A_TimeIdlePhysical" "A_TimeSincePriorHotkey" "A_TimeSinceThisHotkey" "A_TitleMatchMode" "A_TitleMatchModeSpeed" "A_UserName" "A_WDay" "A_WinDelay" "A_WinDir" "A_WorkingDir" "A_YDay" "A_YEAR" "A_YWeek" "A_YYYY" "Clipboard" "ClipboardAll" "ComSpec" "ErrorLevel" "False" "ProgramFiles" "True" "Variable")
  "AHK variables.")

(defvar ahk-keys
  '("Alt" "AltDown" "AltTab" "AltTabAndMenu" "AltTabMenu" "AltTabMenuDismiss" "AltUp" "AppsKey" "BS" "BackSpace" "Browser_Back" "Browser_Favorites" "Browser_Forward" "Browser_Home" "Browser_Refresh" "Browser_Search" "Browser_Stop" "CapsLock" "Control" "Ctrl" "CtrlBreak" "CtrlDown" "CtrlUp" "Del" "Delete" "Down" "End" "Enter" "Esc" "Escape" "F1" "F10" "F11" "F12" "F13" "F14" "F15" "F16" "F17" "F18" "F19" "F2" "F20" "F21" "F22" "F23" "F24" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "Home" "Ins" "Insert" "Joy1" "Joy10" "Joy11" "Joy12" "Joy13" "Joy14" "Joy15" "Joy16" "Joy17" "Joy18" "Joy19" "Joy2" "Joy20" "Joy21" "Joy22" "Joy23" "Joy24" "Joy25" "Joy26" "Joy27" "Joy28" "Joy29" "Joy3" "Joy30" "Joy31" "Joy32" "Joy4" "Joy5" "Joy6" "Joy7" "Joy8" "Joy9" "JoyAxes" "JoyButtons" "JoyInfo" "JoyName" "JoyPOV" "JoyR" "JoyU" "JoyV" "JoyX" "JoyY" "JoyZ" "LAlt" "LButton" "LControl" "LCtrl" "LShift" "LWin" "LWinDown" "LWinUp" "Launch_App1" "Launch_App2" "Launch_Mail" "Launch_Media" "Left" "MButton" "Media_Next" "Media_Play_Pause" "Media_Prev" "Media_Stop" "NumLock" "Numpad0" "Numpad1" "Numpad2" "Numpad3" "Numpad4" "Numpad5" "Numpad6" "Numpad7" "Numpad8" "Numpad9" "NumpadAdd" "NumpadClear" "NumpadDel" "NumpadDiv" "NumpadDot" "NumpadDown" "NumpadEnd" "NumpadEnter" "NumpadHome" "NumpadIns" "NumpadLeft" "NumpadMult" "NumpadPgdn" "NumpadPgup" "NumpadRight" "NumpadSub" "NumpadUp" "PGDN" "PGUP" "Pause" "PrintScreen" "RAlt" "RButton" "RControl" "RCtrl" "RShift" "RWin" "RWinDown" "RWinUp" "Right" "ScrollLock" "Shift" "ShiftDown" "ShiftUp" "Space" "Tab" "Up" "Volume_Down" "Volume_Mute" "Volume_Up" "WheelDown" "WheelLeft" "WheelRight" "WheelUp" "XButton1" "XButton2")
  "AHK keywords for keys.")

(defvar ahk-operator-words
  '("AND" "NOT" "OR")
  "AHK operator words.")

(defvar ahk-operators
  '("\\!" "!=" "&" "&&" "&=" "*" "**" "*=" "+" "++" "+=" "-" "--" "-=" "." "." ".=" "/" "//" "//=" "/=" ":=" "<" "<<" "<<=" "<=" "<>" "=" "==" ">" ">=" ">>" ">>=" "?:" "^" "^=" "|" "|=" "||" "~" "~=" ",")
  "AHK operators.")

(defvar ahk-commands-regexp (regexp-opt ahk-commands 'words))
(defvar ahk-functions-regexp (regexp-opt ahk-functions 'words))
(defvar ahk-directives-regexp (regexp-opt ahk-directives 'words))
(defvar ahk-variables-regexp (regexp-opt ahk-variables 'words))
(defvar ahk-keys-regexp (regexp-opt ahk-keys 'words))
(defvar ahk-operator-words-regexp (regexp-opt ahk-operator-words 'words))
(defvar ahk-operators-regexp (regexp-opt ahk-operators))

(defvar ahk-double-quote-string-re "[\"]\\(\\\\.\\|[^\"\n]\\)*[\"]"
  "Regexp used to match a double-quoted string literal")

(defvar ahk-single-quote-string-re "[']\\(\\\\.\\|[^'\n]\\)*[']"
  "Regexp used to match a single-quoted string literal")

(defvar ahk-font-lock-keywords
  `(("\\s-*;.*$" . font-lock-comment-face)
    ("^/\\*\\(.*\r?\n\\)*\\(\\*/\\)?" . font-lock-comment-face) ; block comments
    
    ;; lLTrim0 usage
    ;; ("(LTrim0\\(.*\n\\)+" . font-lock-string-face)
    (,ahk-double-quote-string-re . font-lock-string-face)
    (,ahk-single-quote-string-re . font-lock-string-face)
    
    ;; bindings
    ("^\\([^\t\n:=]+\\)::" . (1 font-lock-constant-face))
    
    ;; labels
    ("^\\([^\t\n :=]+\\):[^=]" . (1 font-lock-doc-face))
    
    ;; return
    ("\\<\\([Rr]eturn\\)\\>" . font-lock-warning-face)
    
    ;; functions
    ("^\\([^\t\n (]+\\)\\((.*)\\)" . (1 font-lock-function-name-face))
    
    ;; variables
    ("%[^% ]+%" . font-lock-variable-name-face)
    (,ahk-commands-regexp . font-lock-builtin-face)
    (,ahk-functions-regexp . font-lock-builtin-face)
    (,ahk-directives-regexp . font-lock-preprocessor-face)
    (,ahk-variables-regexp . font-lock-variable-name-face)
    (,ahk-keys-regexp . font-lock-constant-face)
    (,ahk-operator-words-regexp . font-lock-keyword-face)
    (,ahk-operators-regexp . font-lock-operator-face)
    ;; note: order matters
    ))

(defvar ahk-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; these are also allowed in variable names
    (modify-syntax-entry ?#  "w" syntax-table)
    (modify-syntax-entry ?_  "w" syntax-table)
    (modify-syntax-entry ?@  "w" syntax-table)
    ;; some additional characters used in paths and switches
    (modify-syntax-entry ?\\  "w" syntax-table)
    (modify-syntax-entry ?\;  "< b" syntax-table)
    ;; for multiline comments
    (modify-syntax-entry ?\/  ". 14" syntax-table)
    (modify-syntax-entry ?*  ". 23"   syntax-table)
    ;; New line
    (modify-syntax-entry ?\n "> b"  syntax-table)
    ;; ` is escape
    (modify-syntax-entry ?` "\\" syntax-table)
    syntax-table)
  "Syntax table for `ahk-mode'.")


;;; indentation
;; depends on `smie' (Simple Minded Indentation Engine)


;;; imenu support
(defconst ahk-imenu-generic-expression
  '(("Functions"   "^[ \t]*\\([^ ]+\\)(.*)[\n]{" 1)
    ("Labels"      "^[ \t]*\\([^:;]+\\):\n" 1)
    ("Keybindings" "^[ \t]*\\([^;: \t\r\n\v\f].*?\\)::" 1)
    ("Hotstrings" "^[ \t]*\\(:.*?:.*?::\\)" 1)
    ("Comments"    "^;imenu \\(.+\\)" 1))
  "imenu index for `ahk-mode'")


;;; Navigation

;; TODO


;;; Action

(defun ahk-run-script ()
  "Run the ahk-script in the current buffer."
  (interactive)
  (let ((file (shell-quote-argument
               (replace-regexp-in-string " " "\ "
                                         (replace-regexp-in-string "\/" "\\\\" (buffer-file-name) t t)))))
    (message "Executing script %s" file)
    (w32-shell-execute "open" file)))

(defun ahk-symbol-at-point ()
  "Determine command at point, and prompt if nothing found."
  (let ((command (or (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))
                       (thing-at-point 'symbol))
                     (read-string "Command: "))))
    command))

(defun ahk-lookup-web ()
  "Look up current word in AutoHotkey's reference doc.
Launches default browser and opens the doc's url."
  (interactive)
  (let* ((name (ahk-symbol-at-point))
         (name (string-replace "#" "_" name))
         (url (concat "https://www.autohotkey.com/docs/v2/lib/" name ".htm")))
    (browse-url url)))

(defun ahk-lookup-chm ()
  "Look up current word in AutoHotkey's reference doc.
Finds the command in the internal AutoHotkey documentation."
  (interactive)
  (let* ((name (ahk-symbol-at-point))
         (name (string-replace "#" "_" name))
         (chm-path
          (or (and (file-exists-p "~/scoop/apps/autohotkey/current/v2/AutoHotkey.chm")
                   (file-truename "~/scoop/apps/autohotkey/current/v2/AutoHotkey.chm"))
              ;; (and (file-exists-p "c:/Program Files (x86)/AutoHotkey/AutoHotkey.chm")
              ;;      "c:/Program Files (x86)/AutoHotkey/AutoHotkey.chm")
              ;; (and (file-exists-p "c:/Program Files/AutoHotkey/AutoHotkey.chm")
              ;;      "c:/Program Files/AutoHotkey/AutoHotkey.chm")
              (and (file-exists-p (concat ahk-path "/AutoHotkey.chm"))
                   (concat ahk-path "/AutoHotkey.chm")))))
    (if chm-path
        (when name (message "Opening help item for \"%s\"" name)
              (w32-shell-execute 1 "hh.exe"
                                 (format
                                  "ms-its:%s::/docs/lib/%s.htm"
                                  chm-path name)))
      (message "Help file could not be found, set ahk-path variable."))))


;;; Symbol completion
;; depends on ahk2-lsp


;;; Utility functions


;;; Major mode

;;;###autoload
(define-derived-mode ahk-mode prog-mode "AutoHotkey"
  "Major mode for editing AutoHotkey (AHK) script files.

\\{ahk-mode-map}"
  :syntax-table ahk-mode-syntax-table
  (setq-local comment-start "; ")
  (setq-local comment-start-skip ";+ *")

  (setq-local block-comment-start "/*")
  (setq-local block-comment-end "*/")
  (setq-local block-comment-left " * ")
  (setq-local block-comment-right " *")
  (setq-local block-comment-top-right "")
  (setq-local block-comment-bot-left " ")
  (setq-local block-comment-char ?*)

  ;; TODO: comment indentation is not correct
  (smie-setup nil #'ignore)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)

  ;; TODO: beginning-of-defun

  (setq-local font-lock-defaults `(,ahk-font-lock-keywords
                                   nil nil))

  (setq-local imenu-generic-expression ahk-imenu-generic-expression)
  (setq-local imenu-sort-function 'imenu--sort-by-position)
  
  (add-to-list 'auto-mode-alist '("\\.ahk\\'" . ahk-mode)))

;; TODO change ^!# into C- M- S-

(provide 'ahk-mode)

;;; ahk-mode.el ends here
