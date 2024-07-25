;;; ahk-mode.el --- Major mode for editing AutoHotKey -*- lexical-binding: t -*-

;; Author: tu10ng
;; URL: https://github.com/tu10ng/ahk-mode
;; Version: 20240725
;; Package-Requires: ((emacs "29.0"))
;;
;; Based on work from
;; xahk-mode - Author:   Xah Lee ( http://xahlee.org/ ) - 2012
;; ahk-mode - Author:   Robert Widhopf-Fenk
;; ahk-mode - Author:   Rich Alesi (https://github.com/ralesi/ahk-mode)
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Keywords: languages, tools

;;; Commentary:

;; A major mode for editing AutoHotkey (AHK) script files.

;; To enable symbol completion, make sure you have the ahk2-lsp
;; program installed (available from github) and activate
;; `company-mode' and `eglot'.


;;; Code:
(require 'font-lock)
(require 'thingatpt)
(require 'rx)
(require 'seq)


;;; Customization

(defgroup ahk nil
  "Major mode for editing AutoHotkey script files."
  :prefix "ahk-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/tu10ng/ahk-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ahk"))

(defcustom ahk-indent-offset 4
  "indentation offset."
  :type 'integer
  :group 'ahk
  :safe #'integerp)

(defcustom ahk-electric-indent-chars  '(?})
  "this will append to electric-indent-chars."
  :type 'list
  :group 'ahk)

(defcustom ahk-chm-path ""
  "Custom path for AutoHotKey chm help file.
`ahk-mode' will try to find chm file by searching common install path.
if the search failed, user can specify the path to chm file manually."
  :type 'string)


;;; Bindings

(defvar ahk-mode-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "C-c d") #'ahk-lookup-web)
    (define-key map (kbd "C-c f") #'ahk-lookup-chm)
    (define-key map (kbd "C-c C-c") #'ahk-run-script)
    (easy-menu-define ahk-menu map "AHK Mode menu"
      '("AHK"
        ["Lookup webdocs on command" ahk-lookup-web]
        ["Lookup local documentation on command" ahk-lookup-chm]
        ["Execute script" ahk-run-script]
        ))
    map)
  "Keymap for `ahk-mode'.")


;;; rx-wrappers for ahk

;; TODO: does this need compile?
(defvar ahk--rx-bindings)

;; methods was dumped from https://www.autohotkey.com/docs/v2/Language.htm, on `index' page, click `filter' then use inspect to copy element out then use keyboard macro:
;; set-mark-command
;; tu10ng/search-and-jump-next
;; insert `aria-label'
;; newline
;; forward-char
;; kill-region
;; forward-sexp
;; forward-char
;; self-insert-command
(setq
 ahk--rx-bindings
 '((symbol (&rest x) (seq symbol-start (or x) symbol-end))
   (ws (* (any " \t")))                 ; white space
   (ws+ (+ (any " \t")))
   
   (name (symbol (seq (+ (any alpha "_")) (* (any alnum "_")))))
   (funcname name)
   (funcheader (seq bol (group-n 1 funcname) ws "(" (* nonl) ")" ws (? "\n") ws "{"))
   (keybind-header (seq bol (group-n 1 (+ nonl)) ws "::"))
   (assignment-op ":=")
   (operator (or "%" "." "?" "++" "--" "**" "-" "!" "~" "&" "*" "/" "//" "+" "-" "<<" ">>" ">>>" "&" "^" "|" (seq ws+ "." ws+) "~=" ">" "<" ">=" "<=" "=" "==" "!=" "!==" "&&" "||" "??" "?:" ":=" "+=" "-=" "*=" "/=" "//=" ".=" "|=" "&=" "^=" ">>=" "<<=" ">>>=" "=>" ","))
   (declaration (symbol "class" "extends" "global" "local" "static"))
   (keyword-operator (symbol "is" "in" "contains" "not" "and" "or"))
   (keyword (symbol "break" "case" "catch" "as" "continue" "default" "else" "finally" "for" "in" "goto" "if" "loop" "loop files" "loop parse" "loop read" "loop reg" "return" "until" "while"))
   (builtin-variables (symbol "A_AhkPath" "A_AhkVersion" "A_AllowMainWindow" "A_AppData" "A_AppDataCommon" "A_Args" "A_Clipboard" "A_ComputerName" "A_ComSpec" "A_ControlDelay" "A_CoordModeCaret" "A_CoordModeMenu" "A_CoordModeMouse" "A_CoordModePixel" "A_CoordModeToolTip" "A_Cursor" "A_DD" "A_DDD" "A_DDDD" "A_DefaultMouseSpeed" "A_Desktop" "A_DesktopCommon" "A_DetectHiddenText" "A_DetectHiddenWindows" "A_EndChar" "A_EventInfo" "A_FileEncoding" "A_HotkeyInterval" "A_HotkeyModifierTimeout" "A_Hour" "A_IconFile" "A_IconHidden" "A_IconNumber" "A_IconTip" "A_Index" "A_InitialWorkingDir" "A_Is64bitOS" "A_IsAdmin" "A_IsCompiled" "A_IsCritical" "A_IsPaused" "A_IsSuspended" "A_KeyDelay" "A_KeyDelayPlay" "A_KeyDuration" "A_KeyDurationPlay" "A_Language" "A_LastError" "A_LineFile" "A_LineNumber" "A_ListLines" "A_LoopField" "A_LoopFileAttrib" "A_LoopFileDir" "A_LoopFileExt" "A_LoopFileFullPath" "A_LoopFileName" "A_LoopFilePath" "A_LoopFileShortName" "A_LoopFileShortPath" "A_LoopFileSize" "A_LoopFileSizeKB" "A_LoopFileSizeMB" "A_LoopFileTimeAccessed" "A_LoopFileTimeCreated" "A_LoopFileTimeModified" "A_LoopReadLine" "A_LoopRegKey" "A_LoopRegName" "A_LoopRegTimeModified" "A_LoopRegType" "A_MaxHotkeysPerInterval" "A_MDay" "A_MenuMaskKey" "A_Min" "A_MM" "A_MMM" "A_MMMM" "A_Mon" "A_MouseDelay" "A_MouseDelayPlay" "A_MSec" "A_MyDocuments" "A_Now" "A_NowUTC" "A_OSVersion" "A_PriorHotkey" "A_PriorKey" "A_ProgramFiles" "A_Programs" "A_ProgramsCommon" "A_PtrSize" "A_RegView" "A_ScreenDPI" "A_ScreenHeight" "A_ScreenWidth" "A_ScriptDir" "A_ScriptFullPath" "A_ScriptHwnd" "A_ScriptName" "A_Sec" "A_SendLevel" "A_SendMode" "A_Space" "A_StartMenu" "A_StartMenuCommon" "A_Startup" "A_StartupCommon" "A_StoreCapsLockMode" "A_Tab" "A_Temp" "A_ThisFunc" "A_ThisHotkey" "A_TickCount" "A_TimeIdle" "A_TimeIdleKeyboard" "A_TimeIdleMouse" "A_TimeIdlePhysical" "A_TimeSincePriorHotkey" "A_TimeSinceThisHotkey" "A_TitleMatchMode" "A_TitleMatchModeSpeed" "A_TrayMenu" "A_UserName" "A_WDay" "A_WinDelay" "A_WinDir" "A_WorkingDir" "A_YDay" "A_Year" "A_YWeek" "A_YYYY" "False" "True" "Unset"))
   (builtin-functions (symbol "Abs" "ACos" "Array" "ASin" "ATan" "BlockInput" "Buffer" "CallbackCreate" "CallbackFree" "CaretGetPos" "Ceil" "Chr" "Class" "Click" "ClipboardAll" "ClipWait" "ComCall" "ComObjActive" "ComObjArray" "ComObjConnect" "ComObject" "ComObjFlags" "ComObjFromPtr" "ComObjGet" "ComObjQuery" "ComObjType" "ComObjValue" "ComValue" "ControlAddItem" "ControlChooseIndex" "ControlChooseString" "ControlClick" "ControlDeleteItem" "ControlFindItem" "ControlFocus" "ControlGetChecked" "ControlGetChoice" "ControlGetClassNN" "ControlGetEnabled" "ControlGetExStyle" "ControlGetFocus" "ControlGetHwnd" "ControlGetIndex" "ControlGetItems" "ControlGetPos" "ControlGetStyle" "ControlGetText" "ControlGetVisible" "ControlHide" "ControlHideDropDown" "ControlMove" "ControlSend" "ControlSendText" "ControlSetChecked" "ControlSetEnabled" "ControlSetExStyle" "ControlSetStyle" "ControlSetText" "ControlShow" "ControlShowDropDown" "CoordMode" "Cos" "Critical" "DateAdd" "DateDiff" "DetectHiddenText" "DetectHiddenWindows" "DirCopy" "DirCreate" "DirDelete" "DirExist" "DirMove" "DirSelect" "DllCall" "Download" "DriveEject" "DriveGetCapacity" "DriveGetFileSystem" "DriveGetLabel" "DriveGetList" "DriveGetSerial" "DriveGetSpaceFree" "DriveGetStatus" "DriveGetStatusCD" "DriveGetType" "DriveLock" "DriveRetract" "DriveSetLabel" "DriveUnlock" "Edit" "EditGetCurrentCol" "EditGetCurrentLine" "EditGetLine" "EditGetLineCount" "EditGetSelectedText" "EditPaste" "EnvGet" "EnvSet" "Error" "Exit" "ExitApp" "Exp" "FileAppend" "FileCopy" "FileCreateShortcut" "FileDelete" "FileEncoding" "FileExist" "FileGetAttrib" "FileGetShortcut" "FileGetSize" "FileGetTime" "FileGetVersion" "FileInstall" "FileMove" "FileOpen" "FileRead" "FileRecycle" "FileRecycleEmpty" "FileSelect" "FileSetAttrib" "FileSetTime" "Float" "Floor" "Format" "FormatTime" "GetKeyName" "GetKeySC" "GetKeyState" "GetKeyVK" "GetMethod" "GroupActivate" "GroupAdd" "GroupClose" "GroupDeactivate" "Gui" "GuiCtrlFromHwnd" "GuiFromHwnd" "HasBase" "HasMethod" "HasProp" "HotIf" "HotIfWinActive" "HotIfWinExist" "HotIfWinNotActive" "HotIfWinNotExist" "Hotkey" "Hotstring" "IL_Add" "IL_Create" "IL_Destroy" "ImageSearch" "IniDelete" "IniRead" "IniWrite" "InputBox" "InputHook" "InstallKeybdHook" "InstallMouseHook" "InStr" "Integer" "IsAlnum" "IsAlpha" "IsDigit" "IsFloat" "IsInteger" "IsLabel" "IsLower" "IsNumber" "IsObject" "IsSet" "IsSetRef" "IsSpace" "IsTime" "IsUpper" "IsXDigit" "KeyHistory" "KeyWait" "ListHotkeys" "ListLines" "ListVars" "ListViewGetContent" "Ln" "LoadPicture" "Log" "LTrim" "Map" "Max" "Menu" "MenuBar" "MenuFromHandle" "MenuSelect" "Min" "Mod" "MonitorGet" "MonitorGetCount" "MonitorGetName" "MonitorGetPrimary" "MonitorGetWorkArea" "MouseClick" "MouseClickDrag" "MouseGetPos" "MouseMove" "MsgBox" "Number" "NumGet" "NumPut" "ObjAddRef" "ObjBindMethod" "Object" "ObjFromPtr" "ObjFromPtrAddRef" "ObjGetBase" "ObjGetCapacity" "ObjHasOwnProp" "ObjOwnPropCount" "ObjOwnProps" "ObjPtr" "ObjPtrAddRef" "ObjRelease" "ObjSetBase" "ObjSetCapacity" "OnClipboardChange" "OnError" "OnExit" "OnMessage" "Ord" "OSError" "OutputDebug" "Pause" "Persistent" "PixelGetColor" "PixelSearch" "PostMessage" "ProcessClose" "ProcessExist" "ProcessGetName" "ProcessGetParent" "ProcessGetPath" "ProcessSetPriority" "ProcessWait" "ProcessWaitClose" "Random" "RegCreateKey" "RegDelete" "RegDeleteKey" "RegExMatch" "RegExReplace" "RegRead" "RegWrite" "Reload" "Round" "RTrim" "Run" "RunAs" "RunWait" "Send" "SendEvent" "SendInput" "SendLevel" "SendMessage" "SendMode" "SendPlay" "SendText" "SetCapsLockState" "SetControlDelay" "SetDefaultMouseSpeed" "SetKeyDelay" "SetMouseDelay" "SetNumLockState" "SetRegView" "SetScrollLockState" "SetStoreCapsLockMode" "SetTimer" "SetTitleMatchMode" "SetWinDelay" "SetWorkingDir" "Shutdown" "Sin" "Sleep" "Sort" "SoundBeep" "SoundGetInterface" "SoundGetMute" "SoundGetName" "SoundGetVolume" "SoundPlay" "SoundSetMute" "SoundSetVolume" "SplitPath" "Sqrt" "StatusBarGetText" "StatusBarWait" "StrCompare" "StrGet" "String" "StrLen" "StrLower" "StrPtr" "StrPut" "StrReplace" "StrSplit" "StrTitle" "StrUpper" "SubStr" "Suspend" "SysGet" "SysGetIPAddresses" "Tan" "Thread" "ToolTip" "TraySetIcon" "TrayTip" "Trim" "Type" "VarSetStrCapacity" "VerCompare" "WinActivate" "WinActivateBottom" "WinActive" "WinClose" "WinExist" "WinGetClass" "WinGetClientPos" "WinGetControls" "WinGetControlsHwnd" "WinGetCount" "WinGetExStyle" "WinGetID" "WinGetIDLast" "WinGetList" "WinGetMinMax" "WinGetPID" "WinGetPos" "WinGetProcessName" "WinGetProcessPath" "WinGetStyle" "WinGetText" "WinGetTitle" "WinGetTransColor" "WinGetTransparent" "WinHide" "WinKill" "WinMaximize" "WinMinimize" "WinMinimizeAll" "WinMinimizeAllUndo" "WinMove" "WinMoveBottom" "WinMoveTop" "WinRedraw" "WinRestore" "WinSetAlwaysOnTop" "WinSetEnabled" "WinSetExStyle" "WinSetRegion" "WinSetStyle" "WinSetTitle" "WinSetTransColor" "WinSetTransparent" "WinShow" "WinWait" "WinWaitActive" "WinWaitClose" "WinWaitNotActive"))
   (directives (symbol "#ClipboardTimeout" "#DllLoad" "#ErrorStdOut" "#HotIf" "#HotIfTimeout" "#Hotstring" "#Include" "#IncludeAgain" "#InputLevel" "#MaxThreads" "#MaxThreadsBuffer" "#MaxThreadsPerHotkey" "#NoTrayIcon" "#Requires" "#SingleInstance" "#SuspendExempt" "#UseHook" "#Warn" "#WinActivateForce"))
   (up-to-9-variables
    (seq (group-n 1 name) ws (? assignment-op ws (+? anything))
         (? "," ws (group-n 2 name) ws (? assignment-op ws (+? anything))
            (? "," ws (group-n 3 name) ws (? assignment-op ws (+? anything))
               (? "," ws (group-n 4 name) ws (? assignment-op ws (+? anything))
                  (? "," ws (group-n 5 name) ws (? assignment-op ws (+? anything))
                     (? "," ws (group-n 6 name) ws (? assignment-op ws (+? anything))
                        (? "," ws (group-n 7 name) ws (? assignment-op ws (+? anything))
                           (? "," ws (group-n 8 name) ws (? assignment-op ws (+? anything))
                              (? "," ws (group-n 9 name) ws (? assignment-op ws (+? anything))))))))))))))

(defmacro ahk-rx (&rest regexps)
  (eval `(rx-let ,ahk--rx-bindings
           (rx ,@regexps))))

(defun ahk-rx-to-string (form &optional no-group)
  (rx-let-eval ahk--rx-bindings
    (rx-to-string form no-group)))

;;; Font-lock and syntax

(defconst ahk-keys
  '("Alt" "AltDown" "AltTab" "AltTabAndMenu" "AltTabMenu" "AltTabMenuDismiss" "AltUp" "AppsKey" "BS" "BackSpace" "Browser_Back" "Browser_Favorites" "Browser_Forward" "Browser_Home" "Browser_Refresh" "Browser_Search" "Browser_Stop" "CapsLock" "Control" "Ctrl" "CtrlBreak" "CtrlDown" "CtrlUp" "Del" "Delete" "Down" "End" "Enter" "Esc" "Escape" "F1" "F10" "F11" "F12" "F13" "F14" "F15" "F16" "F17" "F18" "F19" "F2" "F20" "F21" "F22" "F23" "F24" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "Home" "Ins" "Insert" "Joy1" "Joy10" "Joy11" "Joy12" "Joy13" "Joy14" "Joy15" "Joy16" "Joy17" "Joy18" "Joy19" "Joy2" "Joy20" "Joy21" "Joy22" "Joy23" "Joy24" "Joy25" "Joy26" "Joy27" "Joy28" "Joy29" "Joy3" "Joy30" "Joy31" "Joy32" "Joy4" "Joy5" "Joy6" "Joy7" "Joy8" "Joy9" "JoyAxes" "JoyButtons" "JoyInfo" "JoyName" "JoyPOV" "JoyR" "JoyU" "JoyV" "JoyX" "JoyY" "JoyZ" "LAlt" "LButton" "LControl" "LCtrl" "LShift" "LWin" "LWinDown" "LWinUp" "Launch_App1" "Launch_App2" "Launch_Mail" "Launch_Media" "Left" "MButton" "Media_Next" "Media_Play_Pause" "Media_Prev" "Media_Stop" "NumLock" "Numpad0" "Numpad1" "Numpad2" "Numpad3" "Numpad4" "Numpad5" "Numpad6" "Numpad7" "Numpad8" "Numpad9" "NumpadAdd" "NumpadClear" "NumpadDel" "NumpadDiv" "NumpadDot" "NumpadDown" "NumpadEnd" "NumpadEnter" "NumpadHome" "NumpadIns" "NumpadLeft" "NumpadMult" "NumpadPgdn" "NumpadPgup" "NumpadRight" "NumpadSub" "NumpadUp" "PGDN" "PGUP" "Pause" "PrintScreen" "RAlt" "RButton" "RControl" "RCtrl" "RShift" "RWin" "RWinDown" "RWinUp" "Right" "ScrollLock" "Shift" "ShiftDown" "ShiftUp" "Space" "Tab" "Up" "Volume_Down" "Volume_Mute" "Volume_Up" "WheelDown" "WheelLeft" "WheelRight" "WheelUp" "XButton1" "XButton2")
  "AHK keywords for keys.")

(defvar ahk-keys-regexp (regexp-opt ahk-keys 'words))

(defvar ahk-font-lock-keywords
  `((,ahk-keys-regexp . font-lock-constant-face)
    
    (,(ahk-rx builtin-functions)
     . font-lock-builtin-face)
    
    (,(ahk-rx builtin-variables)
     . font-lock-constant-face)
    
    (,(ahk-rx (or keyword keyword-operator))
     . font-lock-keyword-face)
    
    (,(ahk-rx directives)
     . font-lock-preprocessor-face)
    
    (,(ahk-rx declaration)
     . font-lock-type-face)
    
    (,(ahk-rx (symbol (or "global" "static" "local" "for")) ws+ up-to-9-variables)
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face nil noerror)
     (3 font-lock-variable-name-face nil noerror)
     (4 font-lock-variable-name-face nil noerror)
     (5 font-lock-variable-name-face nil noerror)
     (6 font-lock-variable-name-face nil noerror)
     (7 font-lock-variable-name-face nil noerror)
     (8 font-lock-variable-name-face nil noerror)
     (9 font-lock-variable-name-face nil noerror))

    (,(ahk-rx bol funcname ws "(" ws up-to-9-variables)
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face nil noerror)
     (3 font-lock-variable-name-face nil noerror)
     (4 font-lock-variable-name-face nil noerror)
     (5 font-lock-variable-name-face nil noerror)
     (6 font-lock-variable-name-face nil noerror)
     (7 font-lock-variable-name-face nil noerror)
     (8 font-lock-variable-name-face nil noerror)
     (9 font-lock-variable-name-face nil noerror))
    
    (,(ahk-rx funcheader)
     (1 font-lock-function-name-face))
    
    (,(ahk-rx keybind-header)
     (1 font-lock-function-name-face))
    ))

(defvar ahk-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; comments
    (modify-syntax-entry ?\;  "<" syntax-table)
    (modify-syntax-entry ?\n ">"  syntax-table)
    (modify-syntax-entry ?\/  ". 14" syntax-table)
    (modify-syntax-entry ?*  ". 23b"   syntax-table)
    
    ;; string
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")
    
    ;; symbol constituents
    (modify-syntax-entry ?#  "_" syntax-table)
    (modify-syntax-entry ?_  "_" syntax-table)
    (modify-syntax-entry ?@  "_" syntax-table)
    
    ;; ` is escape
    (modify-syntax-entry ?` "\\" syntax-table)
    syntax-table)
  "Syntax table for `ahk-mode'.")


;;; indentation

(defun ahk-paren-level () (nth 0 (syntax-ppss)))
(defun ahk-in-str () (nth 3 (syntax-ppss)))
(defun ahk-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun ahk-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))

(defun ahk-looking-back-str (str)
  "Return non-nil if there's a match on the text before point and STR.
Like `looking-back' but for fixed strings rather than regexps (so
that it's not so slow)."
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun ahk-rewind-irrelevant ()
  (let ((continue t))
    (while continue
      (let ((starting (point)))
        (skip-chars-backward "[:space:]\n")
        (when (ahk-looking-back-str "*/")
          (backward-char))
        (when (ahk-in-str-or-cmnt)
          (ahk-rewind-past-str-cmnt))
        ;; Rewind until the point no longer moves
        (setq continue (/= starting (point)))))))

(defun ahk-indent-line ()
  "Indent current line.

if user defines keybind like [::Send "", we can't correctly indent. "
  (let ((indent
         (save-excursion
           (back-to-indentation)
           (let* ((level (ahk-paren-level))
                  (baseline (* level ahk-indent-offset)))
             (cond
              ((looking-at "[]})]")
               (- baseline ahk-indent-offset))
              ((looking-at "*")         ; block comment
               (1+ baseline))
              ((and (not (looking-at "{")) ; not bracket if
                    (save-excursion
                      (ahk-rewind-irrelevant)
                      (back-to-indentation)
                      (when (looking-at "}")
                        (forward-char))
                      (and (looking-at (ahk-rx-to-string
                                        '(seq (? punct) ws (symbol "if" "else"))))
                           (= level (ahk-paren-level)))))
               (+ baseline ahk-indent-offset))
              (t
               baseline))))))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))


;;; imenu support
(defconst ahk-imenu-generic-expression
  '(("Functions"   "^[ \t]*\\([^ ]+\\)(.*)[\n]{" 1)
    ("Labels"      "^[ \t]*\\([^:;]+\\):\n" 1)
    ("Keybindings" "^[ \t]*\\([^;: \t\r\n\v\f].*?\\)::" 1)
    ("Hotstrings" "^[ \t]*\\(:.*?:.*?::\\)" 1)
    ("Comments"    "^;imenu \\(.+\\)" 1))
  "imenu index for `ahk-mode'")


;;; Navigation

(defvar ahk--beginning-of-defun-re
  (ahk-rx-to-string '(or funcheader keybind-header)))

(defun ahk-beginning-of-defun (&optional arg)
  "Move backward to the beginning of defun (or similar)."
  (interactive "P")
  (or arg (setq arg 1))

  (while (and (> arg 0)
              (re-search-backward ahk--beginning-of-defun-re nil t))
    (setq arg (1- arg)))

  (while (and (< arg 0)
              (re-search-forward ahk--beginning-of-defun-re nil t))
    (setq arg (1+ arg)))

  (zerop arg))

(defun ahk-end-of-defun (&optional arg)
  "Move forward to the end of defun (or similar).

This function just searches for a `}' at the beginning of a line."
  (interactive "P")
  (or arg (setq arg 1))
  (let (found
        (ret t))
    (while (> arg 0)
      (if (re-search-forward "^}" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^}" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))


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
  "Look up current symbol in AutoHotkey's reference doc.
Launches default browser and opens the doc's url."
  (interactive)
  (let* ((name (ahk-symbol-at-point))
         (name (string-replace "#" "_" name)) ; for directive
         (url (concat "https://www.autohotkey.com/docs/v2/lib/" name ".htm")))
    (browse-url url)))

(defun ahk-lookup-chm ()
  "Look up current symbol in AutoHotkey's reference doc.
Finds the command in the internal AutoHotkey documentation."
  (interactive)
  (let* ((name (ahk-symbol-at-point))
         (name (string-replace "#" "_" name)) ; for directive
         (chm-path
          (seq-some (lambda (path)
                      (when (file-exists-p path)
                        (file-truename path)))
                    '("~/scoop/apps/autohotkey/current/v2/AutoHotkey.chm"
                      "~/appdata/local/programs/autohotkey/v2/AutoHotkey.chm"
                      "c:/Program Files/AutoHotkey/v2/AutoHotkey.chm"
                      "c:/Program Files (x86)/AutoHotkey/v2/AutoHotkey.chm")))
         (chm-path (or chm-path
                       (file-truename ahk-chm-path))))
    (if chm-path
        (when name (message "Opening help item for \"%s\"" name)
              (w32-shell-execute 1 "hh.exe"
                                 (format "ms-its:%s::/docs/lib/%s.htm"
                                         chm-path
                                         name)))
      (message "Help file could not be found, set ahk-chm-path variable."))))


;;; Symbol completion
;; depends on ahk2-lsp


;;; Utility functions


;;; Major mode

;;;###autoload
(define-derived-mode ahk-mode prog-mode "AutoHotkey"
  "Major mode for editing AutoHotkey (AHK) script files.

\\{ahk-mode-map}"
  :syntax-table ahk-mode-syntax-table
  (setq-local font-lock-defaults '(ahk-font-lock-keywords
                                   nil nil nil nil))

  (setq-local indent-line-function 'ahk-indent-line)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local beginning-of-defun-function 'ahk-beginning-of-defun)
  (setq-local end-of-defun-function 'ahk-end-of-defun)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local block-comment-start "/*")
  (setq-local block-comment-end "*/")
  (setq-local comment-use-syntax t)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local imenu-generic-expression ahk-imenu-generic-expression)
  (setq-local imenu-sort-function 'imenu--sort-by-position)
  (when (boundp 'electric-indent-chars)
    (setq-local electric-indent-chars
                (append electric-indent-chars ahk-electric-indent-chars)))

  (add-to-list 'auto-mode-alist '("\\.ahk\\'" . ahk-mode)))

;; TODO change ^!# into C- M- S-

(provide 'ahk-mode)

;;; ahk-mode.el ends here
