#Requires AutoHotkey v2.0
;taken from https://github.com/ashishpatel26/G613-Double-Typing-issue-resolved/blob/main/G613_Keyboard_Fix.ahk
;What does this script do?
;It's a workaround for broken G910  Logitech keyboards (possibly other keyboards too) whereby some keys occasionally register multiple keystrokes for one kkkkkeypress.
;The key bug appears because keyboard registers multiple keystrokes in a very short timespan even though you pressed the key only once. 
;This script makes it so the subsequent keystrokes registered in a very short timespan are ignored thus outputing the key only for the first stroke.

; To have this run on statup Win+R -> "shell:startup" -> create a shortcut of this script in there

;List all your broken keys between quotes below. I.e. if your broken keys are g and f then the line below shoud be 
;brokenKeys := "gf"
brokenKeys := ["a", "n", "s", "o", "x", "c"]

;timepan in which subsequent keystrokes should be ignored.
;In a typical scenario you yourself won't be pressing a single key faster than 5 times a second (every 200 miliseconds) so it's safe to have this number at 200.
;However, this number also determines how fast autorepeat can happen (when you hold the key).
;The smaller the number the faster the auto repeat speed of the fixed keys will be  but also the higher the chance of the key bug happening when you type  normally.
;Values higher than 80 seems work best but it might depend on you operating system. 
fixOffset := 100

;This array will hold timers for each broken key
lastTimePressed := {}

;Create timer with current time for each broken key
for(v in  brokenKeys)
{
    lastTimePressed.%v% := A_TickCount
}

;lastTimePressed := A_TickCount

;Assign a hotkey handler for each broken key
for(v in  brokenKeys)
{    
    Hotkey "*$" . v, HotKeyHandler 
}

HotKeyHandler(hk){
    ; MsgBox(hk)
    pressedKey := SubStr(hk,3)
    sinceLastPress := A_TickCount - lastTimePressed.%pressedKey%
    if (sinceLastPress > fixOffset) { ;send the hijacked key only when sufficient time has passed
        lastTimePressed.%pressedKey% := A_TickCount
        Send "{Blind}" . pressedKey
    }
}
