Invoke-Expression (oh-my-posh init pwsh --config "$HOME\.config\oh-my-posh\config.json")

# rebind keys
Set-PSReadlineOption -EditMode vi
Set-PSReadLineOption -ViModeIndicator Cursor
Set-PSReadLineOption -BellStyle Visual
Set-PSReadLineKeyHandler -Chord 'Alt+q' -Function ViCommandMode
Set-PSReadLineKeyHandler -Chord 'Ctrl+a' -Function BeginningOfLine -ViMode Insert
Set-PSReadLineKeyHandler -Chord 'Ctrl+e' -Function MoveToEndOfLine -ViMode Insert
Set-PSReadLineKeyHandler -Chord 'Ctrl+a' -Function BeginningOfLine -ViMode Command
Set-PSReadLineKeyHandler -Chord 'Ctrl+e' -Function MoveToEndOfLine -ViMode Command
Set-PSReadLineKeyHandler -Chord 'Ctrl+f' -Function ForwardChar
Set-PSReadLineKeyHandler -Chord 'Ctrl+b' -Function BackwardChar
Set-PSReadLineKeyHandler -Chord 'Alt+f' -Function ForwardWord
Set-PSReadLineKeyHandler -Chord 'Alt+b' -Function BackwardWord
Set-PSReadLineKeyHandler -Chord 'Alt+n' -Function NextHistory
Set-PSReadLineKeyHandler -Chord 'Alt+p' -Function PreviousHistory
Set-PSReadLineKeyHandler -Chord 'Alt+F' -Function SelectForwardWord
Set-PSReadLineKeyHandler -Chord 'Alt+B' -Function SelectBackwardWord

Set-PSReadLineOption -PredictionViewStyle ListView
Import-Module -Name Terminal-Icons

