# Import the modules
Import-Module PSReadLine
Import-Module PSFzf
Import-Module -Name Terminal-Icons

Invoke-Expression (oh-my-posh init pwsh --config "$HOME\.config\oh-my-posh\config.json")

function config-git {
    git --git-dir="$HOME\.dotfiles" --work-tree="$HOME" @args
}

Invoke-Expression (& { (zoxide init powershell | Out-String) })

# Set-PSReadLineKeyHandler -Chord 'Ctrl+a' -Function BeginningOfLine -ViMode Insert
# Set-PSReadLineKeyHandler -Chord 'Ctrl+e' -Function MoveToEndOfLine -ViMode Insert
# Set-PSReadLineKeyHandler -Chord 'Ctrl+a' -Function BeginningOfLine -ViMode Command
# Set-PSReadLineKeyHandler -Chord 'Ctrl+e' -Function MoveToEndOfLine -ViMode Command
Set-PSReadLineKeyHandler -Chord 'Ctrl+f' -Function ForwardChar
Set-PSReadLineKeyHandler -Chord 'Ctrl+b' -Function BackwardChar
Set-PSReadLineKeyHandler -Chord 'Alt+f' -Function ForwardWord
Set-PSReadLineKeyHandler -Chord 'Alt+b' -Function BackwardWord
Set-PSReadLineKeyHandler -Chord 'Alt+n' -Function NextHistory
Set-PSReadLineKeyHandler -Chord 'Alt+p' -Function PreviousHistory
Set-PSReadLineKeyHandler -Chord 'Alt+F' -Function SelectForwardWord
Set-PSReadLineKeyHandler -Chord 'Alt+B' -Function SelectBackwardWord

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView

# Set up global key bindings (e.g., Ctrl+T for files, Ctrl+R for history)
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

# Setup tab completion
Set-PSReadLineKeyHandler -Key Tab -ScriptBlock { Invoke-FzfTabCompletion }
Set-PsFzfOption -TabExpansion

