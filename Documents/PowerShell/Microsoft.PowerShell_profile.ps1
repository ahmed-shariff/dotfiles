Invoke-Expression (oh-my-posh init pwsh --config "$HOME\.config\oh-my-posh\config.json")

function config-git {
    git --git-dir="$HOME\.dotfiles" --work-tree="$HOME" @args
}

Invoke-Expression (& { (zoxide init powershell | Out-String) })
