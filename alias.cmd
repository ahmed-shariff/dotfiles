@echo off

:: from https://stackoverflow.com/questions/20530996/aliases-in-windows-command-prompt

:: Commands

set _git_home="%HOME%\.dotfiles\"
DOSKEY ls=dir /B
DOSKEY alias=notepad %HOME%\alias.cmd
DOSKEY config-git=git --git-dir="%HOME%\.dotfiles" --work-tree="%HOME%" $*
