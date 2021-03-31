# run: powershell -executionpolicy bypass -File .\window_setup.ps1
git clone --bare https://github.com/ahmed-shariff/dotfiles.git $env:HOME/.dotfiles
git --git-dir=$env:HOME/.dotfiles --work-tree=$env:HOME checkout
reg import $env:HOME\\.setup\\add_aliad.reg
git --git-dir=$env:HOME/.dotfiles --work-tree=$env:HOME config --local status.showUntrackedFiles no
cd $env:HOME
git --git-dir=$env:HOME/.dotfiles --work-tree=$env:HOME submodule update --init
git --git-dir=$env:HOME/.dotfiles --work-tree=$env:HOME update-index --skip-worktree $env:HOME/.emacs.d/customFiles/configurations.el
