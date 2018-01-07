# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="chris"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=/home/chris/dotfiles/.oh-my-zsh/

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git common-aliases fasd colored-man-pages linuxverboselib mykeybindings zsh-autosuggestions zsh-syntax-highlighting)

# User configuration

#export PATH=$HOME/.local/bin:$HOME/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"


# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
alias vim="nvim"
#alias emacsp="env HOME=$HOME/dotfiles/emacs/evilvanilla emacs -nw"
alias emacsp="emacsclient -t"

ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh

source ~/dotfiles/fzf/key-bindings.zsh
source ~/dotfiles/zsh/zce.zsh/zce.zsh
bindkey "" zce
source /usr/share/fzf/completion.zsh
export TERM=xterm-256color

alias l="ll"
alias ll="exa -l --group-directories-first --sort=extension"
alias r="ranger"
alias grep="rg"

source /usr/share/zsh/scripts/zplug/init.zsh
zplug "changyuheng/fz", defer:1
zplug "rupa/z", use:z.sh
zplug load

#directory based history
beepboop () {
  echo -n "$1" >> .directory_history
}

openFzfDirectoryHistory() {
  # Sort by data
  #RBUFFER=$(cat .directory_history | fzf)
  
  # Sort by frequency
  RBUFFER=$(cat .directory_history | sort | uniq -c | sort -rn | sed -e 's/\s*[0-9]*\s*//' | fzf)

  zle redisplay
	zle end-of-line;
	zle accept-line;
}

zle     -N   openFzfDirectoryHistory
bindkey '^P' openFzfDirectoryHistory
add-zsh-hook zshaddhistory beepboop

chpwd() clear
chpwd() ll

cdUpAbcxyz() {
  RBUFFER="cd ../"

  zle redisplay
	zle end-of-line;
	zle accept-line;
}

zle     -N   cdUpAbcxyz
bindkey '^K' cdUpAbcxyz

__git_files () { 
    _wanted files expl 'local files' _files     
}

source /home/chris/dotfiles/.oh-my-zsh//plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
