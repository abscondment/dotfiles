source $HOME/.bashrc
if [ -e /Users/brendan/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/brendan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Setting PATH for Python 3.6
# The original version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH
