source $HOME/.bashrc

# Setting PATH for Python 3.6
# The original version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH

if [ -e /home/bribera/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bribera/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
