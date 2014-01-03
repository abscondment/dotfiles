# dotfiles

Simple initialization of user profile and various configuration files.

## secrets

To check in new secrets, replace the existing encrypted file:

    tar cvf secrets.tar secrets
    gpg -ca --force-mdc secrets.tar
