# (Re)Setting Gnome Preferences
1. clone a current release of [workspace-grid](https://github.com/zakkak/workspace-grid) and `make install`

2. [Switch only windows within workspace](https://askubuntu.com/questions/464946/force-alt-tab-to-switch-only-on-current-workspace-in-gnome-shell/759740#759740)

3. Orientation Settings

> $ gsettings set org.gnome.settings-daemon.peripherals.touchscreen orientation-lock true
> $ gsettings set org.gnome.settings-daemon.plugins.orientation active false
> $ sudo systemctl stop iio-sensor-proxy.service
> $ sudo systemctl disable iio-sensor-proxy.service
> $ sudo apt-get purge -y iio-sensor-proxy
