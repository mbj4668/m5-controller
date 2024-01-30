# m5-controller

Home automation controller in erlang on M5 stack.

Uses AtomVM with AtomVM support for M5
(https://github.com/pguyot/atomvm_m5).

Controls Shelly Plugs using a very simple menu system on the M5 stack.

The configuraion of the Wifi settings and the Shelly Plugs names and
IP addresses are in the code, in `main.erl`.

## Build instructions

Follow the installation instructions in
https://github.com/pguyot/atomvm_m5.

Before running `idf.py build` do:

```shell
cd /home/mbj/src/AtomVM/src/platforms/esp32
# optionally
idf.py menuconfig
# re-generate CMakeFiles
idf.py reconfigure
# build the vm
idf.py build

# create an image w/ bootloader, partition-table and vm
build/mkimage.sh

# if the device is newly created, and the user is not part of correct group...
sudo chmod a+rw /dev/ttyACM0

esptool.py --chip esp32 --port /dev/ttyACM0 erase_flash

build/flashimage.sh -p /dev/ttyACM0
```
