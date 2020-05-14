#!/bin/bash
pactl -- set-sink-volume bluez_sink.F0_5C_D5_3B_B7_AE.a2dp_sink -5%;
pactl -- set-sink-volume alsa_output.usb-0d8c_C-Media_USB_Audio_Device-00.analog-stereo -5%;
