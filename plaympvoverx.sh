#!/bin/bash

# Compresses video file and plays using mpv over ssh -X connection, requires pulse server running on SSH client to forward audio to (see notes at end of file)
export PULSE_SERVER=$SSH_CLIENT_IP_LAN
ffmpeg -i "$1" -preset veryfast -s 640x480 -crf 20 -f matroska - | mpv


#PulseAudio over network

#One of PulseAudio's unique features is its ability to stream audio from clients over TCP to a server running the PulseAudio daemon reliably within a LAN. Ensure that client and server systems agree on the time (i.e., use NTP), or audio streams may be choppy or may not work at all. For a more detailed guide visit the Official PulseAudio Documentation

#Enable the TCP module on the server(the computer that actually outputs sound), edit /etc/pulse/default.pa to add or uncomment:

#load-module module-native-protocol-tcp

#Or you can use the paprefs gui application (root is not required):

 #Go to Network Access -> Enable network access to local sound devices

#To make sure module-native-protocol-tcp is loaded on the server, you can use:

#pacmd list-modules | grep module-native-protocol-tcp

#It is a requirement that both the client and server share the same cookie. Ensure that the clients and server share the same cookie file found under ~/.config/pulse/cookie. It does not matter whose cookie file you use (the server or a client's), just that the server and client(s) share the same one.

#If it is undesirable to copy the cookie file from clients, anonymous clients can access the server by passing auth-anonymous to module-native-protocol-tcp on the server (again in /etc/pulse/default.pa):

#load-module module-native-protocol-tcp auth-anonymous=1

#It is also possible to authenticate based on client IP address:

#load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1;192.168.0.0/24

#Change the LAN IP subnet to match that of those clients you wish to have access to the server.
#Selecting the Server

#For a single shell or command you can set the environment variable $PULSE_SERVER to the host name or IP address of the desired PulseAudio server.

 #$ export PULSE_SERVER=server-hostname-or-ip && mplayer test.mp3

#Alternatively you can create or modify ~/.pulse/client.conf or /etc/pulse/client.conf to set a default-server persistently.

#default-server = server-hostname-or-ip

