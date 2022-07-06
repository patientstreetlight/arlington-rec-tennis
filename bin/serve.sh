#!/bin/sh

ip=$(ipconfig getifaddr en0)
port=8080
# Spits out a QR code of the URL to the terminal to make it easy
# to open the site from a phone :)
qrencode -t ANSI -o - "http://${ip}:${port}"
http-server site -p $port -c-1