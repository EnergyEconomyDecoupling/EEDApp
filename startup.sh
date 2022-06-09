#!/bin/sh

echo "Start sshd"
/usr/sbin/sshd

echo "Start app"
zekemarshall/eed-app-base-image -g "daemon off;"
