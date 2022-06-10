#!/bin/sh
set -e

# Get env vars in the Dockerfile to show up in the SSH session
eval $(printenv | sed -n "s/^\([^=]\+\)=\(.*\)$/export \1=\2/p" | sed 's/"/\\\"/g' | sed '/=/s//="/' | sed 's/$/"/' >> /etc/profile)

echo "Starting SSH ..."
service ssh start

echo "Running start-up command for the R Shiny App 'shiny::runApp('app.R', host = '0.0.0.0', port = 3838)'..."
R -e "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"
