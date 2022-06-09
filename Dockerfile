# Base image
FROM zekemarshall/eed-app-base-image:latest

# Test Section - Add sshd_config
## Install OpenSSH and set the password for root to "Docker!".

RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y openssh-server \
    && echo "root:Docker!" | chpasswd

# Copy necessary files
## app file
COPY /app.R /app.R
## App modules folder
COPY /App-Modules /App-Modules
## www folder
COPY /www /www
## ReboundTools documenetation .Rmd file
COPY /reboundtools_doc.Rmd /reboundtools_doc.Rmd

# Expose port
EXPOSE 3838

# Run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
