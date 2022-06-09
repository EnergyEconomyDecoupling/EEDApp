# Base image
FROM zekemarshall/eed-app-base-image:latest

# Test Section - Add sshd_config
## Install OpenSSH and set the password for root to "Docker!".

RUN apt-get update \
    && apt-get upgrade -y \
    #&& apk add openssh \
    #&& apt-get install -y openssh \
    && apt-get install -y openssh-server \
    && echo "root:Docker!" | chpasswd
    # && mkdir /run/sshd

# Copy necessary files
## app file
COPY /app.R /app.R
## App modules folder
COPY /App-Modules /App-Modules
## www folder
COPY /www /www
## ReboundTools documenetation .Rmd file
COPY /reboundtools_doc.Rmd /reboundtools_doc.Rmd

# Expose port, 2222 port is used for SSH access
EXPOSE 3838 2222

# Copy sshd_config file
COPY sshd_config /etc/ssh/

# Start SSH
# CMD startup.sh

# Copy and configure the ssh_setup file
RUN mkdir -p /tmp
COPY ssh_setup.sh /tmp
RUN chmod +x /tmp/ssh_setup.sh \
    && (sleep 1;/tmp/ssh_setup.sh 2>&1 > /dev/null)


# Run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
