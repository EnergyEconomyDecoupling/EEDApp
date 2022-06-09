# Base image
FROM zekemarshall/eed-app-base-image:latest

ENV SSH_PASSWD "root:Docker!"
RUN apt-get update \
    && apt-get install -y --no-install-recommends dialog \
    && apt-get install -y --no-install-recommends openssh-server \
    && echo "$SSH_PASSWD" | chpasswd

COPY /sshd_config /etc/ssh/

# Copy necessary files
## app file
COPY /app.R /app.R
## App modules folder
COPY /App-Modules /App-Modules
## www folder
COPY /www /www
## ReboundTools documentation .Rmd file
COPY /reboundtools_doc.Rmd /reboundtools_doc.Rmd

# Expose port, 2222 port is used for SSH access
EXPOSE 3838 2222

COPY /init_container.sh
RUN chmod 755 init_container.sh
ENTRYPOINT ["init_container.sh"]

# Run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
