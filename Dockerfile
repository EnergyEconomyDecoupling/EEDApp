# Base image
FROM zekemarshall/eed-app-base-image:latest
WORKDIR /app/

COPY /sshd_config /etc/ssh/

RUN apt-get update \
    && apt-get install -y --no-install-recommends dialog \
    && apt-get install -y --no-install-recommends openssh-server \
    && echo "root:Docker!" | chpasswd \
    && chmod u+x /app/init_container.sh

# Copy necessary files
## app file
COPY /app.R /app/app.R
## App modules folder
COPY /App-Modules /app/App-Modules
## www folder
COPY /www /app/www
## ReboundTools documentation .Rmd file
COPY /reboundtools_doc.Rmd /app/reboundtools_doc.Rmd
## /init_container.sh file
#COPY /init_container.sh /init_container.sh

# Expose port, 2222 port is used for SSH access
EXPOSE 3838 2222

ENTRYPOINT ["/app/init_container.sh"]

# Run app on container start
# CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
