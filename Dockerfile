FROM fredhutch/r-shiny-server-base:4.2.0
RUN apt-get update -y
RUN apt-get install -y pandoc



RUN rm -rf /srv/shiny-server/
ADD ./app/*.R /srv/shiny-server/

RUN R -e "install.packages(c('pracma', 'shinycssloaders'), repos = 'http://cran.us.r-project.org')"

WORKDIR /srv/shiny-server/
CMD /usr/bin/shiny-server.sh
