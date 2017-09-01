FROM rocker/tidyverse:3.3.2

COPY . /rcsolr/

RUN apt-get update \
  && apt-get install -y curl

# Install some useful dev dependencies
RUN . /etc/environment \
  && install2.r --error \
     --repos $MRAN \
     listviewer \
# Install direct package dependencies
  && R -e "devtools::install_dev_deps(\"/rcsolr\", repos = \"${MRAN}\")" \
  && rm -rf /rcsolr

EXPOSE 8787

CMD ["/init"]
