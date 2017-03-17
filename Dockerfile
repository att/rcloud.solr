FROM rocker/tidyverse:3.3.2

COPY . /rcsolr/

RUN . /etc/environment \
  && R -e "devtools::install_dev_deps(\"/rcsolr\", repos = \"${MRAN}\")" \
  && rm -rf /rcsolr

EXPOSE 8787

CMD ["/init"]
