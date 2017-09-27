# rcloud.solr

[![Travis-CI Build Status](https://travis-ci.org/MangoTheCat/rcloud.solr.svg?branch=master)](https://travis-ci.org/MangoTheCat/rcloud.solr)
![Work in Progress](https://img.shields.io/badge/Status-WIP-green.svg)

This package provides search capability to [RCloud](https://github.com/att/rcloud/) by taking functionality that was previously provided by [search.R](https://github.com/att/rcloud/blob/develop/rcloud.support/R/search.R) from the rcloud.support package.

## Installation

### RCloud

This package must be used alongside an RCloud installation. The branch [feature/refactor-solr](https://github.com/mangothecat/rcloud/tree/feature/solr-refactor) in the mangothecat fork of RCloud works with the `rcloud.solr` package.

Install RCloud normally from the `feature/refactor-solr` branch.

### rcloud.solr

install `rcloud.solr` with something like:

```
source("https://install-github.me/mangothecat/rcloud.solr")
```

or:
```
install.packages("remotes")
remotes::install_github("mangothecat/rcloud.solr")
```

### Solr

Clearly we need solr running to test this out. The solr configuration hasn't been updated in the `feature/refactor-solr` branch yet. Instead the configuration has been added to a Docker image at [mangothecat/docker-rcloud-solr](https://github.com/mangothecat/docker-rcloud-solr) This is also on [Docker hub](https://hub.docker.com/r/mangothecat/rcloud-solr/), so to install it run:

```sh
docker pull mangothecat/rcloud-solr
```

to start the service run something like:

```sh
sudo docker run --rm -d -p 8983:8983 mangothecat/rcloud-solr
```

Your RCloud config will then need the line:

```yaml
solr.url: http://127.0.0.1:8983/solr/rcloudnotebooks
```

When you bring up RCloud it should pick up this service.

## Development Environment

I recommend using [Docker](https://www.docker.com/) to setup the development environment. The repository contains a Docker file to build an RStudio image (mostly based on [rocker/tidyverse](https://hub.docker.com/r/rocker/tidyverse/)). This also installs any dev dependencies that `rcloud.solr` needs so rebuild it if these ever change. To build the RStudio image run (from project root):

```sh
sudo docker build . -t rcsolr-rstudio
```

Or you can pull the version that matches the development head from docker hub: `docker pull mangothecat/rcsolr-rstudio`.

The repo also contains a Docker Compose file to bring up the whole environment. This includes the RStudio container and an instance of Solr. To run all this in the background run:

```sh
sudo docker-compose up -d
```

and then you can use `stop` or `down` depending how you want to tear down.

## Testing

Some of the unit tests require a connection to a solr instance to work. The [docker-compose.yml]("docker-compose.yaml") file lays out a configuration that will allow testing. There needs to be a solr instance, setup for RCloud, running on `http://solr:8983/`. It is recommnded to use this [docker image](https://hub.docker.com/r/mangothecat/rcloud-solr/).

Also note that this package will not work in Windows due to use of parallel forks.

