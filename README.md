# rcloud.solr

[![Travis-CI Build Status](https://travis-ci.org/MangoTheCat/rcloud.solr.svg?branch=master)](https://travis-ci.org/MangoTheCat/rcloud.solr)
![Work in Progress](https://img.shields.io/badge/Status-WIP-green.svg)

This package provides search capability to [RCloud](https://github.com/att/rcloud/) by taking functionality that was previously provided by [search.R](https://github.com/att/rcloud/blob/develop/rcloud.support/R/search.R) from the rcloud.support package.

## Installation

This package must be used alongside an RCloud installation. The branch [feature/refactor-solr](https://github.com/mangothecat/rcloud/tree/feature/solr-refactor) in the mangothecat fork of RCloud works with the `rcloud.solr` package.

Install RCloud normally from the `feature/refactor-solr` branch and also install `rcloud.solr` with something like:

```
source("https://install-github.me/mangothecat/rcloud.solr")
```

You will also need to setup RCloud to work with solr the usual way.

## Testing

Some of the unit tests require a connection to a solr instance to work. The [docker-compose.yml]("docker-compose.yaml") file lays out a configuration that will allow testing. There needs to be a solr instance, setup for RCloud, running on `http://solr:8983/`. It is recommnded to use this [docker image](https://hub.docker.com/r/mangothecat/rcloud-solr/).

Also note that this package will not work in Windows due to use of parallel forks.

## Usage

Nothing just yet.
