# Notes on Inputs and Outputs

## Exported Functions

### RCloud Search

The `rcloud.search` function is exposed as an ocap:

```r
changes$rcloud$search <- make.oc(rcloud.search)
```

### Updating solr

The main function to update a notebook in the solr index is `update_solr`. This is called in a number of places but most commonly from `rcloud.update.notebook` which itself is mapped to an OCAP in `update_notebook`.

Only notebooks with `NULL` group are updated. Private or encrypted notebooks have none-null groups.

## What is updated?

### Cells

When a cell is updated `update_solr` is called near the end of the `rcloud.update.notebook` function.

### Adding and Removing

The function `solr.delete.doc` is called when a document is requested to be invisible by `rcloud.set.notebook.visibility` function or when it is encrypted by `rcloud.set.notebook.cryptgroup`.

New docs seem to be added ith the `update_solr` function.


### Comments

When comments are updated solr is called: See `rcloud.support/R/notebook.comments.R` and this calls some of our functions. `.solr.post.comment`, `.solr.modify.comment` and `.solr.delete.comment`. Even though they're not exported they are called a lot internally.

