# Contributing to bid-framework

This outlines how to propose a change to our work. We follow much of the same
guidelines as tidyverse packages, so please see the
[**development contributing guide**](https://rstd.io/tidy-contrib) for more
detailed information about contributing to this project.

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure at least one of the primary authors has agreed that it is a problem.
If you've found a bug or logical inconsistency, please create an associated
issue and illustrate with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) and/or other supporting
documentation (e.g., peer-reviewed article, data set, etc.).

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).  
*  Look at the build status before and after making changes. The `README` should
contain badges for any continuous integration services used by the package.  
*  New code should follow the tidyverse [style guide](http://style.tidyverse.org).
Please don't restyle code that has nothing to do with your PR.  
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), 
for documentation.  
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept. 

### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its terms.

### See tidyverse [development contributing guide](https://rstd.io/tidy-contrib) for further details.
