# hetviz
### Treatment Effect Heterogeneity visualization

[![Build Status](https://travis-ci.com/sanoke/hetviz-dev.svg?token=yyui4yJMpRQeueaqxnpZ&branch=master)](https://travis-ci.com/sanoke/hetviz-dev) 
[![codecov](https://codecov.io/gh/sanoke/hetviz-dev/branch/master/graph/badge.svg?token=kOXo076z4v)](https://codecov.io/gh/sanoke/hetviz-dev)

#### Installation

First install and load the `devtools` package in `R`, which will facilitate the installation of this package.
```
install.packages("devtools")
library(devtools)
```

If the repository is public...

1. Install `hetviz` using `install_github("sanoke/hetviz-dev")`.
2. Load `hetviz` using `library(hetviz)`.
3. Run `hetviz` using `hetviz()` (no arguments).

If the repository is private...

1. Clone the repository.
2. Within `R`, set the working directory as a directory that *contains* the cloned repository.
3. Load `hetviz` using `devtools::load_all()`.
4. Run `hetviz` using `hetviz()` (no arguments).


#### About hetviz

Paragraph describing the platform (can copy and paste from manuscript).

##### Deployment

Stuff to come.

##### Demo

Screenshots.

##### Bug Reporting

Link to issue tracker.

##### Licensing

The hetviz R package and `shiny` interface are open source licensed under the GNU Public License, version XX (GPLvXX).


----

### plotly bugs

`plotly` has trouble playing nice with shiny. Below is some documentation on the bugs I found and how I corrected them.

#### Bug 1: 

**[SOLUTION]**


----

##### Notes to myself for eventual README...
- [ ] R package intended to run locally, with GUI through a web browser.
- [ ] Application requires JavaScript to be enabled and running properly within the browser.
- [ ] Intended audience is researchers interested in visualizing treatment effect heterogeneity within their data, as estimated using methods within my application or their own procedure.
- [ ] Explain: Vertical instead of horizontal "settings" panel because I thought it was a better use of space, but open to comments.
- [x] Add information on how to run application from GitHub files.
