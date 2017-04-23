# hetviz
### Treatment Effect Heterogeneity visualization using R

[![Build Status](https://travis-ci.org/sanoke/hetviz.svg?branch=master)](https://travis-ci.org/sanoke/hetviz)
[![codecov](https://codecov.io/gh/sanoke/hetviz/branch/master/graph/badge.svg)](https://codecov.io/gh/sanoke/hetviz)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/sanoke/hetviz?branch=master&svg=true)](https://ci.appveyor.com/project/sanoke/hetviz)

See the [wiki](https://github.com/sanoke/hetviz/wiki) for detailed instructions and demonstrations.

#### Installation

1. Download **hetviz** by clicking [here](https://github.com/sanoke/hetviz/archive/master.zip) or cloning the repository.
2. Open `R`.
2. Install and load the `devtools` package in `R`, which will facilitate the installation of this package.
```
install.packages("devtools")
```
3. Within `R`, use `setwd()` to set your working directory to the top level of the **hetviz** clone you just downloaded.
4. Load `hetviz` using `devtools::load_all()`.
3. Run `hetviz` using `hetviz()` (no arguments). The application will launch in a new window.

~~Install `hetviz` using `install_github("sanoke/hetviz")`.~~ Not working (not sourcing scripts correctly); will look into this.

##### Bug Reporting

Report any bugs or suggestions as an [issue](https://github.com/sanoke/hetviz/issues).

##### Licensing

The hetviz R package and `shiny` interface are open source licensed under the GNU Public License, version 3 (GPLv3).
