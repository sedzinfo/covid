# Time Series covid-19

**[Open the live app](https://sedzinfo.shinyapps.io/covid_timeseries/)** · [Website](https://sedzinfo.github.io/covid/)

plot covid-19 time series

## Installation

### Prerequisites

1. **Install R**: https://cran.r-project.org/
2. **Install RStudio** (optional but recommended): https://posit.co/downloads/

### Package Installation

Open RStudio (or an R console) and run:

```r
install.packages(
  "covid",
  repos = c("https://sedzinfo.r-universe.dev", "https://cloud.r-project.org")
)
```

[R-universe](https://sedzinfo.r-universe.dev/covid) builds covid from this repository, and CRAN provides the packages it needs.

To install from GitHub instead:

```r
# Install devtools if you haven't already
install.packages("devtools")
# Install covid from GitHub
devtools::install_github("sedzinfo/covid")
```

Or install the source tarball of the [latest release](https://github.com/sedzinfo/covid/releases/tag/latest), which is rebuilt from `main` on every change, together with the PDF manual:

```r
install.packages(
  "https://github.com/sedzinfo/covid/releases/download/latest/covid_0.0.0.9000.tar.gz",
  repos = NULL, type = "source"
)
```

## Usage

```r
library(covid)
covid()
```

---

[![R-universe](https://sedzinfo.r-universe.dev/covid/badges/version)](https://sedzinfo.r-universe.dev/covid)
![Stars](https://img.shields.io/github/stars/sedzinfo/covid)
![Watchers](https://img.shields.io/github/watchers/sedzinfo/covid)
![Repo Size](https://img.shields.io/github/repo-size/sedzinfo/covid)
![Open Issues](https://img.shields.io/github/issues/sedzinfo/covid)
![Forks](https://img.shields.io/github/forks/sedzinfo/covid)
![Last Commit](https://img.shields.io/github/last-commit/sedzinfo/covid)
![Contributors](https://img.shields.io/github/contributors/sedzinfo/covid)
![License](https://img.shields.io/github/license/sedzinfo/covid)
![Workflow Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/covid/build-tarball.yml)
