
# atRfunctions

<!-- badges: start -->
<!-- badges: end -->

The goal of `atRfunctions` is to wrap-up the custom functions we use most frequently inside one package

## Installation

You can install the development version as below:

``` r
library(devtools)
install_github("atrcodebase/atRfunctions")
```

## Functions
This package includes the following functions.

#### compare_dt()

**usage:**

```r
library(atRfunctions)
compare_dt(df1, df2, unique_id_df1, unique_id_df2)
```

**arguments:**

  `df1` Old version of the dataset

  `df2` Latest version of the dataset

  `unique_id_df1` unique identifier in df1

  `unique_id_df2` unique identifier in df2


#### missing_translation()

**usage:**

```r
library(atRfunctions)
missing_translation(data, KEY)
```

**arguments:**

  `data` data frame
  
  `KEY` Unique identifier. The efault value is "KEY"


#### apply_log()

**usage:**

**arguments:**

  
#### analyze()

**usage:**

**arguments:**

#### labeler()

**usage:**

**arguments:**


***
##### Author: ATR - Data Management Team
