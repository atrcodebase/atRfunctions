
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
compare_dt(df1, df2, unique_id_df1, unique_id_df2, compare_all = TRUE)
```

**arguments:**

  `df1` old version of the dataset

  `df2` latest version of the dataset

  `unique_id_df1` unique identifier in df1

  `unique_id_df2` unique identifier in df2
  
  `compare_all` logical. `TRUE`: compare all columns/variables. `FALSE`: compare only shared columns/variables.


#### missing_translation()

**usage:**

```r
library(atRfunctions)
missing_translation(data, KEY = "KEY")
```

**arguments:**

  `data` data frame
  
  `KEY` unique identifier. The default value is "KEY"


#### apply_log()

**usage:**

```r
library(atRfunctions)
apply_log(data, log,
          data_KEY = "KEY",
          log_columns = c(question = "question",
                          old_value = "old_value",
                          new_value = "new_value",
                          KEY = "KEY"
                           )
          )
```

**arguments:**

  `data` data set

  `log`	the log file

  `date_KEY` the Unique identifier in data set. Must be same as the KEY in log file

  `log_column` column names in log file
  
#### analyze()

**usage:**

**arguments:**

#### labeler()

**usage:**

```r
library(atRfunctions)
labeler(data, tool,
        survey_label = "label:English",
        choice_lable = "label:English",
        multi_response_sep = ";"
        )
```

**arguments:**

  `data` data set

  `tool` the path to the SurveyCTO data collection tool

  `survey_label` column name for the question labels in 'survey' sheet of the SurveyCTO data collection tool. The default value is 'label:English'

  `choice_lable` column name for value label in 'choices' sheet of the SurveyCTO data collection tool. The default value is 'label:English'

  `multi_response_sep` separator for the multi-select questions. The default value is ';'

***
##### Author: ATR - Data Management Team
