Scripts to tidy trait and allometry data bases. These scripts are intended to work particularly the [TRY data base](https://www.try-db.org/TryWeb/Home.php), but it provides a framework to incorporate data from additional sources.

For additional details on these scripts (the "knitted" R Markdown), check the [documentation here](https://mpaiao.github.io/TraitAllom_Workflow/index.html).

1. [**TidyTraitAllomDB.Rmd**](TidyTraitAllomDB.Rmd) – This This (mostly) R Markdown script takes data downloaded from the TRY data base (plus additional sources) and apply [tidy data](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) standards. In a tidy data set, each row represents one observation, each column represents a variable,  and each cell represents a value. In addition, categorical traits are also harmonised into consistent categories and species taxonomy is standardised to the best extent possible (likely with some user input).
  * Make sure to download the Fortran code [**SplitAuthor.f90**](SplitAuthor.f90) to the main working directory. Also, make sure to download the directory [RUtils](RUtils) containing multiple ancillary functions.
  * Data downloaded directly from the TRY data base should go to the directory defined in variable `orig_path`.
  * The csv files provided in [InputLists](InputLists) are examples. One should be able to add/remove traits according to their needs, however, new traits without standardised values and units will require code development, specifically in files [RUtils/TRY_Harmonise_Utils.r](RUtils/TRY_Harmonise_Utils.r) and [RUtils/TRY_Fix_OrigValue_Str.r](RUtils/TRY_Fix_OrigValue_Str.r).
  * Data sets other than TRY can be provided, but users ought to format the data in tidy format, using variable names consistent to those defined in [InputLists](InputLists).  Brief examples of additional data sets from different data bases are provided in [AdditionalData](AdditionalData).


**Important notes**.

* These scripts are (permanently?) under development.  Contributions, suggestions, and pull requests with bug fixes are always welcome!

