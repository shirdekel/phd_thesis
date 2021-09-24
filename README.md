# Shir Dekel's PhD Thesis

This repository contains the code that was used to compile the thesis document.
The thesis itself can be found at https://thesis.shirdekel.com/. The `{targets}`
package was used to run everything as a reproducible pipeline. `_targets.R`
contains each step of the pipeline.

The thesis is made up of fourteen experiments. Seven of these are reported in
the main text, and the other seven are reported in the appendices. Each
experiment's data is stored as a separate R package. The table below shows
the name of the relevant data package, where it is primarily reported, and
the relevant experiment number that it corresponds to in the thesis text.

<!-- ```r -->
<!-- tibble::tribble( -->
<!--   ~`Data R package`, ~`Location in thesis text`, ~`Thesis experiment number`, -->
<!--   "aggregation1", "Chapter 2", "Experiment 1", -->
<!--   "aggregation2", "Chapter 2", "Experiment 2", -->
<!--   "aggregation3", "Appendix A", "Experiment 3", -->
<!--   "aggregation4", "Appendix A", "Experiment 4", -->
<!--   "alignment2", "Chapter 4", "Experiment 1", -->
<!--   "alignment3", "Chapter 4", "Experiment 2", -->
<!--   "alignment8", "Chapter 4", "Experiment 3", -->
<!--   "alignment1", "Appendix B", "Experiment 4", -->
<!--   "alignment4", "Appendix B", "Experiment 5", -->
<!--   "alignment5", "Appendix B", "Experiment 6", -->
<!--   "alignment6", "Appendix B", "Experiment 7", -->
<!--   "alignment7", "Appendix B", "Experiment 8", -->
<!--   "anecdotes1", "Chapter 6", "Experiment 1", -->
<!--   "anecdotes2", "Chapter 6", "Experiment 2" -->
<!--   ) %>% -->
<!--   kableExtra::kbl(format = "markdown") -->
<!-- ``` -->

|Data R package |Location in thesis text |Thesis experiment number |
|:--------------|:-----------------------|:------------------------|
|aggregation1   |Chapter 2               |Experiment 1             |
|aggregation2   |Chapter 2               |Experiment 2             |
|aggregation3   |Appendix A              |Experiment 3             |
|aggregation4   |Appendix A              |Experiment 4             |
|alignment2     |Chapter 4               |Experiment 1             |
|alignment3     |Chapter 4               |Experiment 2             |
|alignment8     |Chapter 4               |Experiment 3             |
|alignment1     |Appendix B              |Experiment 4             |
|alignment4     |Appendix B              |Experiment 5             |
|alignment5     |Appendix B              |Experiment 6             |
|alignment6     |Appendix B              |Experiment 7             |
|alignment7     |Appendix B              |Experiment 8             |
|anecdotes1     |Chapter 6               |Experiment 1             |
|anecdotes2     |Chapter 6               |Experiment 2             |

Data for any given experiment can be accessed by installing the relevant package
from GitHub. Simply use the Data R package name from the table above. For
instance, the data from Chapter 2 Experiment 1 can be installed by running:

```r
## First install the `{remotes}` pacakge if it is not already installed by using:
## install.packages("remotes")
remotes::install_github("shirdekel/aggregation1")
```

Once installed, you can simply call the data using:

```r
aggregation1::data
```

To reproduce any of the analyses or plots in the thesis, use
`targets::tar_read()`. For instance, if you want to see the R object of all the
results used in Chapter 2 Experiment 1, you can run:

```r
targets::tar_read(results_aggregation_1)
```

The plots are accessed through:

```r
targets::tar_read(plot_aggregation_1)
```

Similarly, the directory that contains the materials is accessed through:

```r
targets::tar_read(materials_aggregation_1)
```

Alternatively, the code can be accessed manually by visiting the `R/` directory.
For instance, you can produce the results reported in Chapter 2 Experiment 1 by
loading and then calling the relevant function and data:

```r
source("get_results_aggregation_1.R")
get_results_aggregation_1(aggregation1::data)
```
