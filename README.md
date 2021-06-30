# Shir Dekel's PhD Thesis

This repository contains the code that was used to compile the thesis document.
The `{targets}` package was used to run everything as a reproducible pipeline.
`_targets.R` contains each step of the pipeline.

The thesis is made up of fourteen experiments. Seven of these are reported in
the main text, and the other seven are reported in the appendices. Each
experiment's data is stored as a separate R package. The table below shows a
list of the experiments that identifies the experiment ID (used in the code),
the name of the relevant data package, where it is primarily reported, and the
relevant experiment number that it corresponds to in the thesis text.

<!-- ```r -->
<!-- tibble::tribble( -->
<!--   ~`Experiment ID`, ~`Data R package`, ~`Location in thesis text`, ~`Thesis experiment number`, -->
<!--   "aggregation_1",   "aggregation1", "Chapter 2", "Experiment 1", -->
<!--   "aggregation_2",   "aggregation2", "Chapter 2", "Experiment 2", -->
<!--   "aggregation_3",   "aggregation3", "Appendix A", "Experiment 3", -->
<!--   "aggregation_4",   "aggregation4", "Appendix A", "Experiment 4", -->
<!--   "alignment_2",   "alignment2", "Chapter 4", "Experiment 1", -->
<!--   "alignment_3",   "alignment3", "Chapter 4", "Experiment 2", -->
<!--   "alignment_8",   "alignment8", "Chapter 4", "Experiment 3", -->
<!--   "alignment_1",   "alignment1", "Appendix B", "Experiment 4", -->
<!--   "alignment_4",   "alignment4", "Appendix B", "Experiment 5", -->
<!--   "alignment_5",   "alignment5", "Appendix B", "Experiment 6", -->
<!--   "alignment_6",   "alignment6", "Appendix B", "Experiment 7", -->
<!--   "alignment_7",   "alignment7", "Appendix B", "Experiment 8", -->
<!--   "anecdotes_1",   "anecdotes1", "Chapter 6", "Experiment 1", -->
<!--   "anecdotes_2",   "anecdotes2", "Chapter 6", "Experiment 2", -->
<!--   ) %>% -->
<!--   kableExtra::kbl(format = "markdown") -->
<!-- ``` -->

|Experiment ID |Data R package |Location in thesis text |Thesis experiment number |
|:-------------|:--------------|:-----------------------|:------------------------|
|aggregation_1 |aggregation1   |Chapter 2               |Experiment 1             |
|aggregation_2 |aggregation2   |Chapter 2               |Experiment 2             |
|aggregation_3 |aggregation3   |Appendix A              |Experiment 3             |
|aggregation_4 |aggregation4   |Appendix A              |Experiment 4             |
|alignment_2   |alignment2     |Chapter 4               |Experiment 1             |
|alignment_3   |alignment3     |Chapter 4               |Experiment 2             |
|alignment_8   |alignment8     |Chapter 4               |Experiment 3             |
|alignment_1   |alignment1     |Appendix B              |Experiment 4             |
|alignment_4   |alignment4     |Appendix B              |Experiment 5             |
|alignment_5   |alignment5     |Appendix B              |Experiment 6             |
|alignment_6   |alignment6     |Appendix B              |Experiment 7             |
|alignment_7   |alignment7     |Appendix B              |Experiment 8             |
|anecdotes_1   |anecdotes1     |Chapter 6               |Experiment 1             |
|anecdotes_2   |anecdotes2     |Chapter 6               |Experiment 2             |

Data for any given experiment can be accessed by installing the relevant package
from GitHub. Simply use the Data R package name from the table above. For
instance, the `aggregation_1` data from Chapter 2 Experiment 1 can be installed
by running:

```r
## First install the `{remotes}` pacakge if it is not already installed by using:
## install.packages("remotes")
remotes::install_github("shirdekel/aggregation1")
```

Once installed, you can simply call the data using:

```r
aggregation1::data
```
