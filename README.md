
# Source Code for the PhD Thesis “Integration of Feature Selection Stability in Model Fitting” by Andrea Bommert

## Comparison of Stability Measures

  - Folder “Comparison Small” contains the source code for the analyses
    in Section 6.2.
      - cs\_experiments.R: Source this file on a high performance
        compute cluster. Then submit the jobs manually with
        specifications suitable for your compute cluster.
      - cs\_results.R: When all jobs have finished, source this file in
        the R session on your high performance compute cluster. It
        creates the file “cs\_results.RData”.
      - cs\_evaluation.R: Creates the plots and tables for the analyses
        in Section 6.2. The file “cs\_results.RData” must be located in
        your working directory.
  - Folder “Properties” contains the source code for some analyses in
    Appendix B.

## Datasets

Contains the source code for creating the data sets, resampling
instances and similarity matrices.

  - datasets\_choice.R: Illustrates the choice of data sets. Creates the
    file “data\_ids.RData”.
  - datasets.R: Source this file in order to create the file
    “datasets.RData”. The file “data\_ids.RData” must be located in
    your working directory.
  - task\_generation.R: Source this file in order to create the
    following files:
      - rins.RData
      - simmats.RData The file “datasets.RData” must be located in your
        working directory.
  - task\_generation\_halves.R: Source this file in order to create the
    following files:
      - rins\_halves.RData
      - simmats\_halves.RData The file “datasets.RData” must be located
        in your working directory.
  - pca\_plots.R: Creates the PCA plots of the data sets. The file
    “datasets.RData” must be located in your working directory.

## Feature Section Similar Features

Contains the source code for the analyses in Section 8.1.

  - fssf\_experiments.R: Source this file on a high performance compute
    cluster. Then submit the jobs manually with specifications suitable
    for your compute cluster. The following files must be located in
    your working directory:
      - extractSelectedFeatures.R
      - extractSelectedFeaturesInternal.R
      - helper.R
      - RLearner\_classif\_L0Learn.R
  - fssf\_results.R: When all jobs have finished, source this file in
    the R session on your high performance compute cluster. It creates
    the file “fssf\_results.RData”.
  - fssf\_evaluation.R: Creates the plots for the analyses in Section
    8.1. The file “fssf\_results.RData” must be located in your working
    directory.

## Filter Benchmark

Contains the source code for the analyses in Chapter 5.

  - Folder “Benchmark” contains the source code for the analyses in
    Sections 5.2 and 5.3.
      - pars\_values.R: Source this file first in order to create the
        file “pars\_values.RData”. The file “paramset.R” must be located
        in your working directory.
      - fb\_experiments.R: Source this file on a high performance
        compute cluster. Then submit the jobs manually with
        specifications suitable for your compute cluster. The following
        files must be located in your working directory:
          - make\_lrn.R
          - measure\_time\_filter.R
          - microarray\_filters.R
          - resampling.R
          - datasets.RData (see Datasets)
          - pars\_values.RData
          - rins.RData (see Datasets)
      - fb\_results.R: When all jobs have finished, source this file in
        the R session on your high performance compute cluster. It
        creates the files “results\_filter.RData” and
        “results\_ranking.RData”.
      - fb\_tuning.R: Source this file in order to create the file
        “results\_tuning.RData”. The file “results\_filter.RData” must
        be located in your working directory.
      - fb\_rank\_cors.R: Source this file in order to create the file
        “mean\_cor.RData”. The file “results\_ranking.RData” must be
        located in your working directory. Also, the plots for the
        analyses in Section 5.2 are generated.
      - fb\_evaluation.R: Creates the plots for the analyses in Section
        5.3. The files “results\_tuning.RData” and “mean\_cor.RData”
        must be located in your working directory.
  - Folder “Stochasticity” contains the source code for the analyses in
    Section 5.4.
      - fs\_experiments.R: Source this file on a high performance
        compute cluster. Then submit the jobs manually with
        specifications suitable for your compute cluster. The file
        “datasets.RData” (see Datasets) must be located in your
        working directory.
      - fs\_results.R: When all jobs have finished, source this file in
        the R session on your high performance compute cluster. It
        creates the file “fs\_results.RData”.
      - fs\_evaluation.R: Creates the plots for the analyses in Section
        5.4. The file “fs\_results.RData” must be located in your
        working directory.
  - Folder “Time” contains the source code for the analyses in Section
    5.1.
      - ft\_dataset.R: Source this file first in order to create the
        file “ft\_dataset.RData”. The file “datasets.RData” (see
        Datasets) must be located in your working directory.
      - ft\_experiments.R: Source this file on a high performance
        compute cluster. Then submit the jobs manually with
        specifications suitable for your compute cluster. The following
        files must be located in your working directory:
          - microarray\_filters.R
          - ft\_dataset.RData
      - ft\_results.R: When all jobs have finished, source this file in
        the R session on your high performance compute cluster. It
        creates the file “results\_time.RData”.
      - ft\_evaluation.R: Creates the plots for the analyses in Section
        5.1. The file “results\_time.RData” must be located in your
        working directory.

## Model Finding Multi-Criteria

Contains the source code for the analyses in Section 6.3 and Chapter 7.

  - mf\_experiments.R: Source this file on a high performance compute
    cluster. Then submit the jobs manually with specifications suitable
    for your compute cluster. The following files must be located in
    your working directory:
      - extractSelectedFeatures.R
      - extractSelectedFeaturesInternal.R
      - resampling.R
      - rins\_halves.RData (see Datasets)
      - simmats\_halves.RData (see Datasets)
  - mf\_results.R: When all jobs have finished, source this file in the
    R session on your high performance compute cluster. It creates the
    file “mf\_results.RData”.
  - mf\_tuning.R: Source this file in order to create the file
    “mf\_results\_tuned.RData”. The file “mf\_results.RData” must be
    located in your working directory.
  - mf\_sc\_evaluation.R: Creates the plots for the analyses in Section
    6.3. The files “datasets.RData” (see Datasets) and
    “mf\_results.RData” must be located in your working directory.
  - mf\_evaluation.R: Creates the plots and tables for the analyses in
    Chapter 7. The file “mf\_results\_tuned.RData” must be located in
    your working directory.

## Model Finding Similar Features

Contains the source code for the analyses in Chapter 8.

  - Folder “Simulated Data” contains the source code for the analyses in
    Section 8.3.
      - sd\_experiments.R: Source this file on a high performance
        compute cluster. Then submit the jobs manually with
        specifications suitable for your compute cluster. The following
        files must be located in your working directory:
          - data\_generation.R
          - extractSelectedFeatures.R
          - extractSelectedFeaturesInternal.R
          - false\_features.R
          - RLearner\_classif\_L0Learn.R
      - sd\_results.R: When all jobs have finished, source this file in
        the R session on your high performance compute cluster. It
        creates the files “sd\_results.RData” and “sd\_runtimes.RData”.
      - sd\_tuning.R: Source this file in order to create the file
        “sd\_results\_tuned.RData”. The files “tuning\_funs.R” and
        “sd\_results.RData” must be located in your working directory.
      - sd\_evaluation.R: Creates the plots for the analyses in Section
        8.3. The files “sd\_results\_tuned.RData” and
        “sd\_runtimes.RData” must be located in your working
        directory.
  - Folder “Real Data” contains the source code for the analyses in
    Section 8.4.
      - rd\_experiments.R: Source this file on a high performance
        compute cluster. Then submit the jobs manually with
        specifications suitable for your compute cluster. The following
        files must be located in your working directory:
          - extractSelectedFeatures.R
          - extractSelectedFeaturesInternal.R
          - RLearner\_classif\_L0Learn.R
          - rins.RData (see Datasets)
          - simmats.RData (see Datasets)
      - rd\_results.R: When all jobs have finished, source this file in
        the R session on your high performance compute cluster. It
        creates the file “rd\_results.RData”.
      - rd\_tuning.R: Source this file in order to create the file
        “rd\_results\_tuned.RData”. The files “tuning\_funs.R” and
        “rd\_results.RData” must be located in your working directory.
      - rd\_evaluation.R: Creates the plots for the analyses in Section
        8.4. The file “rd\_results\_tuned.RData” must be located in your
        working directory.
