# Overview

This repo allows users to construct a "standard" set of point verification results using [harp](https://harphub.github.io/harp/), a set of R packages developed within the ACCORD consortium for analysis, verification, and visualisation of NWP data. The typical RRJVSP workflow for deterministic or ensemble experiments is the following:
- Convert HARMONIE-AROME vfld and vobs files to sqlite tables.  
- Use the sqlite data to perform point verification using harp over a standard set of groupings (e.g. ``lead_time``, ``SID``) for surface and upper-air parameters.
- The point verification produces harp ``.rds`` files which can be visualised using ``harpVis::shiny_plot_point_verif``.
- If desired, a set of local png files are also produced for a standard set of verification scores. These pngs can be visualised in the simple shiny app provided in the repo.
- If desired. produce a scorecard comparing two models. While this functionality is available in the scripts, some care must be taken in the construction and interpretation of scorecards. [See here for more information](https://harphub.github.io/harp-training-2022/scrcard.html). The default configuration settings outlined in these scripts are only indicative.

Little to no prior knowledge of harp is assumed, although users are encouraged to familiarise themselves with harp using the [2022](https://harphub.github.io/harp-training-2022/index.html) and [2024](https://harphub.github.io/harp_training_2024/) training courses. Note that harp provides many more utilites than just NWP verification.

