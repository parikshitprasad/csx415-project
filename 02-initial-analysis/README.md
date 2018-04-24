Assignment: Initial Model or Analysis
================

Now that you have completed a *Formal Problem Statement (FPS)*, it is time to begin your analysis.

Assignment
----------

-   Familiarize yourself with the Reproducible Research, and
-   [ProjectTemplate](http://projecttemplate.net/)
-   [Drake](https://github.com/ropensci/drake)
    -   [blog post](https://ropensci.org/blog/2018/02/06/drake/) many links
-   Alternatively, you can work in rstudio notebooks, but I advise against this as most real projects outgrow notebooks pretty quickly and the notebook becomes only the output of the project.

-   Layout your project using **one** of these Framework
-   Put your data in the project (unless it is sensitive -- then leave it out.)

-   Perform some very basic ("naive") analysis or train a model

-   Create a RMarkdown document for evaluating analysis/model performance
-   Evaluate your model/analysis by standard metrics
-   Evaluate your project according to your *FPS*
-   Call it project-performance.Rmd and place it in the root of your `project` fork.

-   Commit and push it to your project into the csx415/projects folder

-   Move your FPS into this folder; Update your FPS;

### Notes:

-   Work in your teams but make sure everyone submits the response.
-   This is largely about developing code; ensure that your code is follows best practices and actually runs.

Background
----------

After completing a Formal Problem Statement (FPS), you should have a pretty good idea about how to approach you problem. You next step is to do a "trial run" of your analysis. The purpose of the trial run is to:

-   Get a piece of your data inplace
-   Examine your data
-   Determine whether it is suitable for your problem
-   Quickly gauge your analysis' performance and how far you need to go to meet your project goals.

The key here is to make this part reproducible for others and future you. There are several good R packages and techniques for doing this. The R community is in the vangaurd of reproducible research and there are a lot of resources to get ideas on how to do this. Two great frameworks are ProjectTemplate and Drake.
