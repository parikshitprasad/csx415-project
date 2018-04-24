Assignment: Initial Model or Analysis
================

Now that have:

-   given structure to your project using *drake* or *ProjectTemplate*
-   built a "naive" model and,
-   a Rmd document for measuring your model

you should have a pretty good idea if there are any problems with data or your problem set-up. FOr example, you might have learned that the *naive* model already satisfied the *model success criteria*. Generally, it won't but it may cause you to recalibrate. Now, you can begin your project in earnest. This starts by understanding what you need to produce and begin to produce it.

Assignment
----------

-   Create a asset list (`ASSETS.md`)

-   Perform some explortory data analysis.

-   Definitely plot (histogram) any response variables.

-   Plot at your predictors.

-   Begin the analysis:

-   For analysis/explanatory/exploratory projects start by

-   For ML projects

    -   Build a linear model use `lm` for regression and/or `glm( family=binomial. ... )` for classification

    -   Evaluate your linear model using your model evaluation RMarkdown document, if you are clever you can reuse the template and version it very quickly.

    -   Knit this to `model-performance-linear.[html|pdf|md]` where you are keeping your assets

    -   Make sure you know who to interpret a linear model! Let me know if we have to cover this.

    -   Build a tree model (`rpart`)

    -   plot the tree paying particular attention to the first few nodes, do they make sense?

    -   Evaluate your tree with you model evaluation RMarkdown document.
        Knit this to `model-performance-rpart.[html|pdf|md]`

-   Has your model(s) met the *model success criteria*?

    -   Briefly state your assessment and what you think are the next steps in "NEXTSTEPS.md"
-   If needed update your *FPS*

### Notes:

-   Work in your teams but make sure everyone submits the response even if it is the same
-   Ensure that the files you create adhere to your framework.
-   All files go into their appropriate place within your framework.

Background
----------

### Asset List

Document for yourself a list of assets (`ASSETS.md`) that specifies all assets that are needed. This should include

-   inputs: data and code (not documented elsewhere, you do not need to document intermediate code and data that fall into the *ProjectTemplate* or *drake*
    process flow, since we know how that works!)
-   outputs: plots, files, documents, produced by the analysis

The list of assets should not be onerous. It should contain a name, path to where the assets can be found, and a short description. This purpose of the assetlist two-fold:

1.  It focuses you on what you need to produce. This is not necessarily always clear when you start and almost never clear for large projects that blend data from many different sources).
2.  It documents (for future-you) where things are comming from you are producing and what is important.

### Starting Work

#### An Analysis: exploration or explanation or answer

When starting an analysis task, it is best to start backwards from the *easiest* thing that you are trying to answer (e.g. is there a significance between several cohorts.) Understand what is being measured and what goes into that measurement. Identify any statistical test or inferrence task. For example, do you need to use the `chisq.test` or `binom.test`? Understand what populations are needed and how they are defined. Create the data munging steps to build those populations.

Do not p-hacking! (Look it up!)

#### Machine Learning

You should start a machine learning problem by building a very simple, highly interpretable model. You do not and often should not use all your features. The goal is not to produce the most accurate model, but gain some intution The goal and assess performance.

If you have a lot of data, you should down sample your data so that iterations are quick!
