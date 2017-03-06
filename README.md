# exploreDataViz
Data exploration in R can be painful(ly difficult), even for those with some experience with R. This package provides some functions with which you can easily explore your data using "point and click". The plots are made through ggplot and are both interactive (using plotly) and reactive (using shiny). You will also have access to a more user-friendly table of your data (which can be easily sorted and searched through) than the one that R and RStudio typically give you. Importantly, the R-code to produce the plot will also be provided, so that users of this package might choose to refrain from it in the future and use the code directly in R(Studio). The main goal is to get more people to use R and ggplot, something that is not at all common in the social sciences. Please do note that I am quite aware that this point-click way of exploring data might clash somewhat with the philosophy of R and ggplot, but I repeat that the main goal is to get people to use R (even if that means a going through a point and click initiation rite). 

The functions:

1. The function "exploration_distributions" visualizes the distribution of a single variable through either histograms, density plots, violin plots, and boxplots. Variables can be split by factors.

2. The function "exploration_means" allows you to get and display means and standard errors. Output is both a plot and a summary table. Splitting by multiple groups is possible. There is also a possibility of downloading a summary table into excel and Word. If you are using a Mac, you might require Java and RQuartz to be able to download the Word-file.

3. The function “exploration_scatter” creates scatter plots between two variables. Splitting by multiple groups possible. Possibility to show fitted lines (also split per group) and confidence intervals around these lines. 

4. The function “exploration_networks” allows you to examine networks and several covariates. Several estimation techniques are possible. 





