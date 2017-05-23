# RStudio Solution to the following error message:
>"Error: GeomRasterAnn was built with an incompatible version of ggproto." in RStudio

If you recently downloaded a new version of RStudio, chances are high that you may see the error message up above when using any of the "gg..." packages.

Here are lines of code to be run in RStudio to help you solve this error.

```{r}
devtools::install_github('thomasp85/ggforce')
devtools::install_github('thomasp85/ggraph')
devtools::install_github("slowkow/ggrepel")
devtools::install_github("hadley/ggplot2@v2.2.0") 
devtools::install_github("dkahle/ggmap")
````

*Last updated on May 23, 2017*
