# install.packages('dataverse')

library(tidyverse)
library(haven)
library(dataverse)

x <- rnorm(2500, mean = 50, sd = 10)
mean(x)
sd(x)
hist(x)
hist(x, breaks = 30, probability = TRUE)

ABH.data <- get_dataframe_by_doi(
                filedoi = "doi:10.7910/DVN/VR12G4/WGIHIT", 
                original = TRUE,
                .f = haven::read_dta,
                server = "dataverse.harvard.edu"
)

ABH.data |> summarize(mean_nom = mean(abs_dist_nom, na.rm = TRUE),
                      mean_cvp = mean(abs_dist_cvp, na.rm = TRUE))

newdata <- ABH.data |> filter(congress > 105) |> 
            group_by(congress, first_term) |>
            summarize(mean_nom = mean(abs_dist_nom, na.rm = TRUE),
                      mean_cvp = mean(abs_dist_cvp, na.rm = TRUE))