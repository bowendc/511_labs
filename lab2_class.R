library(tidyverse)
library(haven)
library(dataverse)
library(gt)
library(vtable)

public.hosp <- get_dataframe_by_doi(
                      filedoi = "doi:10.7910/DVN/NRQPZX/KOEWTQ",
                      original = TRUE,
                      .f = haven::read_dta,
                      server = "dataverse.harvard.edu"
                )

sumtable(public.hosp)

public.hosp |> select(!(c("q1", "q2", "q3", "q4"))) |>
               sumtable()

desc.tab.out <- public.hosp |> select(!(c("q1", "q2", "q3", "q4"))) |>
                               sumtable(out = "return")

desc.tab.out <- gt(desc.tab.out)

desc.tab.out |> tab_header(
                    title = md("**Table of Descriptive Statistics**")) |>
                tab_source_note(
                    source_note = md("*Source: Meier, Johnson, and An (2019).*"))

public.hosp.recode <- public.hosp |> 
                            mutate(agesq = age^2,
                                   age4cat = case_when(
                                     age < 35 ~ 0,
                                     age >= 35 & age < 50 ~ 1,
                                     age >= 50 & age < 65 ~ 2,
                                     age >= 65 ~ 3,
                                     TRUE ~ NA_real_
                                   ))
table(public.hosp.recode$age4cat)

public.hosp.recode$age4cat <- ordered(public.hosp.recode$age4cat,
                                      labels = c("0-34", "35-49", "50-64", "65+"))

round(prop.table(table(public.hosp.recode$age4cat)) * 100, digits = 1)

CT.summary <- table(public.hosp$q9, public.hosp$public) 
CT.summary

round(prop.table(CT.summary, 2) * 100, digits = 1)

MC.tab <- public.hosp |> 
                group_by(public) |>
                summarize("Mean of q9" = mean(q9, na.rm = TRUE))
MC.tab

t.test(q9 ~ public, data = public.hosp, var.equal = TRUE)
