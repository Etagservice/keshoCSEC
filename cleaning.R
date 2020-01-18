
library("tidyverse")
library("here")
library(readxl)


CSEC_DATA = read_excel(here("/../csec_data.xlsx"))

csec_fields = read_csv(here("/../csec_fields.csv"))
