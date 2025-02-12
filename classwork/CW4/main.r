#!/usr/bin/env Rscript

Advertising <- read.csv("../../data/Advertising.csv")

sales_lm_Full = lm(sales~TV+radio+newspaper)
