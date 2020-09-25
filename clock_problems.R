#!/usr/bin/env Rscript

## Create clock problems for kids
## Author: John Horton
## Date: 9/25/2020
## License: Have at it - no rights reserved.

suppressPackageStartupMessages({
    library(magrittr)
    library(dplyr)
    library(ggplot2)
})

N <- 16
hours <- sample(1:12, N, TRUE)
minutes <- sample(seq(0, 60, 5), N, TRUE)

AngleHour <- function(H, M) -2*pi * (H-3 + M/60)/12
AngleMin <- function(M)  -2*pi *  (M-15)/60

h.a <- mapply(AngleHour, hours, minutes)
m.a <- sapply(minutes, AngleMin)
x.h <- (1/2)*cos(h.a)
y.h <- (1/2)*sin(h.a)
x.m <- cos(m.a)
y.m <- sin(m.a)

df <- data.frame(hours,
                 minutes,
                 x.h,
                 y.h,
                 x.m,
                 y.m,
                 index = 1:N) %>%
    mutate(correct.answer = paste0(hours,":",
                                   ifelse(minutes < 10,
                                          paste0("0",minutes),
                                          minutes)))

## Add labels for hours
df.positions <- data.frame(
    number = 1:12,
    angle = sapply(1:12, function(h) AngleHour(h, 0))) %>%
    mutate(
        x = cos(angle),
        y = sin(angle)
    )

g.base <- ggplot(data = df) +
    geom_segment(aes(x = x.h, xend = 0, y = y.h, yend = 0)) +
    geom_segment(aes(x = x.m, xend = 0, y = y.m, yend = 0)) +
    geom_text(data = df.positions, aes(x = x, y = y, label = number)) +
    theme_bw() +
    facet_wrap(~index, ncol = 4) + 
    geom_point(x = 0, y = 0) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()
          )

pdf("test.pdf", width = 10, height = 10)
g.base
dev.off()

pdf("answer_key.pdf", width = 10, height = 10)
g.base +
    geom_text(data = df,
              aes(x = 0.9, y = 0.9, label = correct.answer),
              colour = "red")
dev.off()

