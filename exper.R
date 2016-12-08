#! /usr/bin/env Rscript

# File description -------------------------------------------------------------
# This is the master script for one bootstrapping experiment. This work is very
# preliminary.
#
# author: kriss1@stanford.edu

## ---- setup ----
library("jsonlite")
library("data.table")
library("feather")
source("src/lda_data.R")

## ---- read-parameters ----
exper <- fromJSON("exper.json")

## ---- setup-directories ----
paths <- exper$paths
dir.create(paths$base)
dir.create(file.path(paths$base, paths$params_dir))
dir.create(file.path(paths$base, paths$data_dir))
dir.create(file.path(paths$base, paths$output_dir))

## generate and save data
model <- exper$model
params <- generate_params(model$D, model$K, model$V, model$alpha0, model$gamma0)
X <- generate_data(model$N, params$theta, params$beta)
write_feather(data.table(params$theta), file.path(paths$base, paths$params_dir, "theta.feather"))
write_feather(data.table(params$beta), file.path(paths$base, paths$params_dir, "beta.feather"))
write_feather(data.table(X), file.path(paths$base, paths$data_dir, "X.feather"))

## send off estimation jobs
