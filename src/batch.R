#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

output_path <- function(dir) {
  function(filename) {
    file.path(dir, filename)
  }
}

create_job <- function(outfile,
                       jobname,
                       script_lines,
                       modules = c("R/3.3.0"),
                       time_alloc = "00:15:00",
                       mem_alloc = 2000) {
  slurm_opts <- c("#!/bin/bash",
                  "#",
                  "#all commands that start with SBATCH contain commands that are just used by SLURM for scheduling",
                  "#################",
                  "#set a job name",
                  sprintf("#SBATCH --job-name=%s", jobname),
                  "#################",
                  "#a file for job output, you can check job progress",
                  sprintf("#SBATCH --output=%s.out", jobname),
                  "#################",
                  "# a file for errors from the job",
                  sprintf("#SBATCH --error=%s.err", jobname),
                  "#################",
                  "#time you think you need; default is one hour",
                  "#in minutes in this case, hh:mm:ss",
                  sprintf("#SBATCH --time=%s", time_alloc),
                  "#################",
                  "#memory per node; default is 4000 MB per CPU",
                  sprintf("#SBATCH --mem=%d", mem_alloc),
                  "#SBATCH --mail-type=ALL",
                  "#SBATCH --mail-user=kriss1@stanford.edu"
                  )
  ml_opts <- paste("ml", modules) %>%
    paste(collapse = "\n")

  cat(
    c(slurm_opts, ml_opts, script_lines),
    sep = "\n",
    file = outfile
  )
}
