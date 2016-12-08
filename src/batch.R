#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

output_path <- function(dir) {
  function(filename) {
    file.path(dir, filename)
  }
}

merge_defaults <- function(opts) {
  defaults <- list(
    "modules" = c("R/3.3.0"),
    "time_alloc" = "00:15:00",
    "mem_alloc" = 2000,
    "email" = "kriss1@stanford.edu",
    "partitions" = "normal,hns",
    "mail_type" = "ALL"
  )
  modifyList(defaults, opts)
}

create_job <- function(outfile, jobname, script_lines, opts = list()) {
  opts <- merge_defaults(opts)
  slurm_opts <- c(
    "#!/bin/bash",
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
    sprintf("#SBATCH --time=%s", opts$time_alloc),
    "#################",
    "#memory per node; default is 4000 MB per CPU",
    sprintf("#SBATCH --mem=%d", opts$mem_alloc),
    sprintf("#SBATCH --partition=%s", opts$partitions),
    sprintf("#SBATCH --mail-type=%s", opts$mail_type),
    sprintf("#SBATCH --mail-user=%s", opts$email)
  )
  ml_opts <- paste("ml", opts$modules) %>%
    paste(collapse = "\n")

  cat(
    c(slurm_opts, ml_opts, script_lines),
    sep = "\n",
    file = outfile
  )
}
