#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

#' Function to concatenate path to file
#'
#' This is a wrapper function that simplifies specifying long paths to
#' files. It returns a function that, when called on a filename, returns
#' the path to the original directory + that filename.
#'
#' @param dir [string] The parent directory the returned function
#'   should concatenate names to.
#' @return f [function] A function that, when called on a filename,
#'   concatenates dir over that filename.
output_path <- function(dir) {
  function(filename) {
    file.path(dir, filename)
  }
}

#' Merge default batch options
#'
#' This will merge in defaults to the opts in create_job, so that we
#' don't need to specify all the options every time.
#'
#' @param opts [list] A list partially specifying options expected by
#'   create_job.
#' @return opts [list] The original opts list with unspecified opts
#'   filled in with defaults.
merge_defaults <- function(opts) {
  defaults <- list(
    "modules" = "R/3.3.0",
    "time_alloc" = "01:00:00",
    "mem_alloc" = 1000,
    "email" = "kriss1@stanford.edu",
    "partitions" = "normal,hns",
    "mail_type" = "ALL"
  )
  modifyList(defaults, opts)
}

#' Create a bash file to submit a script to SLURM
#' @param outfile [string] The path / filename to save the bash file.
#' @param jobname [string] What should the jobname be on slurm?
#' @param script_lines [string] A command that should be executed on
#'   the terminal on the cluster machine.
#' @param opts [list] Batch submission options. See merge_defaults for
#'   default arguments.
#' @return NULL
#' @side-effects Writes a file to outfile giving the script that can
#'   be sbatched.
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
    sprintf("#SBATCH --error=/scratch/users/kriss1/batch/%s.err", jobname),
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
