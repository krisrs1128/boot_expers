"""
Pipeline for Evaluating Unigram inferences on Simulated Data
"""

import logging
import logging.config

import luigi
from luigi import configuration
import subprocess
import os
import hashlib
import json

logging_conf = configuration.get_config().get("core", "logging_conf_file")
logging.config.fileConfig(logging_conf)
logger = logging.getLogger("unigram.pipeline")

###############################################################################
# Minor utilities used throughout the pipeline
###############################################################################

def hash_string(string, max_chars=32):
    """
    Return the sha1 hash of a string
    """
    hash_obj = hashlib.sha1(string.encode())
    return hash_obj.hexdigest()[:max_chars]


def fit_id(self):
    return hash_string(
        "".join([self.K_fit, self.sigma_fit, self.D, self.N, self.V, self.K,
                 self.sigma, self.conf.get("expers", "n_samples")])
    )


def run_and_check(cmds):
    run_cmd = [str(s) for s in cmds]
    status = subprocess.call(run_cmd)
    if status is not 0:
        raise ValueError("Bash commands failed")

###############################################################################
# Core pipeline classes
###############################################################################

class UnigramParams(luigi.ExternalTask):
    """
    Simulate parameters for a Unigram model

    This generates the parameters mu[t] for a particular instance of the
    dynamic unigram model.

    Arguments:
      D (int): How many samples are there in this experiment?
      V (int): How many terms are there across samples?
      sigma0 (float): What is the true sigma random walk size parameter used in
      generating the data?
    """
    D = luigi.Parameter()
    V = luigi.Parameter()
    sigma0 = luigi.Parameter()

    conf = configuration.get_config()

    def run(self):
        raise(NotImplementedError())

    def output(self):
        raise(NotImplementedError())
