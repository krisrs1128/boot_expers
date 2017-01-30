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
logger = logging.getLogger("lda.pipeline")


def hash_string(string, max_chars=32):
    """
    Return the sha1 hash of a string
    """
    hash_obj = hashlib.sha1(string.encode())
    return hash_obj.hexdigest()[:max_chars]


def run_and_check(cmds):
    run_cmd = [str(s) for s in cmds]
    status = subprocess.call(run_cmd)
    if status is not 0:
        raise ValueError("Bash commands failed")


class LDAExperiment(luigi.WrapperTask):
    conf = configuration.get_config()

    def requires(self):
        # get the experiments configuration file
        with open(self.conf.get("expers", "master")) as f:
            experiment = json.load(f)

        tasks = []
        for (k, v) in enumerate(experiment):
            tasks.append(
                LDAData(
                    str(v["D"]),
                    str(v["N"]),
                    str(v["V"]),
                    str(v["K"]),
                    str(v["alpha0"]),
                    str(v["gamma0"])
                )
            )

        return tasks


class LDAData(luigi.Task):
    """
    Simulate data from an LDA model, given the theta and beta parameters
    """
    D = luigi.Parameter()
    N = luigi.Parameter()
    V = luigi.Parameter()
    K = luigi.Parameter()
    alpha0 = luigi.Parameter()
    gamma0 = luigi.Parameter()

    conf = configuration.get_config()

    def requires(self):
        return LDAParams(self.D, self.V, self.K, self.alpha0, self.gamma0)

    def run(self):
        beta_path = self.input()[0].open("r").name
        theta_path = self.input()[1].open("r").name
        gen_id = hash_string("".join([self.D, self.N, self.V, self.K, self.alpha0, self.gamma0]))

        run_cmd = [
            "Rscript",
            self.conf.get("expers", "sim_script"),
            self.conf.get("expers", "output_dir"),
            gen_id, self.N, beta_path, theta_path
        ]
        run_and_check(run_cmd)

    def output(self):
        gen_id = hash_string("".join([self.D, self.N, self.V, self.K, self.alpha0, self.gamma0]))
        output_dir = self.conf.get("expers", "output_dir")
        return luigi.LocalTarget(os.path.join(output_dir, "N-" + gen_id + ".feather"))


class LDAParams(luigi.ExternalTask):
    """
    Simulate parameters for an LDA model
    """
    D = luigi.Parameter()
    V = luigi.Parameter()
    K = luigi.Parameter()
    alpha0 = luigi.Parameter()
    gamma0 = luigi.Parameter()

    conf = configuration.get_config()

    def run(self):
        gen_id = hash_string("".join([self.D, self.V, self.K, self.alpha0, self.gamma0]))
        run_cmd = [
            "Rscript",
            self.conf.get("expers", "param_script"),
            self.conf.get("expers", "output_dir"),
            gen_id, self.D, self.V, self.K,
            self.alpha0, self.gamma0
        ]
        run_and_check(run_cmd)

    def output(self):
        gen_id = hash_string("".join([self.D, self.V, self.K, self.alpha0, self.gamma0]))
        output_dir = self.conf.get("expers", "output_dir")
        return [
            luigi.LocalTarget(os.path.join(output_dir, "beta-" + gen_id + ".feather")),
            luigi.LocalTarget(os.path.join(output_dir, "theta-" + gen_id + ".feather"))
        ]


if __name__ == "__main__":
    luigi.run()
