import logging
import logging.config

import luigi
from luigi import configuration
import subprocess
import os
import json

logging_conf = configuration.get_config().get("core", "logging_conf_file")
logging.config.fileConfig(logging_conf)
logger = logging.getLogger("lda.pipeline")


class LDAExperiment(luigi.WrapperTask):
    conf = configuration.get_config()

    def requires(self):
        # get the experiments configuration file
        with open(self.conf.get("expers", "master")) as f:
            experiment = json.load(f)

        tasks = []
        logger.info(experiment)
        for (k, v) in enumerate(experiment):
            logger.info(v)
            tasks.append(
                LDAParams(
                    str(v["id"]),
                    str(v["D"]),
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
    def requires(self):

    
class LDAParams(luigi.ExternalTask):
    """
    Simulate parameters for an LDA model
    """
    id  = luigi.Parameter()
    D = luigi.Parameter()
    V = luigi.Parameter()
    K = luigi.Parameter()
    alpha0 = luigi.Parameter()
    gamma0 = luigi.Parameter()

    conf = configuration.get_config()

    def run(self):

        run_cmd = [
            "Rscript",
            self.conf.get("expers", "param_script"),
            self.conf.get("expers", "output_dir"),
            self.id, self.D, self.V, self.K,
            self.alpha0, self.gamma0
        ]

        run_cmd = [str(s) for s in run_cmd]
        status = subprocess.call(run_cmd)

        if status is not 0:
            raise ValueError("Parameter generation failed")

    def output(self):
        output_dir = self.conf.get("expers", "output_dir")
        return [
            luigi.LocalTarget(os.path.join(output_dir, "beta-" + self.id + ".feather")),
            luigi.LocalTarget(os.path.join(output_dir, "theta-" + self.id + ".feather"))
        ]


if __name__ == "__main__":
    luigi.run()
