import os

import reframe as rfm
import reframe.utility.sanity as sn


@rfm.parameterized_test(['cuda'], ['openacc'])
class MiniAppTest(rfm.RegressionTest):
    def __init__(self, variant):
        super().__init__()
        self.valid_systems = ['daint:gpu']
        self.sourcesdir = os.path.join('../', variant)
        if variant == 'cuda':
            self.valid_prog_environs = ['PrgEnv-gnu']
        elif variant == 'openacc':
            self.valid_prog_environs = ['PrgEnv-pgi']

        if self.current_system.name == 'daint':
            self.modules = ['craype-accel-nvidia60']

        self.executable = './main'
        self.executable_opts = ['256', '256', '100', '0.01']
        self.keep_files = ['output.bin', 'output.bov']
        self.time_limit = (0, 5, 0)
        self.sanity_patterns = sn.allx([
            sn.assert_found(r'Goodbye\!', self.stdout),
            sn.assert_not_found(r'ERROR', self.stdout)
        ])
        self.perf_patterns = {
            'exec_time': sn.extractsingle(r'simulation took (\S+) seconds',
                                          self.stdout, 1, float),
            'perf': sn.extractsingle(r'at rate of (\S+) iters/second',
                                     self.stdout, 1, float)
        }
        self.reference = {
            'daint:gpu': {
                'exec_time': (0, None, None, 's'),
                'perf': (0, None, None, 'iters/s')
            },
            '*': {
                'exec_time': (0, None, None, 's'),
                'perf': (0, None, None, 'iters/s')
            }
        }
        self.maintainers = ['karakasis<at>cscs.ch']
