import os
import reframe as rfm
import reframe.core.runtime as rt
import reframe.utility.sanity as sn


class OpenACCBaseTest(rfm.RegressionTest):
    def __init__(self):
        super().__init__()
        self.valid_systems = ['dom:gpu', 'daint:gpu']
        self.valid_prog_environs = ['PrgEnv-cray', 'PrgEnv-pgi']
        self.sourcesdir = os.path.join(self.prefix, '../practicals')
        self.modules = ['craype-accel-nvidia60']
        self.maintainers = ['karakasis<at>cscs.ch']
        self.tags = {'openacc-training', 'summerschool'}

    # Remove underscore to test with PGI 18.4 (but use -p PrgEnv-pgi only!)
    def _setup(self, partition, environ, **job_opts):
        if environ.name == 'PrgEnv-pgi':
            rt.runtime().modules_system.searchpath_add(
                '/apps/common/UES/pgi/18.4/modulefiles')
            self.modules += ['pgi/18.4']
            self.variables.update({'PGI_VERS_STR': '18.4.0'})
            self.pre_run = ['module use /apps/common/UES/pgi/18.4/modulefiles']

        super().setup(partition, environ, **job_opts)

    def compile(self):
        self.current_environ.propagate = False
        super().compile()


@rfm.parameterized_test(['c'], ['fortran'])
class AXPYExample(OpenACCBaseTest):
    def __init__(self, lang='c'):
        super().__init__()
        if lang == 'fortran':
            self.executable = './axpy/axpy.openacc.fort'
        else:
            self.executable = './axpy/axpy.openacc'

        self.sourcepath = 'axpy/'
        self.sanity_patterns = sn.assert_found('PASSED', self.stdout)


@rfm.parameterized_test(['omp'], ['openacc'], ['openacc+cuda'],
                        ['openacc+cuda+mpi'], ['openacc+mpi'],
                        ['openacc+fort'], ['openacc+fort+mpi'])
class DiffusionExample(OpenACCBaseTest):
    def __init__(self, version):
        super().__init__()
        self.sourcepath = 'diffusion/'
        self.executable = ('./diffusion/diffusion2d.%s' %
                           version.replace('+', '.'))
        if 'mpi' in version:
            self.num_tasks = 2
            self.num_tasks_per_node = 1
            self.variables = {
                'CRAY_CUDA_MPS': '1',
                'MPICH_RDMA_ENABLED_CUDA': '1'
            }

        self.sanity_patterns = sn.assert_found('writing to output',
                                               self.stdout)
        self.keep_files = ['output.bin', 'output.bov']


@rfm.simple_test
class GemmExample(OpenACCBaseTest):
    def __init__(self):
        super().__init__()
        self.sourcepath = 'gemm/'
        self.executable = './gemm/gemm.openacc'
        self.num_cpus_per_task = 12
        self.variables = {'OMP_NUM_THREADS': str(self.num_cpus_per_task)}
        self.sanity_patterns = sn.assert_eq(
            4, sn.count(sn.extractall('success', self.stdout))
        )


@rfm.parameterized_test(['openacc'], ['openacc+fort'])
class BlurExample(OpenACCBaseTest):
    def __init__(self, version):
        super().__init__()
        self.sourcepath = 'basics/'
        self.executable = './basics/blur.%s' % version.replace('+', '.')
        self.sanity_patterns = sn.assert_found('success', self.stdout)


@rfm.parameterized_test(['openacc'], ['openacc+fort'])
class DotExample(OpenACCBaseTest):
    def __init__(self, version):
        super().__init__()
        self.sourcepath = 'basics/'
        self.executable = './basics/dot.%s' % version.replace('+', '.')
        self.sanity_patterns = sn.assert_found('success', self.stdout)


@sn.sanity_function
def dset(iterable):
    return set(iterable)


#@rfm.simple_test
class ImagePipelineExample(OpenACCBaseTest):
    def __init__(self):
        super().__init__()
        self.sourcepath = 'image-pipeline/'
        self.valid_prog_environs = ['PrgEnv-pgi']

        # We need to reload the PGI compiler here, cos OpenCV loads PrgEnv-gnu
        self.modules = ['craype-accel-nvidia60', 'OpenCV', 'pgi']
        self.executable = './image-pipeline/filter.x'
        self.executable_opts = ['image-pipeline/california-1751455_1280.jpg',
                                'image-pipeline/output.jpg']
        self.sanity_patterns = sn.assert_eq(
            {'original', 'blocked', 'update', 'pipelined', 'multi'},
            dset(sn.extractall('Time \((\S+)\):.*', self.stdout, 1)))
        self.tags = {'openacc-training'}


#@rfm.parameterized_test(['openacc'], ['openacc+fort'])
class DeepcopyExample(OpenACCBaseTest):
    def __init__(self, version):
        super().__init__()
        self.sourcepath = 'deepcopy/'
        self.valid_prog_environs = ['PrgEnv-pgi']
        self.modules = ['craype-accel-nvidia60']
        self.executable = './deepcopy/deepcopy.%s' % version.replace('+', '.')
        self.sanity_patterns = sn.assert_found('3', self.stdout)
