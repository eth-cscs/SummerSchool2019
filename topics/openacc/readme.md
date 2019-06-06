# OpenACC exercises

This folder contains the following subfolders:

1. `practicals/`: The exercises for the students.
2. `solutions/`: The directory where the solutions will be uploaded at the end of the school.
3. `ci/`: A continuous integration script for testing all the examples (not exhaustively).
   On Daint you can run as follows:
   
   ```bash
   module load reframe
   reframe --prefix=$SCRATCH/ci -c ci/practical_tests.py --reserv=course -r
   ```
