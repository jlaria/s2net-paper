#!/bin/sh
# embedded options to bsub - start with #BSUB
### -- set the job Name --
#BSUB -J MPIJob-test
### specify queue --
#BSUB -q compute
### -- ask for number of cores (default: 1) --
#BSUB -n 128
##BSUB -R "span[hosts=1]"
#BSUB -R "span[block=4]"
### -- set walltime limit: hh:mm --
#BSUB -W 72:00 
### -- specify that we need 2GB of memory per core/slot -- 
#BSUB -R "rusage[mem=0.5GB]"
### -- set the email address --
# please uncomment the following line and put in your e-mail address,
# if you want to receive e-mail notifications on a non-default address
##BSUB -u jlaria@est-econ.uc3m.es
### -- send notification at start --
##BSUB -B
### -- send notification at completion--
##BSUB -N
### -- Specify the output and error file. %J is the job-id -- 
### -- -o and -e mean append, -oo and -eo mean overwrite -- 
#BSUB -o Output_%J.out
#BSUB -e Error_%J.err
# here follow the commands you want to execute
# 

# load the necessary modules
# NOTE: this is just an example, check with the available modules
# module load gcc
# module load mpi/2.1.1-gcc-7.2.0
### This uses the LSB_DJOB_NUMPROC to assign all the cores reserved
### This is a very basic syntax. For more complex examples, see the documentation

module load mpi/4.0.1-gcc-8.3.0

mpirun -np $LSB_DJOB_NUMPROC ~/miniconda3/bin/R --slave -f main_script_mpi.R
