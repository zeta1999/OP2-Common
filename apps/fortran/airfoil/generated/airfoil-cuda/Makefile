IC = pgcc
FC = pgfortran
OP2 = $(OP2_INSTALL_PATH)

EXEC = airfoil_cuda
F_OP2_MOD = $(OP2)/fortran/mod/pgi
F_OP2_LIB = $(OP2)/fortran/lib

OPT = -Mcuda=cuda4.2 -Minform=inform -O2 -fast -Mcuda=cc20
FLINK = -L$(F_OP2_LIB)
FMODS = -I$(F_OP2_MOD)

all: airfoil_seq link

airfoil_seq: cudaConfigurationParams.F95 constvars.F95 input.F95 constvars.F95 rose_cuda_code.CUF rose_airfoil.F95
	$(FC) $(OPT) $(FMODS) -c cudaConfigurationParams.F95 constvars.F95 input.F95 rose_cuda_code.CUF rose_airfoil.F95

link: cudaConfigurationParams.o input.o constvars.o rose_cuda_code.o rose_airfoil.o
	$(FC) $(OPT) $(FLINK) cudaConfigurationParams.o input.o constvars.o rose_cuda_code.o rose_airfoil.o -lop2_for_cuda -o $(EXEC)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(EXEC)
	rm -f *~
