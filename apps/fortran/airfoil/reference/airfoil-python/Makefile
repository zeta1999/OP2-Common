IC = pgcc
#/home/gihan/openmpi-intel/bin/mpicc -O2
#-debug full
FC = pgfortran
#/opt/intel/ifort-2011.4.191/bin/ifort -O2
#-debug full

OP2 = $(OP2_INSTALL_PATH)

OP2_MOD = $(OP2)/fortran/mod/pgi
OP2_LIB = $(OP2)/fortran/lib

PROF=../OpenMP/support

all: airfoil_seq link


airfoil_seq:  constvars.f90 airfoil_seqfun.f90 input.f90 output.f90 airfoil.f90
	$(FC) -I$(OP2_MOD) -c constvars.f90 airfoil_seqfun.f90 input.f90 output.f90 airfoil.f90

link:   constvars.o airfoil.o airfoil_seqfun.o input.o output.o
	$(FC) constvars.o airfoil_seqfun.o input.o output.o airfoil.o -o airfoil_seq -L$(OP2_LIB) -lop2_for_reference
clean:
	rm -f *.o
	rm -f *.mod
	rm -f airfoil_seq
