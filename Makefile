# Start of the Makefile
# Defining variables
objects = main.o max_mecl.o max_mstar.o massfunc.o 
#======================
# Fortran 90 compiler
# (Uncomment only one)
#======================
# GNU
F90 = gfortran
#---------------------
# Intel
#F90 = ifort
#---------------------

# Makefile
GWIMF.out: $(objects)
	$(F90) -o GWIMF.out $(objects)

main.o: main.f90
	 $(F90) -c main.f90
max_mecl.o: max_mecl.f90
	 $(F90) -c max_mecl.f90
max_mstar.o: max_mstar.f90
	 $(F90) -c max_mstar.f90
massfunc.o: massfunc.f90
	 $(F90) -c massfunc.f90

# Cleaning everything
clean:
	 rm  GWIMF.out
	 rm $(objects)
	 rm *~ *.txt *.log fort.*
	 rm *.o
# End of the makefile
