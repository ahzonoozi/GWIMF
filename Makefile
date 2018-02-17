# Start of the Makefile
# Defining variables
objects = main.o MAX_Mecl.o MAX_MSTAR.o MASS_FUNC.o 
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
MAX_Mecl.o: MAX_Mecl.f90
	 $(F90) -c MAX_Mecl.f90
MAX_MSTAR.o: MAX_MSTAR.f90
	 $(F90) -c MAX_MSTAR.f90
MASS_FUNC.o: MASS_FUNC.f90
	 $(F90) -c MASS_FUNC.f90

# Cleaning everything
clean:
	 rm  GWIMF.out
	 rm $(objects)
	 rm *~ *.txt *.log fort.*
	 rm *.o
# End of the makefile
