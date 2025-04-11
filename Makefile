EXE = 
OBJ = caseConverter.o ncio.o

FC = ifort
FLAGS = -O2 -warn all -traceback

INC = -I/home/kosei/FortranLib/build/netcdf/include -I/home/kosei/FortranLib/build/netcdf/include
LIB = -L/home/kosei/FortranLib/build/netcdf/lib -lnetcdff -L/home/kosei/FortranLib/build/netcdf/lib -lnetcdf -lnetcdf -lm

%.o : %.f90
	${FC} -c $< ${FLAGS} ${INC}

all : ${OBJ}

${EXE} : ${OBJ}
	${FC} -o $@ $^ ${LIB}


.PHONY : clean re

clean :
	rm -fv *.o *.mod ${EXE}

re : clean all

