EXE = 
OBJ = caseConverter.o ncio.o

FC = ifort
FLAGS = -O2 -warn all -traceback

INC = $(shell nf-config --fflags)
LIB = $(shell nf-config --flibs)

%.o : %.f90
	${FC} -c $< ${FLAGS} ${INC}

all : ${OBJ}

${EXE} : ${OBJ}
	${FC} -o $@ $^ ${LIB}


.PHONY : clean re

clean :
	rm -fv *.o *.mod ${EXE}

re : clean all

