.SUFFIXES: .o .f90

FC  =gfortran
LIBS= #-L/usr/lib/x86_64-linux-gnu -lnetcdf -lnetcdff -lm
INC = #-I/usr/include

FCFLAGS=-O1 -ffree-line-length-none #-Wunused #-Wall#
LDFLAGS=-O1

OBJS=geotiff.o test.o
EXE =a.out

$(EXE): $(OBJS)
	$(FC) $(LDFLAGS) $(OBJS) $(LIBS) -o $(EXE)
%.o: %.f90
	$(FC) -c $(FCFLAGS) $(INC) $< -o $@
clean:
	rm -f *.o *.mod a.out
