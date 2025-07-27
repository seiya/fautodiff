OUTDIR ?= $(shell pwd)

FC = gfortran
MPIFC ?= mpifort
FFLAGS = -O2 -ffree-line-length-none \
         -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check -finit-real=nan -g -fbacktrace

SRCS := $(wildcard *.f90)
OBJS := $(addprefix $(OUTDIR)/,$(SRCS:.f90=.o))

HELPER_DIR = ../fortran_modules
HELPER_SRCS = $(wildcard $(HELPER_DIR)/*.f90)
HELPER_OBJS = $(addprefix $(OUTDIR)/,$(notdir $(HELPER_SRCS:.f90=.o)))

vpath %.f90 $(HELPER_DIR)
vpath %.f90 .

all: $(HELPER_OBJS) $(OBJS)

$(OUTDIR)/mpi_%.o: mpi_%.f90
	$(MPIFC) $(FFLAGS) -c $< -I$(OUTDIR) -J $(OUTDIR) -o $@

$(OUTDIR)/%.o: %.f90
	$(FC) $(FFLAGS) -c $< -I$(OUTDIR) -J $(OUTDIR) -o $@

# AD modules depend on their original modules
$(OUTDIR)/%_ad.o: $(OUTDIR)/%.o

# Cross-module dependencies
$(OUTDIR)/cross_mod_b.o: $(OUTDIR)/cross_mod_a.o
$(OUTDIR)/cross_mod_b_ad.o: $(OUTDIR)/cross_mod_b.mod $(OUTDIR)/cross_mod_a_ad.o
$(OUTDIR)/call_module_vars.o: $(OUTDIR)/module_vars.o
$(OUTDIR)/call_module_vars_ad.o: $(OUTDIR)/module_vars.o $(OUTDIR)/module_vars_ad.o
$(OUTDIR)/store_vars_ad.o: $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/module_vars_ad.o: $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/allocate_vars_ad.o: $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/exit_cycle_ad.o: $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/mpi_example_ad.o: $(OUTDIR)/mpi_ad.o

clean:
	rm -f $(OUTDIR)/*.o $(OUTDIR)/*.mod
