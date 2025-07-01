OUTDIR ?= .

FC ?= gfortran
FFLAGS ?= -O2

SRCS := $(wildcard *.f90)
OBJS := $(addprefix $(OUTDIR)/,$(SRCS:.f90=.o))

all: $(OBJS)

$(OUTDIR)/%.o: %.f90
	$(FC) $(FFLAGS) -c $< -J $(OUTDIR) -o $@

# AD modules depend on their original modules
$(OUTDIR)/%_ad.o: $(OUTDIR)/%.mod

# Cross-module dependencies
$(OUTDIR)/cross_mod_b.o: $(OUTDIR)/cross_mod_a.mod
$(OUTDIR)/cross_mod_b_ad.o: $(OUTDIR)/cross_mod_b.mod $(OUTDIR)/cross_mod_a_ad.mod

clean:
	rm -f $(OUTDIR)/*.o $(OUTDIR)/*.mod
