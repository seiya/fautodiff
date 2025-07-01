OUTDIR ?= .

FC ?= gfortran
FFLAGS ?= -O2

vpath %.f90 ../examples
vpath %.f90 ../fortran_modules
vpath %.f90 .

PROGRAMS = run_simple_math run_arrays run_call_example run_control_flow run_cross_mod            run_data_storage run_intrinsic_func run_real_kind run_save_vars run_store_vars

all: $(PROGRAMS)

$(OUTDIR)/%.o: %.f90
	$(FC) $(FFLAGS) -c $< -J $(OUTDIR) -o $@

# AD modules depend on their original modules
$(OUTDIR)/%_ad.o: $(OUTDIR)/%.mod

# Additional module dependencies
$(OUTDIR)/store_vars_ad.o: $(OUTDIR)/store_vars.mod $(OUTDIR)/fautodiff_data_storage.mod
$(OUTDIR)/cross_mod_b.o: $(OUTDIR)/cross_mod_a.mod
$(OUTDIR)/cross_mod_b_ad.o: $(OUTDIR)/cross_mod_b.mod $(OUTDIR)/cross_mod_a_ad.mod

run_simple_math: $(OUTDIR)/run_simple_math.o $(OUTDIR)/simple_math.o $(OUTDIR)/simple_math_ad.o
	$(FC) $^ -o $@

run_arrays: $(OUTDIR)/run_arrays.o $(OUTDIR)/array.o $(OUTDIR)/array_ad.o
	$(FC) $^ -o $@

run_call_example: $(OUTDIR)/run_call_example.o $(OUTDIR)/call_example.o $(OUTDIR)/call_example_ad.o
	$(FC) $^ -o $@

run_control_flow: $(OUTDIR)/run_control_flow.o $(OUTDIR)/control_flow.o $(OUTDIR)/control_flow_ad.o
	$(FC) $^ -o $@

run_cross_mod: $(OUTDIR)/run_cross_mod.o $(OUTDIR)/cross_mod_a.o $(OUTDIR)/cross_mod_a_ad.o                $(OUTDIR)/cross_mod_b.o $(OUTDIR)/cross_mod_b_ad.o
	$(FC) $^ -o $@

run_data_storage: $(OUTDIR)/run_data_storage.o $(OUTDIR)/fautodiff_data_storage.o
	$(FC) $^ -o $@

run_intrinsic_func: $(OUTDIR)/run_intrinsic_func.o $(OUTDIR)/intrinsic_func.o $(OUTDIR)/intrinsic_func_ad.o
	$(FC) $^ -o $@

run_real_kind: $(OUTDIR)/run_real_kind.o $(OUTDIR)/real_kind.o $(OUTDIR)/real_kind_ad.o
	$(FC) $^ -o $@

run_save_vars: $(OUTDIR)/run_save_vars.o $(OUTDIR)/save_vars.o $(OUTDIR)/save_vars_ad.o
	$(FC) $^ -o $@

run_store_vars: $(OUTDIR)/run_store_vars.o $(OUTDIR)/store_vars.o $(OUTDIR)/store_vars_ad.o $(OUTDIR)/fautodiff_data_storage.o
	$(FC) $^ -o $@

clean:
	rm -f $(OUTDIR)/*.o $(OUTDIR)/*.mod
