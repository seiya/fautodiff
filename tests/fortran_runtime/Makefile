OUTDIR ?= .

FC ?= gfortran
FFLAGS ?= -O2 -ffree-line-length-none

vpath %.f90 ../../examples
vpath %.f90 ../../fortran_modules
vpath %.f90 .

PROGRAMS = run_simple_math run_arrays run_call_example run_control_flow run_cross_mod \
	   run_data_storage run_intrinsic_func run_real_kind run_save_vars run_store_vars \
	   run_directives run_parameter_var run_module_vars run_allocate

all: $(PROGRAMS)

$(OUTDIR)/%.o: %.f90
	$(FC) $(FFLAGS) -c $< -J $(OUTDIR) -o $@

# AD modules depend on their original modules
$(OUTDIR)/%_ad.o: $(OUTDIR)/%.mod

# Special case: module 'array' is defined in arrays.f90
$(OUTDIR)/arrays_ad.o: $(OUTDIR)/arrays.mod

# driver module dependencies
$(OUTDIR)/run_simple_math.o: $(OUTDIR)/simple_math.o $(OUTDIR)/simple_math_ad.o
$(OUTDIR)/run_arrays.o: $(OUTDIR)/arrays.o $(OUTDIR)/arrays_ad.o
$(OUTDIR)/run_call_example.o: $(OUTDIR)/call_example.o $(OUTDIR)/call_example_ad.o
$(OUTDIR)/run_control_flow.o: $(OUTDIR)/control_flow.o $(OUTDIR)/control_flow_ad.o
$(OUTDIR)/run_cross_mod.o: $(OUTDIR)/cross_mod_a.o $(OUTDIR)/cross_mod_a_ad.o \
  $(OUTDIR)/cross_mod_b.o $(OUTDIR)/cross_mod_b_ad.o
$(OUTDIR)/run_data_storage.o: $(OUTDIR)/data_storage.o
$(OUTDIR)/run_intrinsic_func.o: $(OUTDIR)/intrinsic_func.o $(OUTDIR)/intrinsic_func_ad.o
$(OUTDIR)/run_real_kind.o: $(OUTDIR)/real_kind.o $(OUTDIR)/real_kind_ad.o
$(OUTDIR)/run_save_vars.o: $(OUTDIR)/save_vars.o $(OUTDIR)/save_vars_ad.o
$(OUTDIR)/run_store_vars.o: $(OUTDIR)/store_vars.o $(OUTDIR)/store_vars_ad.o \
  $(OUTDIR)/data_storage.o
$(OUTDIR)/run_directives.o: $(OUTDIR)/directives.o $(OUTDIR)/directives_ad.o
$(OUTDIR)/run_parameter_var.o: $(OUTDIR)/parameter_var.o $(OUTDIR)/parameter_var_ad.o
$(OUTDIR)/run_module_vars.o: $(OUTDIR)/module_vars.o $(OUTDIR)/module_vars_ad.o
$(OUTDIR)/run_allocate.o: $(OUTDIR)/allocate.o $(OUTDIR)/allocate_ad.o

# Additional module dependencies
$(OUTDIR)/store_vars_ad.o: $(OUTDIR)/store_vars.mod $(OUTDIR)/data_storage.o
$(OUTDIR)/cross_mod_b.o: $(OUTDIR)/cross_mod_a.mod
$(OUTDIR)/cross_mod_b_ad.o: $(OUTDIR)/cross_mod_b.mod $(OUTDIR)/cross_mod_a_ad.mod
$(OUTDIR)/allocate_ad.o: $(OUTDIR)/allocate_example.mod

run_simple_math: $(OUTDIR)/run_simple_math.o $(OUTDIR)/simple_math.o $(OUTDIR)/simple_math_ad.o
	$(FC) $^ -o $@

run_arrays: $(OUTDIR)/run_arrays.o $(OUTDIR)/arrays.o $(OUTDIR)/arrays_ad.o
	$(FC) $^ -o $@

run_call_example: $(OUTDIR)/run_call_example.o $(OUTDIR)/call_example.o $(OUTDIR)/call_example_ad.o
	$(FC) $^ -o $@

run_control_flow: $(OUTDIR)/run_control_flow.o $(OUTDIR)/control_flow.o $(OUTDIR)/control_flow_ad.o
	$(FC) $^ -o $@

run_cross_mod: $(OUTDIR)/run_cross_mod.o $(OUTDIR)/cross_mod_a.o $(OUTDIR)/cross_mod_a_ad.o                $(OUTDIR)/cross_mod_b.o $(OUTDIR)/cross_mod_b_ad.o
	$(FC) $^ -o $@

run_data_storage: $(OUTDIR)/run_data_storage.o $(OUTDIR)/data_storage.o
	$(FC) $^ -o $@

run_intrinsic_func: $(OUTDIR)/run_intrinsic_func.o $(OUTDIR)/intrinsic_func.o $(OUTDIR)/intrinsic_func_ad.o
	$(FC) $^ -o $@

run_real_kind: $(OUTDIR)/run_real_kind.o $(OUTDIR)/real_kind.o $(OUTDIR)/real_kind_ad.o
	$(FC) $^ -o $@

run_save_vars: $(OUTDIR)/run_save_vars.o $(OUTDIR)/save_vars.o $(OUTDIR)/save_vars_ad.o
	$(FC) $^ -o $@

run_store_vars: $(OUTDIR)/run_store_vars.o $(OUTDIR)/store_vars.o $(OUTDIR)/store_vars_ad.o $(OUTDIR)/data_storage.o
	$(FC) $^ -o $@

run_directives: $(OUTDIR)/run_directives.o $(OUTDIR)/directives.o $(OUTDIR)/directives_ad.o
	$(FC) $^ -o $@

run_parameter_var: $(OUTDIR)/run_parameter_var.o $(OUTDIR)/parameter_var.o $(OUTDIR)/parameter_var_ad.o
	$(FC) $^ -o $@

run_module_vars: $(OUTDIR)/run_module_vars.o $(OUTDIR)/module_vars.o $(OUTDIR)/module_vars_ad.o
	$(FC) $^ -o $@

run_allocate: $(OUTDIR)/run_allocate.o $(OUTDIR)/allocate.o $(OUTDIR)/allocate_ad.o
	$(FC) $^ -o $@


clean:
	rm -f $(OUTDIR)/*.o $(OUTDIR)/*.mod
