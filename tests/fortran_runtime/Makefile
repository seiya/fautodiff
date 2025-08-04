OUTDIR ?= $(shell pwd)

FC = gfortran
MPIFC ?= mpifort
FFLAGS = -O2 -ffree-line-length-none \
         -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check -finit-real=nan -g -fbacktrace -fopenmp

HELPER_DIR = ../../fortran_modules
MOD_DIR = ../../examples
MOD_SRCS = $(wildcard $(MOD_DIR)/*.f90)
MOD_OBJS = $(addprefix $(OUTDIR)/,$(notdir $(MOD_SRCS:.f90=.o)))

vpath %.f90 $(HELPER_DIR)
vpath %.f90 $(MOD_DIR)
vpath %.f90 .

PROGRAM_NAMES = run_simple_math.out run_arrays.out run_call_example.out run_control_flow.out run_cross_mod.out \
           run_intrinsic_func.out run_real_kind.out run_save_vars.out run_store_vars.out \
           run_directives.out run_parameter_var.out run_module_vars.out run_call_module_vars.out run_allocate_vars.out \
           run_exit_cycle.out run_pointer_arrays.out run_derived_alloc.out run_mpi_example.out \
                   run_stack.out run_where_forall.out run_omp_loops.out
PROGRAMS = $(addprefix $(OUTDIR)/,$(PROGRAM_NAMES))

.PHONY: all clean

all: $(PROGRAMS)


$(OUTDIR)/run_%.out: $(OUTDIR)/run_%.o
	$(FC) $(FFLAGS) -o $@ $^

$(OUTDIR)/run_mpi_example.out: $(OUTDIR)/run_mpi_example.o
	$(MPIFC) $(FFLAGS) -o $@ $^

$(OUTDIR)/run_%.o: run_%.f90
	$(FC) $(FFLAGS) -c $< -J $(OUTDIR) -o $@

$(OUTDIR)/run_mpi_example.o: run_mpi_example.f90
	$(MPIFC) $(FFLAGS) -c $< -J $(OUTDIR) -o $@

$(OUTDIR)/%.o: %.f90
	$(MAKE) -C $(MOD_DIR) OUTDIR=$(OUTDIR) MPIFC=$(MPIFC) $@

# AD modules depend on their original modules
$(OUTDIR)/%_ad.o: $(OUTDIR)/%.mod

# Special case: module 'array' is defined in arrays.f90
$(OUTDIR)/arrays_ad.o: $(OUTDIR)/arrays.mod

# driver module dependencies
$(OUTDIR)/run_simple_math.o: $(OUTDIR)/simple_math.o $(OUTDIR)/simple_math_ad.o
$(OUTDIR)/run_arrays.o: $(OUTDIR)/arrays.o $(OUTDIR)/arrays_ad.o
$(OUTDIR)/run_call_example.o: $(OUTDIR)/call_example.o $(OUTDIR)/call_example_ad.o
$(OUTDIR)/run_control_flow.o: $(OUTDIR)/control_flow.o $(OUTDIR)/control_flow_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_cross_mod.o: $(OUTDIR)/cross_mod_a.o $(OUTDIR)/cross_mod_a_ad.o $(OUTDIR)/cross_mod_b.o $(OUTDIR)/cross_mod_b_ad.o
$(OUTDIR)/run_intrinsic_func.o: $(OUTDIR)/intrinsic_func.o $(OUTDIR)/intrinsic_func_ad.o
$(OUTDIR)/run_real_kind.o: $(OUTDIR)/real_kind.o $(OUTDIR)/real_kind_ad.o
$(OUTDIR)/run_save_vars.o: $(OUTDIR)/save_vars.o $(OUTDIR)/save_vars_ad.o
$(OUTDIR)/run_store_vars.o: $(OUTDIR)/store_vars.o $(OUTDIR)/store_vars_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_directives.o: $(OUTDIR)/directives.o $(OUTDIR)/directives_ad.o
$(OUTDIR)/run_parameter_var.o: $(OUTDIR)/parameter_var.o $(OUTDIR)/parameter_var_ad.o
$(OUTDIR)/run_module_vars.o: $(OUTDIR)/module_vars.o $(OUTDIR)/module_vars_ad.o
$(OUTDIR)/run_call_module_vars.o: $(OUTDIR)/call_module_vars.o $(OUTDIR)/call_module_vars_ad.o $(OUTDIR)/module_vars.o $(OUTDIR)/module_vars_ad.o
$(OUTDIR)/run_allocate_vars.o: $(OUTDIR)/allocate_vars.o $(OUTDIR)/allocate_vars_ad.o
$(OUTDIR)/run_exit_cycle.o: $(OUTDIR)/exit_cycle.o $(OUTDIR)/exit_cycle_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_pointer_arrays.o: $(OUTDIR)/pointer_arrays.o $(OUTDIR)/pointer_arrays_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_derived_alloc.o: $(OUTDIR)/derived_alloc.o $(OUTDIR)/derived_alloc_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_mpi_example.o: $(OUTDIR)/mpi_example.o $(OUTDIR)/mpi_example_ad.o $(OUTDIR)/mpi_ad.o
$(OUTDIR)/run_where_forall.o: $(OUTDIR)/where_forall.o $(OUTDIR)/where_forall_ad.o

$(OUTDIR)/run_omp_loops.o: $(OUTDIR)/omp_loops.o $(OUTDIR)/omp_loops_ad.o

$(OUTDIR)/run_fautodiff_stack.o: $(OUTDIR)/fautodiff_stack.o


$(OUTDIR)/run_simple_math.out: $(OUTDIR)/run_simple_math.o $(OUTDIR)/simple_math.o $(OUTDIR)/simple_math_ad.o
$(OUTDIR)/run_arrays.out: $(OUTDIR)/run_arrays.o $(OUTDIR)/arrays.o $(OUTDIR)/arrays_ad.o
$(OUTDIR)/run_call_example.out: $(OUTDIR)/run_call_example.o $(OUTDIR)/call_example.o $(OUTDIR)/call_example_ad.o
$(OUTDIR)/run_control_flow.out: $(OUTDIR)/run_control_flow.o $(OUTDIR)/control_flow.o $(OUTDIR)/control_flow_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_cross_mod.out: $(OUTDIR)/run_cross_mod.o $(OUTDIR)/cross_mod_a.o $(OUTDIR)/cross_mod_a_ad.o $(OUTDIR)/cross_mod_b.o $(OUTDIR)/cross_mod_b_ad.o
$(OUTDIR)/run_intrinsic_func.out: $(OUTDIR)/run_intrinsic_func.o $(OUTDIR)/intrinsic_func.o $(OUTDIR)/intrinsic_func_ad.o
$(OUTDIR)/run_real_kind.out: $(OUTDIR)/run_real_kind.o $(OUTDIR)/real_kind.o $(OUTDIR)/real_kind_ad.o
$(OUTDIR)/run_save_vars.out: $(OUTDIR)/run_save_vars.o $(OUTDIR)/save_vars.o $(OUTDIR)/save_vars_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_store_vars.out: $(OUTDIR)/run_store_vars.o $(OUTDIR)/store_vars.o $(OUTDIR)/store_vars_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_directives.out: $(OUTDIR)/run_directives.o $(OUTDIR)/directives.o $(OUTDIR)/directives_ad.o
$(OUTDIR)/run_parameter_var.out: $(OUTDIR)/run_parameter_var.o $(OUTDIR)/parameter_var.o $(OUTDIR)/parameter_var_ad.o
$(OUTDIR)/run_module_vars.out: $(OUTDIR)/run_module_vars.o $(OUTDIR)/module_vars.o $(OUTDIR)/module_vars_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_call_module_vars.out: $(OUTDIR)/run_call_module_vars.o $(OUTDIR)/call_module_vars.o $(OUTDIR)/call_module_vars_ad.o $(OUTDIR)/module_vars.o $(OUTDIR)/module_vars_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_allocate_vars.out: $(OUTDIR)/run_allocate_vars.o $(OUTDIR)/allocate_vars.o $(OUTDIR)/allocate_vars_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_exit_cycle.out: $(OUTDIR)/run_exit_cycle.o $(OUTDIR)/exit_cycle.o $(OUTDIR)/exit_cycle_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_pointer_arrays.out: $(OUTDIR)/run_pointer_arrays.o $(OUTDIR)/pointer_arrays.o $(OUTDIR)/pointer_arrays_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_derived_alloc.out: $(OUTDIR)/run_derived_alloc.o $(OUTDIR)/derived_alloc.o $(OUTDIR)/derived_alloc_ad.o $(OUTDIR)/fautodiff_stack.o
$(OUTDIR)/run_mpi_example.out: $(OUTDIR)/run_mpi_example.o $(OUTDIR)/mpi_example.o $(OUTDIR)/mpi_example_ad.o $(OUTDIR)/mpi_ad.o
$(OUTDIR)/run_where_forall.out: $(OUTDIR)/run_where_forall.o $(OUTDIR)/where_forall.o $(OUTDIR)/where_forall_ad.o

$(OUTDIR)/run_omp_loops.out: $(OUTDIR)/run_omp_loops.o $(OUTDIR)/omp_loops.o $(OUTDIR)/omp_loops_ad.o

$(OUTDIR)/run_fautodiff_stack.out: $(OUTDIR)/run_fautodiff_stack.o $(OUTDIR)/fautodiff_stack.o

clean:
	rm -f $(OUTDIR)/*.o $(OUTDIR)/*.mod
