import itertools

TEMPLATE_PUSH_SCALAR = """
  subroutine push_{tname}_0d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_{tname}_t), intent(inout) :: self
    {ftype}, intent(in) :: data
    if (.not. allocated(self%ary(self%page_num)%data)) then
      allocate(self%ary(self%page_num)%data(self%page_size))
    end if
    self%ary(self%page_num)%data(self%pos) = data
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      self%pos = 1
      self%page_num = self%page_num + 1
      if (self%page_num > self%max_page_num) then
        print *, 'Page number exceeds the limit'
        error stop 1
      end if
    end if
    return
  end subroutine push_{tname}_0d
"""

TEMPLATE_PUSH_ARRAY = """
  subroutine push_{tname}_{rank}d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_{tname}_t), intent(inout) :: self
    {ftype}, intent(in), target :: data{dims}
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    {ftype}, pointer :: ptr(:)
    len = size(data)
    call c_f_pointer(c_loc(data), ptr, [len])
    i0 = 1
    do while (len > 0)
      i1 = i0 + min(len - 1, self%page_size - self%pos)
      j0 = self%pos
      j1 = self%pos + int(i1 - i0)
      if (.not. allocated(self%ary(self%page_num)%data)) then
        allocate(self%ary(self%page_num)%data(self%page_size))
      end if
      self%ary(self%page_num)%data(j0:j1) = ptr(i0:i1)
      len = len - (i1 - i0 + 1)
      self%pos = j1 + 1
      i0 = i1 + 1
      if (self%pos > self%page_size) then
        self%pos = 1
        self%page_num = self%page_num + 1
        if (self%page_num > self%max_page_num) then
          print *, 'Page number exceeds the limit'
          error stop 1
        end if
      end if
    end do
    return
  end subroutine push_{tname}_{rank}d
"""

TEMPLATE_POP_SCALAR = """
  subroutine pop_{tname}_0d(self, data)
    implicit none
    class(fautodiff_stack_{tname}_t), intent(inout) :: self
    {ftype}, intent(out) :: data
    self%pos = self%pos - 1
    if (self%pos < 1) then
        self%page_num = self%page_num - 1
        if (self%page_num < 1) then
            print *, 'No stacked data'
            error stop 1
        end if
        self%pos = self%page_size
    end if
    data = self%ary(self%page_num)%data(self%pos)
    return
  end subroutine pop_{tname}_0d
"""

TEMPLATE_POP_ARRAY = """
  subroutine pop_{tname}_{rank}d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_{tname}_t), intent(inout) :: self
        {ftype}, intent(out), target :: data{dims}
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    {ftype}, pointer :: ptr(:)
    len = size(data)
    call c_f_pointer(c_loc(data), ptr, [len])
    i1 = len
    do while (len > 0)
      if (self%pos == 1) then
        self%page_num = self%page_num - 1
        if (self%page_num < 1) then
          print *, 'No stacked data'
          error stop 1
        end if
        self%pos = self%page_size + 1
      end if
      i0 = i1 - min(len, self%pos - 1) + 1
      j0 = self%pos - int(i1 - i0) - 1
      j1 = self%pos - 1
      ptr(i0:i1) = self%ary(self%page_num)%data(j0:j1)
      len = len - (i1 - i0 + 1)
      self%pos = j0
      i1 = i0 - 1
    end do
    return
  end subroutine pop_{tname}_{rank}d
"""

TEMPLATE_GET_SCALAR = """
  function get_{tname}(self) result(res)
    class(fautodiff_stack_{tname}_t), intent(inout) :: self
    {ftype} :: res
    call self%pop(res)
  end function get_{tname}
"""

TEMPLATE_PUSH_PTR = """
  subroutine push_p_{tname}_{rank}d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    {ftype}, intent(in), target :: data{dims}
    {set_dims}
    self%ary(self%pos) = c_loc(data)
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      print *, 'Data size exceeds the limit'
      error stop 1
    end if
    return
  end subroutine push_p_{tname}_{rank}d
"""

TEMPLATE_POP_PTR = """
  subroutine pop_p_{tname}_{rank}d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    {ftype}, intent(out), pointer :: data{dims}
    type(c_ptr) :: ptr
    self%pos = self%pos - 1
    if (self%pos < 1) then
       print *, 'No stacked data'
       error stop 1
    end if
    call c_f_pointer(self%ary(self%pos), data, {shape})
    return
  end subroutine pop_p_{tname}_{rank}d
"""

types = ["r4", "r8", "i", "l"]
ranks = [0, 1, 2, 3]
tmap = {
    "r4": "real",
    "r8": "real(8)",
    "i": "integer",
    "l": "logical",
}
max_page_num = {
    "r4": "MAX_PAGE_NUM_LARGE",
    "r8": "MAX_PAGE_NUM_LARGE",
    "i": "MAX_PAGE_NUM_SMALL",
    "l": "MAX_PAGE_NUM_SMALL",
}
shape_expr = {
    1: "[self%dims(1, self%pos)]",
    2: "[self%dims(1, self%pos), self%dims(2, self%pos)]",
    3: "[self%dims(1, self%pos), self%dims(2, self%pos), self%dims(3, self%pos)]",
}


def gen_push_subroutine(tname, rank):
    ftype = tmap[tname]
    dims = "(" + ", ".join([":" for _ in range(rank)]) + ")"
    if rank == 0:
        return TEMPLATE_PUSH_SCALAR.format(tname=tname, ftype=ftype)
    else:
        return TEMPLATE_PUSH_ARRAY.format(
            tname=tname, ftype=ftype, rank=rank, dims=dims
        )


def gen_push_ptr_subroutine(tname, rank):
    ftype = tmap[tname]
    dims = "(" + ", ".join([":" for _ in range(rank)]) + ")"
    set_dims = []
    for i in range(rank):
        set_dims.append(f"    self%dims({i+1}, self%pos) = size(data, {i+1})")
    set_dims = "\n".join(set_dims)
    return TEMPLATE_PUSH_PTR.format(
        tname=tname, ftype=ftype, rank=rank, dims=dims, set_dims=set_dims
    )


def gen_pop_subroutine(tname, rank):
    ftype = tmap[tname]
    dims = "(" + ", ".join([":" for _ in range(rank)]) + ")"
    if rank == 0:
        return TEMPLATE_POP_SCALAR.format(tname=tname, ftype=ftype)
    else:
        return TEMPLATE_POP_ARRAY.format(tname=tname, ftype=ftype, rank=rank, dims=dims)


def gen_pop_ptr_subroutine(tname, rank):
    ftype = tmap[tname]
    dims = "(" + ", ".join([":" for _ in range(rank)]) + ")"
    return TEMPLATE_POP_PTR.format(
        tname=tname, ftype=ftype, rank=rank, dims=dims, shape=shape_expr[rank]
    )


def gen_get_function(tname):
    ftype = tmap[tname]
    return TEMPLATE_GET_SCALAR.format(tname=tname, ftype=ftype)


def gen_wrapper_push(tname):
    ftype = tmap[tname]
    lines = [
        f"  subroutine fautodiff_stack_push_{tname}(data)",
        f"    {ftype}, intent(in), target :: data(..)",
        f"    select rank(data)",
    ]
    for r in ranks:
        lines.append(f"    rank({r})")
        # The rank of DATA is only known inside each SELECT RANK branch, so
        # dispatch directly to the corresponding specific procedure and pass
        # the stack instance explicitly. Using type-bound generics here would
        # require compile-time rank knowledge.
        lines.append(f"      call push_{tname}_{r}d(fautodiff_stack_{tname}, data)")
    lines.extend(
        [
            "    rank default",
            "      print *, 'Rank larger than 3 is not supported'",
            "      error stop 1",
            "    end select",
            f"  end subroutine fautodiff_stack_push_{tname}",
            "",
        ]
    )
    return "\n".join(lines)


def gen_wrapper_pop(tname):
    ftype = tmap[tname]
    lines = [
        f"  subroutine fautodiff_stack_pop_{tname}(data)",
        f"    {ftype}, intent(out), target :: data(..)",
        f"    select rank(data)",
    ]
    for r in ranks:
        lines.append(f"    rank({r})")
        # See GEN_WRAPPER_PUSH for an explanation of the explicit procedure
        # calls with the stack instance passed as the first argument.
        lines.append(f"      call pop_{tname}_{r}d(fautodiff_stack_{tname}, data)")
    lines.extend(
        [
            "    rank default",
            "      print *, 'Rank larger than 3 is not supported'",
            "      error stop 1",
            "    end select",
            f"  end subroutine fautodiff_stack_pop_{tname}",
            "",
        ]
    )
    return "\n".join(lines)


def gen_module():
    lines = [
        "module fautodiff_stack",
        "  use iso_c_binding",
        "  implicit none",
        "  private",
        "",
    ]
    for t in types:
        lines.append(f"  public :: fautodiff_stack_{t}_t")
    lines.append(f"  public :: fautodiff_stack_p_t")
    for t in types:
        lines.append(f"  public :: fautodiff_stack_{t}")
    lines.append(f"  public :: fautodiff_stack_p")
    lines.append(f"  public :: fautodiff_stack_push_r")
    lines.append(f"  public :: fautodiff_stack_pop_r")
    lines.extend(
        [
            "",
            "  integer, parameter :: DEFAULT_PAGE_SIZE = 1024 * 1024",
            "  integer, parameter :: MAX_PAGE_NUM_LARGE = 1024 * 1024",
            "  integer, parameter :: MAX_PAGE_NUM_SMALL = 1",
            "",
        ]
    )
    for t in types:
        lines.extend(
            [
                f"  type :: data_{t}_t",
                f"    {tmap[t]}, allocatable :: data(:)",
                f"  end type data_{t}_t",
                "",
            ]
        )
    for t in types:
        lines.extend(
            [
                f"  type :: fautodiff_stack_{t}_t",
                f"    integer :: max_page_num = {max_page_num[t]}",
                f"    type(data_{t}_t) :: ary({max_page_num[t]})",
                "    integer :: page_num = 1",
                "    integer :: pos = 1",
                "    integer :: page_size = DEFAULT_PAGE_SIZE",
                "  contains",
            ]
        )
        push = []
        for r in ranks:
            if t == "p" and r == 0:
                continue
            push.append(f"push_{t}_{r}d")
            lines.append(f"    procedure :: push_{t}_{r}d")
        pop = []
        for r in ranks:
            pop.append(f"pop_{t}_{r}d")
            lines.append(f"    procedure :: pop_{t}_{r}d")
        lines.append(f"    generic :: push => {', '.join(push)}")
        lines.append(f"    generic :: pop => {', '.join(pop)}")
        lines.append(f"    procedure :: get => get_{t}")
        lines.append(f"  end type fautodiff_stack_{t}_t\n")
    lines.extend(
        [
            f"  type :: fautodiff_stack_p_t",
            "    type(c_ptr) :: ary(DEFAULT_PAGE_SIZE)",
            f"    integer :: dims({max(ranks)}, DEFAULT_PAGE_SIZE)",
            "    integer :: pos = 1",
            "    integer :: page_size = DEFAULT_PAGE_SIZE",
            "  contains",
        ]
    )
    push = []
    pop = []
    for t in ("r4", "r8"):
        for r in ranks:
            if r == 0:
                continue
            push.append(f"push_p_{t}_{r}d")
            lines.append(f"    procedure :: push_p_{t}_{r}d")
        for r in ranks:
            if r == 0:
                continue
            pop.append(f"pop_p_{t}_{r}d")
            lines.append(f"    procedure :: pop_p_{t}_{r}d")
    lines.append(f"    generic :: push => {', '.join(push)}")
    lines.append(f"    generic :: pop => {', '.join(pop)}")
    lines.append(f"  end type fautodiff_stack_p_t")
    lines.append("")
    for t in types:
        lines.append(f"  type(fautodiff_stack_{t}_t), save :: fautodiff_stack_{t}")
    lines.append(f"  type(fautodiff_stack_p_t), save :: fautodiff_stack_p")
    lines.append("")
    push_mods = []
    pop_mods = []
    for t in ("r4", "r8"):
        push_mods.append(f"fautodiff_stack_push_{t}")
        pop_mods.append(f"fautodiff_stack_pop_{t}")
    lines.append("  interface fautodiff_stack_push_r")
    lines.append("    module procedure " + ", ".join(push_mods))
    lines.append("  end interface")
    lines.append("  interface fautodiff_stack_pop_r")
    lines.append("    module procedure " + ", ".join(pop_mods))
    lines.append("  end interface")
    lines.append("")
    lines.append("contains")
    for t, r in itertools.product(types, ranks):
        lines.append(gen_push_subroutine(t, r))
        lines.append(gen_pop_subroutine(t, r))
    for t in types:
        lines.append(gen_get_function(t))
    for t in ("r4", "r8"):
        for r in ranks:
            if r != 0:
                lines.append(gen_push_ptr_subroutine(t, r))
                lines.append(gen_pop_ptr_subroutine(t, r))
    for t in ("r4", "r8"):
        lines.append(gen_wrapper_push(t))
        lines.append(gen_wrapper_pop(t))
    lines.append("end module fautodiff_stack")
    return "\n".join(lines)


if __name__ == "__main__":
    print(gen_module())
