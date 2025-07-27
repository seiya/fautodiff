module fautodiff_stack
  implicit none
  private

  public :: fautodiff_stack_t
  public :: fautodiff_stack_r4_t, fautodiff_stack_r8_t, fautodiff_stack_i_t, fautodiff_stack_l_t
  public :: fautodiff_stack_r4, fautodiff_stack_r8, fautodiff_stack_i, fautodiff_stack_l

  integer, parameter :: DEFAULT_PAGE_SIZE = 1024 * 1024
  integer, parameter :: MAX_PAGE_NUM = 1024 * 1024

  integer, parameter :: i_type_r4 = 1
  integer, parameter :: i_type_r8 = 2
  integer, parameter :: i_type_i  = 3
  integer, parameter :: i_type_l  = 4

  type :: ptr_r4_t
    real, pointer, contiguous :: ptr(:) => null()
  end type ptr_r4_t

  type :: ptr_r8_t
    real(8), pointer, contiguous :: ptr(:) => null()
  end type ptr_r8_t

  type :: ptr_i_t
    integer, pointer, contiguous :: ptr(:) => null()
  end type ptr_i_t

  type :: ptr_l_t
    logical, pointer, contiguous :: ptr(:) => null()
  end type ptr_l_t

  type, abstract :: fautodiff_stack_t
    integer :: page_num = 1
    integer :: pos = 1
    integer :: page_size = DEFAULT_PAGE_SIZE
  contains
    procedure :: push
    procedure :: pop
    procedure :: size => data_size
  end type fautodiff_stack_t

  ! stack types
  type, extends(fautodiff_stack_t) :: fautodiff_stack_r4_t
    type(ptr_r4_t) :: ary(MAX_PAGE_NUM)
  end type fautodiff_stack_r4_t

  type, extends(fautodiff_stack_t) :: fautodiff_stack_r8_t
    type(ptr_r8_t) :: ary(MAX_PAGE_NUM)
  end type fautodiff_stack_r8_t

  type, extends(fautodiff_stack_t) :: fautodiff_stack_i_t
    type(ptr_i_t) :: ary(MAX_PAGE_NUM)
  contains
    procedure :: get => get_i
  end type fautodiff_stack_i_t

  type, extends(fautodiff_stack_t) :: fautodiff_stack_l_t
    type(ptr_l_t) :: ary(MAX_PAGE_NUM)
  contains
    procedure :: get => get_l
  end type fautodiff_stack_l_t

  ! default stacks used by wrapper procedures
  type(fautodiff_stack_r4_t), save :: fautodiff_stack_r4
  type(fautodiff_stack_r8_t), save :: fautodiff_stack_r8
  type(fautodiff_stack_i_t),  save :: fautodiff_stack_i
  type(fautodiff_stack_l_t),  save :: fautodiff_stack_l

contains

  !====================== type-bounded proedures ======================
  integer(8) function data_size(self)
    class(fautodiff_stack_t), intent(in) :: self
    data_size = (self%page_num - 1) * self%page_size + self%pos - 1
  end function data_size

  subroutine push(self, data)
    use iso_c_binding
    class(fautodiff_stack_t), intent(inout) :: self
    class(*), dimension(..), intent(in), target :: data
    type(c_ptr) :: ptr
    real,    pointer, contiguous :: ptr_r4(:)
    real(8), pointer, contiguous :: ptr_r8(:)
    integer, pointer, contiguous :: ptr_i(:)
    logical, pointer, contiguous :: ptr_l(:)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer :: i_type

    call get_ptr(self, data, i_type, len, ptr_r4, ptr_r8, ptr_i, ptr_l)

    i0 = 1
    do while (len > 0)
      i1 = i0 + min(len - 1, self%page_size - self%pos)
      j0 = self%pos
      j1 = self%pos + int(i1 - i0)
      select type (self)
      type is (fautodiff_stack_r4_t)
        if (.not. associated(self%ary(self%page_num)%ptr)) then
          allocate(self%ary(self%page_num)%ptr(self%page_size))
        end if
        self%ary(self%page_num)%ptr(j0:j1) = ptr_r4(i0:i1)
      type is (fautodiff_stack_r8_t)
        if (.not. associated(self%ary(self%page_num)%ptr)) then
          allocate(self%ary(self%page_num)%ptr(self%page_size))
        end if
        self%ary(self%page_num)%ptr(j0:j1) = ptr_r8(i0:i1)
      type is (fautodiff_stack_i_t)
        if (.not. associated(self%ary(self%page_num)%ptr)) then
          allocate(self%ary(self%page_num)%ptr(self%page_size))
        end if
        self%ary(self%page_num)%ptr(j0:j1) = ptr_i(i0:i1)
      type is (fautodiff_stack_l_t)
        if (.not. associated(self%ary(self%page_num)%ptr)) then
          allocate(self%ary(self%page_num)%ptr(self%page_size))
        end if
        self%ary(self%page_num)%ptr(j0:j1) = ptr_l(i0:i1)
      end select
      len = len - (i1 - i0 + 1)
      self%pos = j1 + 1
      i0 = i1 + 1
      if (self%pos > self%page_size) then
        self%pos = 1
        self%page_num = self%page_num + 1
        if (self%page_num > MAX_PAGE_NUM) then
          print *, 'Page number exceeds the limit'
          error stop 1
        end if
      end if
    end do
  end subroutine push

  subroutine pop(self, data)
    use iso_c_binding
    class(fautodiff_stack_t), intent(inout) :: self
    class(*), dimension(..), intent(inout), target :: data
    type(c_ptr) :: ptr
    real,    pointer, contiguous :: ptr_r4(:)
    real(8), pointer, contiguous :: ptr_r8(:)
    integer, pointer, contiguous :: ptr_i(:)
    logical, pointer, contiguous :: ptr_l(:)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer :: i_type

    call get_ptr(self, data, i_type, len, ptr_r4, ptr_r8, ptr_i, ptr_l)

    i1 = len
    if (len > self%size()) then
       print *, 'Stored data is not enough: ', len, self%size()
       error stop 1
    end if
    do while (len > 0)
      i0 = i1 - min(len, self%pos - 1) + 1
      j0 = self%pos - int(i1 - i0) - 1
      j1 = self%pos - 1
      select type(self)
      type is (fautodiff_stack_r4_t)
        ptr_r4(i0:i1) = self%ary(self%page_num)%ptr(j0:j1)
      type is (fautodiff_stack_r8_t)
        ptr_r8(i0:i1) = self%ary(self%page_num)%ptr(j0:j1)
      type is (fautodiff_stack_i_t)
        ptr_i(i0:i1) = self%ary(self%page_num)%ptr(j0:j1)
      type is (fautodiff_stack_l_t)
        ptr_l(i0:i1) = self%ary(self%page_num)%ptr(j0:j1)
      end select
      len = len - (i1 - i0 + 1)
      self%pos = j0
      i1 = i0 - 1
      if (self%pos < 1) then
        self%pos = self%page_size
        self%page_num = self%page_num - 1
        if (self%page_num < 1) then
          print *, 'Unexpected error occured'
          error stop 1
        end if
      end if
    end do
  end subroutine pop

  function get_i(self) result(res)
    class(fautodiff_stack_i_t), intent(inout) :: self
    integer :: res

    call pop(self, res)
  end function

  function get_l(self) result(res)
    class(fautodiff_stack_l_t), intent(inout) :: self
    logical :: res

    call pop(self, res)
  end function

  !====================== helper routines =============================
  subroutine get_ptr(self, data, i_type, len, ptr_r4, ptr_r8, ptr_i, ptr_l)
    use iso_c_binding
    class(fautodiff_stack_t), intent(in) :: self
    class(*), dimension(..), intent(in), target :: data
    integer, intent(out) :: i_type
    integer(8) , intent(out) :: len
    real,    intent(out), pointer :: ptr_r4(:)
    real(8), intent(out), pointer :: ptr_r8(:)
    integer, intent(out), pointer :: ptr_i(:)
    logical, intent(out), pointer :: ptr_l(:)
    type(c_ptr) :: ptr

    i_type = 0
    select rank(data)
    rank (0)
      len = 1
      select type(data)
      type is (real)
        ptr = c_loc(data)
        i_type = i_type_r4
      type is (real(8))
        ptr = c_loc(data)
        i_type = i_type_r8
      type is (integer)
        ptr = c_loc(data)
        i_type = i_type_i
      type is (logical)
        ptr = c_loc(data)
        i_type = i_type_l
      end select
    rank (1)
      select type(data)
      type is (real)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_r4
      type is (real(8))
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_r8
      type is (integer)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_i
      type is (logical)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_l
      end select
    rank (2)
      select type(data)
      type is (real)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_r4
      type is (real(8))
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_r8
      type is (integer)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_i
      type is (logical)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_l
      end select
    rank (3)
      select type(data)
      type is (real)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_r4
      type is (real(8))
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_r8
      type is (integer)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_i
      type is (logical)
        ptr = c_loc(data)
        len = size(data)
        i_type = i_type_l
      end select
    end select

    select case (i_type)
    case (i_type_r4)
      if (.not. extends_type_of(self, fautodiff_stack_r4)) then
        print *, 'data must be real'
        error stop 1
      end if
      call c_f_pointer(ptr, ptr_r4, [len])
    case (i_type_r8)
      if (.not. extends_type_of(self, fautodiff_stack_r8)) then
        print *, 'data must be real(8)'
        error stop 1
      end if
      call c_f_pointer(ptr, ptr_r8, [len])
    case (i_type_i)
      if (.not. extends_type_of(self, fautodiff_stack_i)) then
        print *, 'data must be integer'
        error stop 1
      end if
      call c_f_pointer(ptr, ptr_i, [len])
    case (i_type_l)
      if (.not. extends_type_of(self, fautodiff_stack_l)) then
        print *, 'data must be logical'
        error stop 1
      end if
      call c_f_pointer(ptr, ptr_l, [len])
    case default
      print *, 'Unsupported data type'
      error stop 1
    end select

    return
  end subroutine get_ptr

end module fautodiff_stack
