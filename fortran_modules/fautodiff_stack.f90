module fautodiff_stack
  implicit none
  private

  public :: stack_r4_t, stack_r8_t, stack_l_t, stack_i_t
  public :: fautodiff_stack_push, fautodiff_stack_pop, fautodiff_stack_get

  integer, parameter :: DEFAULT_PAGE_SIZE = 1024 * 1024
  integer, parameter :: MAX_PAGE_NUM = 1024 * 1024

  type :: ptr_r4_t
     real, pointer, contiguous :: ptr(:)
  end type ptr_r4_t

  type :: ptr_r8_t
     real(8), pointer, contiguous :: ptr(:)
  end type ptr_r8_t

  type :: ptr_l_t
     logical, pointer, contiguous :: ptr(:)
  end type ptr_l_t

  type :: ptr_i_t
     integer, pointer, contiguous :: ptr(:)
  end type ptr_i_t

  type :: page_t
     integer :: page_num = 1
     integer :: pos = 1
  end type page_t

  ! stack types
  type, public :: stack_r4_t
     type(ptr_r4_t) :: ary(MAX_PAGE_NUM)
     type(page_t)   :: page
     integer :: page_size = DEFAULT_PAGE_SIZE
  contains
     procedure :: push => push_r4
     procedure :: pop  => pop_r4
     procedure :: size => size_r4
   end type stack_r4_t

  type, public :: stack_r8_t
     type(ptr_r8_t) :: ary(MAX_PAGE_NUM)
     type(page_t)   :: page
     integer :: page_size = DEFAULT_PAGE_SIZE
  contains
     procedure :: push => push_r8
     procedure :: pop  => pop_r8
     procedure :: size => size_r8
   end type stack_r8_t

  type, public :: stack_l_t
     type(ptr_l_t) :: ary(MAX_PAGE_NUM)
     type(page_t)  :: page
     integer :: page_size = DEFAULT_PAGE_SIZE
  contains
     procedure :: push => push_l
     procedure :: pop  => pop_l
     procedure :: get => get_l
     procedure :: size => size_l
   end type stack_l_t

  type, public :: stack_i_t
     type(ptr_i_t) :: ary(MAX_PAGE_NUM)
     type(page_t)  :: page
     integer :: page_size = DEFAULT_PAGE_SIZE
  contains
     procedure :: push => push_i
     procedure :: pop  => pop_i
     procedure :: size => size_i
   end type stack_i_t

  ! default stacks used by wrapper procedures
  type(stack_r4_t), save, public :: fautodiff_stack_r4
  type(stack_r8_t), save, public :: fautodiff_stack_r8
  type(stack_l_t),  save, public :: fautodiff_stack_l
  type(stack_i_t),  save, public :: fautodiff_stack_i

  interface fautodiff_stack_push
     module procedure push_r4_wrapper
     module procedure push_r8_wrapper
     module procedure push_l_wrapper
     module procedure push_i_wrapper
  end interface fautodiff_stack_push

  interface fautodiff_stack_pop
     module procedure pop_r4_wrapper
     module procedure pop_r8_wrapper
     module procedure pop_l_wrapper
     module procedure pop_i_wrapper
  end interface fautodiff_stack_pop

  interface fautodiff_stack_get
     module procedure get_l_wrapper
  end interface fautodiff_stack_get

contains

  !====================== helper ========================================
  integer(8) function data_size(page, page_size)
    type(page_t), intent(in) :: page
    integer, intent(in) :: page_size
    data_size = (page%page_num - 1) * page_size + page%pos - 1
  end function data_size

  !====================== stack_r4 methods ===============================
  subroutine push_r4(self, data)
    class(stack_r4_t), intent(inout) :: self
    class(*), dimension(..), intent(in) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (real)
        real :: buf(1)
        buf(1) = d
        call push_r4_array(self, buf)
      class default
        print *, 'Type mismatch in push_r4'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (real)
        call push_r4_array(self, d)
      class default
        print *, 'Type mismatch in push_r4'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in push_r4'
      error stop 1
    end select
  end subroutine push_r4

  subroutine push_r4_array(self, ary)
    class(stack_r4_t), intent(inout) :: self
    real, intent(in), contiguous :: ary(:)
    real, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i0 = 1
    do while (len > 0)
       if (.not. associated(self%ary(self%page%page_num)%ptr)) then
          allocate(self%ary(self%page%page_num)%ptr(self%page_size))
       end if
       ptr => self%ary(self%page%page_num)%ptr
       i1 = i0 + min(len - 1, self%page_size - self%page%pos)
       ptr(self%page%pos:self%page%pos + i1 - i0) = ary(i0:i1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos + i1 - i0 + 1
       i0 = i1 + 1
       if (self%page%pos > self%page_size) then
         self%page%pos = 1
         self%page%page_num = self%page%page_num + 1
         if (self%page%page_num > MAX_PAGE_NUM) then
            print *, 'Page number exceeds the limit'
            error stop 1
         end if
      end if
    end do
  end subroutine push_r4_array

  subroutine pop_r4(self, data)
    class(stack_r4_t), intent(inout) :: self
    class(*), dimension(..), intent(out) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (real)
        real :: buf(1)
        call pop_r4_array(self, buf)
        d = buf(1)
      class default
        print *, 'Type mismatch in pop_r4'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (real)
        call pop_r4_array(self, d)
      class default
        print *, 'Type mismatch in pop_r4'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in pop_r4'
      error stop 1
    end select
  end subroutine pop_r4

  subroutine pop_r4_array(self, ary)
    class(stack_r4_t), intent(inout) :: self
    real, intent(out), contiguous :: ary(:)
    real, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i1 = len
    if (len > data_size(self%page, self%page_size)) then
       print *, 'Stored data is not enough: ', len, data_size(self%page, self%page_size)
       error stop 1
    end if
    do while (len > 0)
       ptr => self%ary(self%page%page_num)%ptr
       i0 = i1 - min(len, self%page%pos - 1) + 1
       ary(i0:i1) = ptr(self%page%pos - (i1 - i0 + 1):self%page%pos - 1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos - (i1 - i0 + 1)
       i1 = i0 - 1
       if (self%page%pos < 1) then
          self%page%pos = self%page_size
          self%page%page_num = self%page%page_num - 1
          if (self%page%page_num < 1) then
             print *, 'Unexpected error occured'
             error stop 1
          end if
       end if
    end do
  end subroutine pop_r4_array

  integer(8) function size_r4(self)
    class(stack_r4_t), intent(in) :: self
    size_r4 = data_size(self%page, self%page_size)
  end function size_r4

  !====================== stack_r8 methods ===============================
  subroutine push_r8(self, data)
    class(stack_r8_t), intent(inout) :: self
    class(*), dimension(..), intent(in) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (real(8))
        real(8) :: buf(1)
        buf(1) = d
        call push_r8_array(self, buf)
      class default
        print *, 'Type mismatch in push_r8'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (real(8))
        call push_r8_array(self, d)
      class default
        print *, 'Type mismatch in push_r8'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in push_r8'
      error stop 1
    end select
  end subroutine push_r8

  subroutine push_r8_array(self, ary)
    class(stack_r8_t), intent(inout) :: self
    real(8), intent(in), contiguous :: ary(:)
    real(8), pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i0 = 1
    do while (len > 0)
       if (.not. associated(self%ary(self%page%page_num)%ptr)) then
          allocate(self%ary(self%page%page_num)%ptr(self%page_size))
       end if
       ptr => self%ary(self%page%page_num)%ptr
       i1 = i0 + min(len - 1, self%page_size - self%page%pos)
       ptr(self%page%pos:self%page%pos + i1 - i0) = ary(i0:i1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos + i1 - i0 + 1
       i0 = i1 + 1
       if (self%page%pos > self%page_size) then
         self%page%pos = 1
          self%page%page_num = self%page%page_num + 1
          if (self%page%page_num > MAX_PAGE_NUM) then
             print *, 'Page number exceeds the limit'
             error stop 1
          end if
       end if
    end do
  end subroutine push_r8_array

  subroutine pop_r8(self, data)
    class(stack_r8_t), intent(inout) :: self
    class(*), dimension(..), intent(out) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (real(8))
        real(8) :: buf(1)
        call pop_r8_array(self, buf)
        d = buf(1)
      class default
        print *, 'Type mismatch in pop_r8'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (real(8))
        call pop_r8_array(self, d)
      class default
        print *, 'Type mismatch in pop_r8'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in pop_r8'
      error stop 1
    end select
  end subroutine pop_r8

  subroutine pop_r8_array(self, ary)
    class(stack_r8_t), intent(inout) :: self
    real(8), intent(out), contiguous :: ary(:)
    real(8), pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i1 = len
    if (len > data_size(self%page, self%page_size)) then
       print *, 'Stored data is not enough: ', len, data_size(self%page, self%page_size)
       error stop 1
    end if
    do while (len > 0)
       ptr => self%ary(self%page%page_num)%ptr
       i0 = i1 - min(len, self%page%pos - 1) + 1
       ary(i0:i1) = ptr(self%page%pos - (i1 - i0 + 1):self%page%pos - 1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos - (i1 - i0 + 1)
       i1 = i0 - 1
       if (self%page%pos < 1) then
          self%page%pos = self%page_size
          self%page%page_num = self%page%page_num - 1
          if (self%page%page_num < 1) then
             print *, 'Unexpected error occured'
             error stop 1
          end if
       end if
    end do
  end subroutine pop_r8_array

  integer(8) function size_r8(self)
    class(stack_r8_t), intent(in) :: self
    size_r8 = data_size(self%page, self%page_size)
  end function size_r8

  !====================== stack_l methods ===============================
  subroutine push_l(self, data)
    class(stack_l_t), intent(inout) :: self
    class(*), dimension(..), intent(in) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (logical)
        logical :: buf(1)
        buf(1) = d
        call push_l_array(self, buf)
      class default
        print *, 'Type mismatch in push_l'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (logical)
        call push_l_array(self, d)
      class default
        print *, 'Type mismatch in push_l'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in push_l'
      error stop 1
    end select
  end subroutine push_l

  subroutine push_l_array(self, ary)
    class(stack_l_t), intent(inout) :: self
    logical, intent(in), contiguous :: ary(:)
    logical, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i0 = 1
    do while (len > 0)
       if (.not. associated(self%ary(self%page%page_num)%ptr)) then
          allocate(self%ary(self%page%page_num)%ptr(self%page_size))
       end if
       ptr => self%ary(self%page%page_num)%ptr
       i1 = i0 + min(len - 1, self%page_size - self%page%pos)
       ptr(self%page%pos:self%page%pos + i1 - i0) = ary(i0:i1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos + i1 - i0 + 1
       i0 = i1 + 1
       if (self%page%pos > self%page_size) then
         self%page%pos = 1
          self%page%page_num = self%page%page_num + 1
          if (self%page%page_num > MAX_PAGE_NUM) then
             print *, 'Page number exceeds the limit'
             error stop 1
          end if
       end if
    end do
  end subroutine push_l_array

  subroutine pop_l(self, data)
    class(stack_l_t), intent(inout) :: self
    class(*), dimension(..), intent(out) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (logical)
        logical :: buf(1)
        call pop_l_array(self, buf)
        d = buf(1)
      class default
        print *, 'Type mismatch in pop_l'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (logical)
        call pop_l_array(self, d)
      class default
        print *, 'Type mismatch in pop_l'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in pop_l'
      error stop 1
    end select
  end subroutine pop_l

  subroutine pop_l_array(self, ary)
    class(stack_l_t), intent(inout) :: self
    logical, intent(out), contiguous :: ary(:)
    logical, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i1 = len
    if (len > data_size(self%page, self%page_size)) then
       print *, 'Stored data is not enough: ', len, data_size(self%page, self%page_size)
       error stop 1
    end if
    do while (len > 0)
       ptr => self%ary(self%page%page_num)%ptr
       i0 = i1 - min(len, self%page%pos - 1) + 1
       ary(i0:i1) = ptr(self%page%pos - (i1 - i0 + 1):self%page%pos - 1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos - (i1 - i0 + 1)
       i1 = i0 - 1
       if (self%page%pos < 1) then
          self%page%pos = self%page_size
          self%page%page_num = self%page%page_num - 1
          if (self%page%page_num < 1) then
             print *, 'Unexpected error occured'
             error stop 1
          end if
       end if
    end do
  end subroutine pop_l_array

  logical function get_l(self) result(val)
    class(stack_l_t), intent(inout) :: self
    logical :: buf(1)
    call pop_l_array(self, buf)
    val = buf(1)
  end function get_l

  integer(8) function size_l(self)
    class(stack_l_t), intent(in) :: self
    size_l = data_size(self%page, self%page_size)
  end function size_l

  !====================== stack_i methods ===============================
  subroutine push_i(self, data)
    class(stack_i_t), intent(inout) :: self
    class(*), dimension(..), intent(in) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (integer)
        integer :: buf(1)
        buf(1) = d
        call push_i_array(self, buf)
      class default
        print *, 'Type mismatch in push_i'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (integer)
        call push_i_array(self, d)
      class default
        print *, 'Type mismatch in push_i'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in push_i'
      error stop 1
    end select
  end subroutine push_i

  subroutine push_i_array(self, ary)
    class(stack_i_t), intent(inout) :: self
    integer, intent(in), contiguous :: ary(:)
    integer, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i0 = 1
    do while (len > 0)
       if (.not. associated(self%ary(self%page%page_num)%ptr)) then
          allocate(self%ary(self%page%page_num)%ptr(self%page_size))
       end if
       ptr => self%ary(self%page%page_num)%ptr
       i1 = i0 + min(len - 1, self%page_size - self%page%pos)
       ptr(self%page%pos:self%page%pos + i1 - i0) = ary(i0:i1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos + i1 - i0 + 1
       i0 = i1 + 1
       if (self%page%pos > self%page_size) then
         self%page%pos = 1
          self%page%page_num = self%page%page_num + 1
          if (self%page%page_num > MAX_PAGE_NUM) then
             print *, 'Page number exceeds the limit'
             error stop 1
          end if
       end if
    end do
  end subroutine push_i_array

  subroutine pop_i(self, data)
    class(stack_i_t), intent(inout) :: self
    class(*), dimension(..), intent(out) :: data
    select rank(data)
    rank(0)
      select type(d => data)
      type is (integer)
        integer :: buf(1)
        call pop_i_array(self, buf)
        d = buf(1)
      class default
        print *, 'Type mismatch in pop_i'
        error stop 1
      end select
    rank(1)
      select type(d => data)
      type is (integer)
        call pop_i_array(self, d)
      class default
        print *, 'Type mismatch in pop_i'
        error stop 1
      end select
    rank default
      print *, 'Unsupported rank in pop_i'
      error stop 1
    end select
  end subroutine pop_i

  subroutine pop_i_array(self, ary)
    class(stack_i_t), intent(inout) :: self
    integer, intent(out), contiguous :: ary(:)
    integer, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)
    i1 = len
    if (len > data_size(self%page, self%page_size)) then
       print *, 'Stored data is not enough: ', len, data_size(self%page, self%page_size)
       error stop 1
    end if
    do while (len > 0)
       if (.not. associated(self%ary(self%page%page_num)%ptr)) then
          print *, 'Accessing empty memory'
          error stop 1
       end if
       ptr => self%ary(self%page%page_num)%ptr
       i0 = i1 - min(len, self%page%pos - 1) + 1
       ary(i0:i1) = ptr(self%page%pos - (i1 - i0 + 1):self%page%pos - 1)
       len = len - (i1 - i0 + 1)
       self%page%pos = self%page%pos - (i1 - i0 + 1)
       i1 = i0 - 1
       if (self%page%pos < 1) then
          self%page%pos = self%page_size
          self%page%page_num = self%page%page_num - 1
          if (self%page%page_num < 1) then
             print *, 'Page number is below one'
             error stop 1
          end if
       end if
    end do
  end subroutine pop_i_array

  integer(8) function size_i(self)
    class(stack_i_t), intent(in) :: self
    size_i = data_size(self%page, self%page_size)
  end function size_i

  !====================== wrapper procedures ============================
  subroutine push_r4_wrapper(data)
    real, intent(in) :: data
    call fautodiff_stack_r4%push(data)
  end subroutine push_r4_wrapper

  subroutine push_r4_wrapper_array(ary)
    real, intent(in), contiguous :: ary(:)
    call fautodiff_stack_r4%push(ary)
  end subroutine push_r4_wrapper_array

  subroutine push_r8_wrapper(data)
    real(8), intent(in) :: data
    call fautodiff_stack_r8%push(data)
  end subroutine push_r8_wrapper

  subroutine push_r8_wrapper_array(ary)
    real(8), intent(in), contiguous :: ary(:)
    call fautodiff_stack_r8%push(ary)
  end subroutine push_r8_wrapper_array

  subroutine push_l_wrapper(data)
    logical, intent(in) :: data
    call fautodiff_stack_l%push(data)
  end subroutine push_l_wrapper

  subroutine push_l_wrapper_array(ary)
    logical, intent(in), contiguous :: ary(:)
    call fautodiff_stack_l%push(ary)
  end subroutine push_l_wrapper_array

  subroutine push_i_wrapper(data)
    integer, intent(in) :: data
    call fautodiff_stack_i%push(data)
  end subroutine push_i_wrapper

  subroutine push_i_wrapper_array(ary)
    integer, intent(in), contiguous :: ary(:)
    call fautodiff_stack_i%push(ary)
  end subroutine push_i_wrapper_array

  interface push_r4_wrapper
     module procedure push_r4_wrapper
     module procedure push_r4_wrapper_array
  end interface

  interface push_r8_wrapper
     module procedure push_r8_wrapper
     module procedure push_r8_wrapper_array
  end interface

  interface push_l_wrapper
     module procedure push_l_wrapper
     module procedure push_l_wrapper_array
  end interface

  interface push_i_wrapper
     module procedure push_i_wrapper
     module procedure push_i_wrapper_array
  end interface

  subroutine pop_r4_wrapper(data)
    real, intent(out) :: data
    call fautodiff_stack_r4%pop(data)
  end subroutine pop_r4_wrapper

  subroutine pop_r4_wrapper_array(ary)
    real, intent(out), contiguous :: ary(:)
    call fautodiff_stack_r4%pop(ary)
  end subroutine pop_r4_wrapper_array

  subroutine pop_r8_wrapper(data)
    real(8), intent(out) :: data
    call fautodiff_stack_r8%pop(data)
  end subroutine pop_r8_wrapper

  subroutine pop_r8_wrapper_array(ary)
    real(8), intent(out), contiguous :: ary(:)
    call fautodiff_stack_r8%pop(ary)
  end subroutine pop_r8_wrapper_array

  subroutine pop_l_wrapper(data)
    logical, intent(out) :: data
    call fautodiff_stack_l%pop(data)
  end subroutine pop_l_wrapper

  subroutine pop_l_wrapper_array(ary)
    logical, intent(out), contiguous :: ary(:)
    call fautodiff_stack_l%pop(ary)
  end subroutine pop_l_wrapper_array

  subroutine pop_i_wrapper(data)
    integer, intent(out) :: data
    call fautodiff_stack_i%pop(data)
  end subroutine pop_i_wrapper

  subroutine pop_i_wrapper_array(ary)
    integer, intent(out), contiguous :: ary(:)
    call fautodiff_stack_i%pop(ary)
  end subroutine pop_i_wrapper_array

  interface pop_r4_wrapper
     module procedure pop_r4_wrapper
     module procedure pop_r4_wrapper_array
  end interface

  interface pop_r8_wrapper
     module procedure pop_r8_wrapper
     module procedure pop_r8_wrapper_array
  end interface

  interface pop_l_wrapper
     module procedure pop_l_wrapper
     module procedure pop_l_wrapper_array
  end interface

  interface pop_i_wrapper
     module procedure pop_i_wrapper
     module procedure pop_i_wrapper_array
  end interface

  function get_l_wrapper() result(val)
    logical :: val
    val = fautodiff_stack_l%get()
  end function get_l_wrapper

end module fautodiff_stack
