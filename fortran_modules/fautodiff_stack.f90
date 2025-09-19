module fautodiff_stack
  use iso_c_binding
  implicit none
  private

  public :: fautodiff_stack_r4_t
  public :: fautodiff_stack_r8_t
  public :: fautodiff_stack_i_t
  public :: fautodiff_stack_l_t
  public :: fautodiff_stack_p_t
  public :: fautodiff_stack_r4
  public :: fautodiff_stack_r8
  public :: fautodiff_stack_i
  public :: fautodiff_stack_l
  public :: fautodiff_stack_p
  public :: fautodiff_stack_push_r
  public :: fautodiff_stack_pop_r

  integer, parameter :: DEFAULT_PAGE_SIZE = 1024 * 1024
  integer, parameter :: MAX_PAGE_NUM_LARGE = 1024 * 1024
  integer, parameter :: MAX_PAGE_NUM_SMALL = 1

  type :: data_r4_t
    real, allocatable :: data(:)
  end type data_r4_t

  type :: data_r8_t
    real(8), allocatable :: data(:)
  end type data_r8_t

  type :: data_i_t
    integer, allocatable :: data(:)
  end type data_i_t

  type :: data_l_t
    logical, allocatable :: data(:)
  end type data_l_t

  type :: fautodiff_stack_r4_t
    integer :: max_page_num = MAX_PAGE_NUM_LARGE
    type(data_r4_t) :: ary(MAX_PAGE_NUM_LARGE)
    integer :: page_num = 1
    integer :: pos = 1
    integer :: page_size = DEFAULT_PAGE_SIZE
  contains
    procedure :: push_r4_0d
    procedure :: push_r4_1d
    procedure :: push_r4_2d
    procedure :: push_r4_3d
    procedure :: pop_r4_0d
    procedure :: pop_r4_1d
    procedure :: pop_r4_2d
    procedure :: pop_r4_3d
    generic :: push => push_r4_0d, push_r4_1d, push_r4_2d, push_r4_3d
    generic :: pop => pop_r4_0d, pop_r4_1d, pop_r4_2d, pop_r4_3d
    procedure :: get => get_r4
  end type fautodiff_stack_r4_t

  type :: fautodiff_stack_r8_t
    integer :: max_page_num = MAX_PAGE_NUM_LARGE
    type(data_r8_t) :: ary(MAX_PAGE_NUM_LARGE)
    integer :: page_num = 1
    integer :: pos = 1
    integer :: page_size = DEFAULT_PAGE_SIZE
  contains
    procedure :: push_r8_0d
    procedure :: push_r8_1d
    procedure :: push_r8_2d
    procedure :: push_r8_3d
    procedure :: pop_r8_0d
    procedure :: pop_r8_1d
    procedure :: pop_r8_2d
    procedure :: pop_r8_3d
    generic :: push => push_r8_0d, push_r8_1d, push_r8_2d, push_r8_3d
    generic :: pop => pop_r8_0d, pop_r8_1d, pop_r8_2d, pop_r8_3d
    procedure :: get => get_r8
  end type fautodiff_stack_r8_t

  type :: fautodiff_stack_i_t
    integer :: max_page_num = MAX_PAGE_NUM_SMALL
    type(data_i_t) :: ary(MAX_PAGE_NUM_SMALL)
    integer :: page_num = 1
    integer :: pos = 1
    integer :: page_size = DEFAULT_PAGE_SIZE
  contains
    procedure :: push_i_0d
    procedure :: push_i_1d
    procedure :: push_i_2d
    procedure :: push_i_3d
    procedure :: pop_i_0d
    procedure :: pop_i_1d
    procedure :: pop_i_2d
    procedure :: pop_i_3d
    generic :: push => push_i_0d, push_i_1d, push_i_2d, push_i_3d
    generic :: pop => pop_i_0d, pop_i_1d, pop_i_2d, pop_i_3d
    procedure :: get => get_i
  end type fautodiff_stack_i_t

  type :: fautodiff_stack_l_t
    integer :: max_page_num = MAX_PAGE_NUM_SMALL
    type(data_l_t) :: ary(MAX_PAGE_NUM_SMALL)
    integer :: page_num = 1
    integer :: pos = 1
    integer :: page_size = DEFAULT_PAGE_SIZE
  contains
    procedure :: push_l_0d
    procedure :: push_l_1d
    procedure :: push_l_2d
    procedure :: push_l_3d
    procedure :: pop_l_0d
    procedure :: pop_l_1d
    procedure :: pop_l_2d
    procedure :: pop_l_3d
    generic :: push => push_l_0d, push_l_1d, push_l_2d, push_l_3d
    generic :: pop => pop_l_0d, pop_l_1d, pop_l_2d, pop_l_3d
    procedure :: get => get_l
  end type fautodiff_stack_l_t

  type :: fautodiff_stack_p_t
    type(c_ptr) :: ary(DEFAULT_PAGE_SIZE)
    integer :: dims(3, DEFAULT_PAGE_SIZE)
    integer :: pos = 1
    integer :: page_size = DEFAULT_PAGE_SIZE
  contains
    procedure :: push_p_r4_1d
    procedure :: push_p_r4_2d
    procedure :: push_p_r4_3d
    procedure :: pop_p_r4_1d
    procedure :: pop_p_r4_2d
    procedure :: pop_p_r4_3d
    procedure :: push_p_r8_1d
    procedure :: push_p_r8_2d
    procedure :: push_p_r8_3d
    procedure :: pop_p_r8_1d
    procedure :: pop_p_r8_2d
    procedure :: pop_p_r8_3d
    generic :: push => push_p_r4_1d, push_p_r4_2d, push_p_r4_3d, push_p_r8_1d, push_p_r8_2d, push_p_r8_3d
    generic :: pop => pop_p_r4_1d, pop_p_r4_2d, pop_p_r4_3d, pop_p_r8_1d, pop_p_r8_2d, pop_p_r8_3d
  end type fautodiff_stack_p_t

  type(fautodiff_stack_r4_t), save :: fautodiff_stack_r4
  type(fautodiff_stack_r8_t), save :: fautodiff_stack_r8
  type(fautodiff_stack_i_t), save :: fautodiff_stack_i
  type(fautodiff_stack_l_t), save :: fautodiff_stack_l
  type(fautodiff_stack_p_t), save :: fautodiff_stack_p

  interface fautodiff_stack_push_r
    module procedure fautodiff_stack_push_r4_0d, fautodiff_stack_push_r4_1d, fautodiff_stack_push_r4_2d, fautodiff_stack_push_r4_3d
    module procedure fautodiff_stack_push_r8_0d, fautodiff_stack_push_r8_1d, fautodiff_stack_push_r8_2d, fautodiff_stack_push_r8_3d
  end interface
  interface fautodiff_stack_pop_r
    module procedure fautodiff_stack_pop_r4_0d, fautodiff_stack_pop_r4_1d, fautodiff_stack_pop_r4_2d, fautodiff_stack_pop_r4_3d
    module procedure fautodiff_stack_pop_r8_0d, fautodiff_stack_pop_r8_1d, fautodiff_stack_pop_r8_2d, fautodiff_stack_pop_r8_3d
  end interface

contains

  subroutine push_r4_0d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
    real, intent(in) :: data
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
  end subroutine push_r4_0d


  subroutine pop_r4_0d(self, data)
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
    real, intent(out) :: data
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
  end subroutine pop_r4_0d


  subroutine push_r4_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
    real, intent(in), target :: data(:)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real, pointer :: ptr(:)
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
  end subroutine push_r4_1d


  subroutine pop_r4_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
        real, intent(out), target :: data(:)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real, pointer :: ptr(:)
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
  end subroutine pop_r4_1d


  subroutine push_r4_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
    real, intent(in), target :: data(:, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real, pointer :: ptr(:)
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
  end subroutine push_r4_2d


  subroutine pop_r4_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
        real, intent(out), target :: data(:, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real, pointer :: ptr(:)
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
  end subroutine pop_r4_2d


  subroutine push_r4_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
    real, intent(in), target :: data(:, :, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real, pointer :: ptr(:)
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
  end subroutine push_r4_3d


  subroutine pop_r4_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r4_t), intent(inout) :: self
        real, intent(out), target :: data(:, :, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real, pointer :: ptr(:)
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
  end subroutine pop_r4_3d


  subroutine push_r8_0d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
    real(8), intent(in) :: data
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
  end subroutine push_r8_0d


  subroutine pop_r8_0d(self, data)
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
    real(8), intent(out) :: data
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
  end subroutine pop_r8_0d


  subroutine push_r8_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
    real(8), intent(in), target :: data(:)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real(8), pointer :: ptr(:)
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
  end subroutine push_r8_1d


  subroutine pop_r8_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
        real(8), intent(out), target :: data(:)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real(8), pointer :: ptr(:)
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
  end subroutine pop_r8_1d


  subroutine push_r8_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
    real(8), intent(in), target :: data(:, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real(8), pointer :: ptr(:)
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
  end subroutine push_r8_2d


  subroutine pop_r8_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
        real(8), intent(out), target :: data(:, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real(8), pointer :: ptr(:)
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
  end subroutine pop_r8_2d


  subroutine push_r8_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
    real(8), intent(in), target :: data(:, :, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real(8), pointer :: ptr(:)
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
  end subroutine push_r8_3d


  subroutine pop_r8_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_r8_t), intent(inout) :: self
        real(8), intent(out), target :: data(:, :, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    real(8), pointer :: ptr(:)
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
  end subroutine pop_r8_3d


  subroutine push_i_0d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
    integer, intent(in) :: data
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
  end subroutine push_i_0d


  subroutine pop_i_0d(self, data)
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
    integer, intent(out) :: data
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
  end subroutine pop_i_0d


  subroutine push_i_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
    integer, intent(in), target :: data(:)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer, pointer :: ptr(:)
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
  end subroutine push_i_1d


  subroutine pop_i_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
        integer, intent(out), target :: data(:)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer, pointer :: ptr(:)
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
  end subroutine pop_i_1d


  subroutine push_i_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
    integer, intent(in), target :: data(:, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer, pointer :: ptr(:)
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
  end subroutine push_i_2d


  subroutine pop_i_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
        integer, intent(out), target :: data(:, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer, pointer :: ptr(:)
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
  end subroutine pop_i_2d


  subroutine push_i_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
    integer, intent(in), target :: data(:, :, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer, pointer :: ptr(:)
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
  end subroutine push_i_3d


  subroutine pop_i_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_i_t), intent(inout) :: self
        integer, intent(out), target :: data(:, :, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    integer, pointer :: ptr(:)
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
  end subroutine pop_i_3d


  subroutine push_l_0d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
    logical, intent(in) :: data
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
  end subroutine push_l_0d


  subroutine pop_l_0d(self, data)
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
    logical, intent(out) :: data
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
  end subroutine pop_l_0d


  subroutine push_l_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
    logical, intent(in), target :: data(:)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    logical, pointer :: ptr(:)
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
  end subroutine push_l_1d


  subroutine pop_l_1d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
        logical, intent(out), target :: data(:)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    logical, pointer :: ptr(:)
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
  end subroutine pop_l_1d


  subroutine push_l_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
    logical, intent(in), target :: data(:, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    logical, pointer :: ptr(:)
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
  end subroutine push_l_2d


  subroutine pop_l_2d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
        logical, intent(out), target :: data(:, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    logical, pointer :: ptr(:)
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
  end subroutine pop_l_2d


  subroutine push_l_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
    logical, intent(in), target :: data(:, :, :)
       integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    logical, pointer :: ptr(:)
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
  end subroutine push_l_3d


  subroutine pop_l_3d(self, data)
    use iso_c_binding
    implicit none
    class(fautodiff_stack_l_t), intent(inout) :: self
        logical, intent(out), target :: data(:, :, :)
    integer(8) :: len
    integer(8) :: i0, i1
    integer :: j0, j1
    logical, pointer :: ptr(:)
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
  end subroutine pop_l_3d


  function get_r4(self) result(res)
    class(fautodiff_stack_r4_t), intent(inout) :: self
    real :: res
    call self%pop(res)
  end function get_r4


  function get_r8(self) result(res)
    class(fautodiff_stack_r8_t), intent(inout) :: self
    real(8) :: res
    call self%pop(res)
  end function get_r8


  function get_i(self) result(res)
    class(fautodiff_stack_i_t), intent(inout) :: self
    integer :: res
    call self%pop(res)
  end function get_i


  function get_l(self) result(res)
    class(fautodiff_stack_l_t), intent(inout) :: self
    logical :: res
    call self%pop(res)
  end function get_l


  subroutine push_p_r4_1d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real, intent(in), target :: data(:)
        self%dims(1, self%pos) = size(data, 1)
    self%ary(self%pos) = c_loc(data)
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      print *, 'Data size exceeds the limit'
      error stop 1
    end if
    return
  end subroutine push_p_r4_1d


  subroutine pop_p_r4_1d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real, intent(out), pointer :: data(:)
    type(c_ptr) :: ptr
    self%pos = self%pos - 1
    if (self%pos < 1) then
       print *, 'No stacked data'
       error stop 1
    end if
    call c_f_pointer(self%ary(self%pos), data, [self%dims(1, self%pos)])
    return
  end subroutine pop_p_r4_1d


  subroutine push_p_r4_2d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real, intent(in), target :: data(:, :)
        self%dims(1, self%pos) = size(data, 1)
    self%dims(2, self%pos) = size(data, 2)
    self%ary(self%pos) = c_loc(data)
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      print *, 'Data size exceeds the limit'
      error stop 1
    end if
    return
  end subroutine push_p_r4_2d


  subroutine pop_p_r4_2d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real, intent(out), pointer :: data(:, :)
    type(c_ptr) :: ptr
    self%pos = self%pos - 1
    if (self%pos < 1) then
       print *, 'No stacked data'
       error stop 1
    end if
    call c_f_pointer(self%ary(self%pos), data, [self%dims(1, self%pos), self%dims(2, self%pos)])
    return
  end subroutine pop_p_r4_2d


  subroutine push_p_r4_3d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real, intent(in), target :: data(:, :, :)
        self%dims(1, self%pos) = size(data, 1)
    self%dims(2, self%pos) = size(data, 2)
    self%dims(3, self%pos) = size(data, 3)
    self%ary(self%pos) = c_loc(data)
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      print *, 'Data size exceeds the limit'
      error stop 1
    end if
    return
  end subroutine push_p_r4_3d


  subroutine pop_p_r4_3d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real, intent(out), pointer :: data(:, :, :)
    type(c_ptr) :: ptr
    self%pos = self%pos - 1
    if (self%pos < 1) then
       print *, 'No stacked data'
       error stop 1
    end if
    call c_f_pointer(self%ary(self%pos), data, [self%dims(1, self%pos), self%dims(2, self%pos), self%dims(3, self%pos)])
    return
  end subroutine pop_p_r4_3d


  subroutine push_p_r8_1d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real(8), intent(in), target :: data(:)
        self%dims(1, self%pos) = size(data, 1)
    self%ary(self%pos) = c_loc(data)
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      print *, 'Data size exceeds the limit'
      error stop 1
    end if
    return
  end subroutine push_p_r8_1d


  subroutine pop_p_r8_1d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real(8), intent(out), pointer :: data(:)
    type(c_ptr) :: ptr
    self%pos = self%pos - 1
    if (self%pos < 1) then
       print *, 'No stacked data'
       error stop 1
    end if
    call c_f_pointer(self%ary(self%pos), data, [self%dims(1, self%pos)])
    return
  end subroutine pop_p_r8_1d


  subroutine push_p_r8_2d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real(8), intent(in), target :: data(:, :)
        self%dims(1, self%pos) = size(data, 1)
    self%dims(2, self%pos) = size(data, 2)
    self%ary(self%pos) = c_loc(data)
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      print *, 'Data size exceeds the limit'
      error stop 1
    end if
    return
  end subroutine push_p_r8_2d


  subroutine pop_p_r8_2d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real(8), intent(out), pointer :: data(:, :)
    type(c_ptr) :: ptr
    self%pos = self%pos - 1
    if (self%pos < 1) then
       print *, 'No stacked data'
       error stop 1
    end if
    call c_f_pointer(self%ary(self%pos), data, [self%dims(1, self%pos), self%dims(2, self%pos)])
    return
  end subroutine pop_p_r8_2d


  subroutine push_p_r8_3d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real(8), intent(in), target :: data(:, :, :)
        self%dims(1, self%pos) = size(data, 1)
    self%dims(2, self%pos) = size(data, 2)
    self%dims(3, self%pos) = size(data, 3)
    self%ary(self%pos) = c_loc(data)
    self%pos = self%pos + 1
    if (self%pos > self%page_size) then
      print *, 'Data size exceeds the limit'
      error stop 1
    end if
    return
  end subroutine push_p_r8_3d


  subroutine pop_p_r8_3d(self, data)
    use iso_c_binding
    class(fautodiff_stack_p_t), intent(inout) :: self
    real(8), intent(out), pointer :: data(:, :, :)
    type(c_ptr) :: ptr
    self%pos = self%pos - 1
    if (self%pos < 1) then
       print *, 'No stacked data'
       error stop 1
    end if
    call c_f_pointer(self%ary(self%pos), data, [self%dims(1, self%pos), self%dims(2, self%pos), self%dims(3, self%pos)])
    return
  end subroutine pop_p_r8_3d

  subroutine fautodiff_stack_push_r4_0d(data)
    real, intent(in) :: data
    call push_r4_0d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_push_r4_0d

  subroutine fautodiff_stack_pop_r4_0d(data)
    real, intent(out) :: data
    call pop_r4_0d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_pop_r4_0d

  subroutine fautodiff_stack_push_r4_1d(data)
    real, intent(in), target :: data(:)
    call push_r4_1d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_push_r4_1d

  subroutine fautodiff_stack_pop_r4_1d(data)
    real, intent(out), target :: data(:)
    call pop_r4_1d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_pop_r4_1d

  subroutine fautodiff_stack_push_r4_2d(data)
    real, intent(in), target :: data(:, :)
    call push_r4_2d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_push_r4_2d

  subroutine fautodiff_stack_pop_r4_2d(data)
    real, intent(out), target :: data(:, :)
    call pop_r4_2d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_pop_r4_2d

  subroutine fautodiff_stack_push_r4_3d(data)
    real, intent(in), target :: data(:, :, :)
    call push_r4_3d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_push_r4_3d

  subroutine fautodiff_stack_pop_r4_3d(data)
    real, intent(out), target :: data(:, :, :)
    call pop_r4_3d(fautodiff_stack_r4, data)
  end subroutine fautodiff_stack_pop_r4_3d

  subroutine fautodiff_stack_push_r8_0d(data)
    real(8), intent(in) :: data
    call push_r8_0d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_push_r8_0d

  subroutine fautodiff_stack_pop_r8_0d(data)
    real(8), intent(out) :: data
    call pop_r8_0d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_pop_r8_0d

  subroutine fautodiff_stack_push_r8_1d(data)
    real(8), intent(in), target :: data(:)
    call push_r8_1d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_push_r8_1d

  subroutine fautodiff_stack_pop_r8_1d(data)
    real(8), intent(out), target :: data(:)
    call pop_r8_1d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_pop_r8_1d

  subroutine fautodiff_stack_push_r8_2d(data)
    real(8), intent(in), target :: data(:, :)
    call push_r8_2d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_push_r8_2d

  subroutine fautodiff_stack_pop_r8_2d(data)
    real(8), intent(out), target :: data(:, :)
    call pop_r8_2d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_pop_r8_2d

  subroutine fautodiff_stack_push_r8_3d(data)
    real(8), intent(in), target :: data(:, :, :)
    call push_r8_3d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_push_r8_3d

  subroutine fautodiff_stack_pop_r8_3d(data)
    real(8), intent(out), target :: data(:, :, :)
    call pop_r8_3d(fautodiff_stack_r8, data)
  end subroutine fautodiff_stack_pop_r8_3d

end module fautodiff_stack
