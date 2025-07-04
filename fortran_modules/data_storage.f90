module fautodiff_data_storage
  implicit none
  private

  public fautodiff_data_storage_push
  public fautodiff_data_storage_pop

  interface fautodiff_data_storage_push
     module procedure push_ary_r4
     module procedure push_scalar_r4
  end interface fautodiff_data_storage_push

  interface fautodiff_data_storage_pop
     module procedure pop_ary_r4
     module procedure pop_scalar_r4
  end interface fautodiff_data_storage_pop


  type :: ptr_r4_t
     real, pointer, contiguous :: ptr(:)
  end type ptr_r4_t

  type :: page_t
     integer :: page_num = 1
     integer :: pos = 1
  end type page_t

  integer, parameter :: PAGE_SIZE = 1024 * 1024
  integer, parameter :: MAX_PAGE_NUM = 1024 * 1024

  type(ptr_r4_t) :: ary_r4(MAX_PAGE_NUM)
  type(page_t) :: page_r4

contains

  subroutine push_ary_r4(ary)
    real, intent(in), contiguous :: ary(:)
    real, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)

    i0 = 1

    do while (len > 0)

       if (.not. associated(ary_r4(page_r4%page_num)%ptr)) then
          allocate(ary_r4(page_r4%page_num)%ptr(PAGE_SIZE))
       end if
       ptr => ary_r4(page_r4%page_num)%ptr

       i1 = i0 + min(len - 1, PAGE_SIZE - page_r4%pos)
       ptr(page_r4%pos:page_r4%pos + i1 - i0) = ary(i0:i1)
       len = len - (i1 - i0 + 1)
       page_r4%pos = page_r4%pos + i1 - i0 + 1
       i0 = i1 + 1

       if (page_r4%pos > PAGE_SIZE) then
          page_r4%pos = 1
          page_r4%page_num = page_r4%page_num + 1
          if (page_r4%page_num > MAX_PAGE_NUM) then
             print *, "Page number exceeds the limit"
             error stop 1
          end if
       end if

    end do

    return
  end subroutine push_ary_r4

  subroutine push_scalar_r4(scalar)
    real, intent(in) :: scalar
    real :: buf(1)
    buf(1) = scalar

    call push_ary_r4(buf)

    return
  end subroutine push_scalar_r4

  subroutine pop_ary_r4(ary)
    real, intent(out), contiguous :: ary(:)
    real, pointer, contiguous :: ptr(:)
    integer(8) :: len
    integer :: i0, i1

    len = size(ary)

    i1 = len

    do while (len > 0)

       ptr => ary_r4(page_r4%page_num)%ptr

       i0 = i1 - min(len, page_r4%pos - 1) + 1
       ary(i0:i1) = ptr(page_r4%pos - (i1 - i0 + 1):page_r4%pos - 1)
       len = len - (i1 - i0 + 1)
       page_r4%pos = page_r4%pos - (i1 - i0 + 1)
       i1 = i0 - 1

       if (page_r4%pos < 1) then
          page_r4%pos = PAGE_SIZE
          page_r4%page_num = page_r4%page_num - 1
          if (page_r4%page_num < 1) then
             print *, "Unexpected error occured"
             error stop 1
          end if
       end if

    end do

    return
  end subroutine pop_ary_r4

  subroutine pop_scalar_r4(scalar)
    real, intent(out) :: scalar
    real :: buf(1)

    call pop_ary_r4(buf)
    scalar = buf(1)

    return
  end subroutine pop_scalar_r4

end module fautodiff_data_storage
