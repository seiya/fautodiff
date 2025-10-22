module return_example_ad
  implicit none


contains

  subroutine conditional_return(x, y)
    real, intent(in)  :: x
    real, intent(out) :: y

    if (x < 0.0) then
      y = - x
      return
    end if
    y = x * x

    return
  end subroutine conditional_return

  subroutine alloc_return(n, x, y, f)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(out) :: y(n)
    logical, intent(in)  :: f
    real, allocatable :: xtmp(:)

    allocate(xtmp(n))
    if (f) then
      xtmp = x**2
      y = xtmp
      return
    end if
    xtmp = x + 1.0
    y = xtmp * x

    return
  end subroutine alloc_return

  subroutine conditional_return_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad

    if (x < 0.0) then
      y_ad = - x_ad ! y = -x
      y = - x
      return
    end if
    y_ad = x_ad * (x + x) ! y = x * x
    y = x * x

    return
  end subroutine conditional_return_fwd_ad

  subroutine conditional_return_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad
    logical :: return_flag_10_ad

    return_flag_10_ad = .true.
    if (x < 0.0) then
      return_flag_10_ad = .false.
    end if

    if (return_flag_10_ad) then
      x_ad = y_ad * (x + x) + x_ad ! y = x * x
      y_ad = 0.0 ! y = x * x
    end if
    if (x < 0.0) then
      x_ad = - y_ad + x_ad ! y = -x
      y_ad = 0.0 ! y = -x
    end if

    return
  end subroutine conditional_return_rev_ad

  subroutine alloc_return_fwd_ad(n, x, x_ad, y, y_ad, f)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: x_ad(n)
    real, intent(out) :: y(n)
    real, intent(out) :: y_ad(n)
    logical, intent(in)  :: f
    real, allocatable :: xtmp_ad(:)
    real, allocatable :: xtmp(:)

    allocate(xtmp(n))
    allocate(xtmp_ad(n))
    if (f) then
      xtmp_ad = x_ad * 2.0 * x ! xtmp = x**2
      xtmp = x**2
      y_ad = xtmp_ad ! y = xtmp
      y = xtmp
      if (allocated(xtmp_ad)) then
        deallocate(xtmp_ad)
      end if
      if (allocated(xtmp)) then
        deallocate(xtmp)
      end if
      return
    end if
    xtmp_ad = x_ad ! xtmp = x + 1.0
    xtmp = x + 1.0
    y_ad = xtmp_ad * x + x_ad * xtmp ! y = xtmp * x
    y = xtmp * x

    return
  end subroutine alloc_return_fwd_ad

  subroutine alloc_return_rev_ad(n, x, x_ad, y_ad, f)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(inout) :: x_ad(n)
    real, intent(inout) :: y_ad(n)
    logical, intent(in)  :: f
    real, allocatable :: xtmp_ad(:)
    logical :: return_flag_28_ad
    real, allocatable :: xtmp(:)

    return_flag_28_ad = .true.
    allocate(xtmp_ad(n))
    allocate(xtmp(n))
    if (f) then
      xtmp = x**2
      return_flag_28_ad = .false.
    end if
    if (return_flag_28_ad) then
      xtmp = x + 1.0
    end if

    if (return_flag_28_ad) then
      xtmp_ad = y_ad * x ! y = xtmp * x
      x_ad = y_ad * xtmp + x_ad ! y = xtmp * x
      y_ad = 0.0 ! y = xtmp * x
      x_ad = xtmp_ad + x_ad ! xtmp = x + 1.0
    end if
    if (f) then
      return_flag_28_ad = .true. ! return
      xtmp_ad = y_ad ! y = xtmp
      y_ad = 0.0 ! y = xtmp
      x_ad = xtmp_ad * 2.0 * x + x_ad ! xtmp = x**2
    end if
    if (return_flag_28_ad) then
      if (allocated(xtmp_ad)) then
        deallocate(xtmp_ad)
      end if
      if (allocated(xtmp)) then
        deallocate(xtmp)
      end if
    end if

    return
  end subroutine alloc_return_rev_ad

end module return_example_ad
