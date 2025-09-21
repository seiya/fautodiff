module return_example
contains

  subroutine conditional_return(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    if (x < 0.0) then
      y = -x
      return
    end if
    y = x * x
    return
  end subroutine conditional_return

  subroutine alloc_return(n, x, y, f)
  integer, intent(in) :: n
  real, intent(in) :: x(n)
  real, intent(out) :: y(n)
  logical, intent(in) :: f
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

end module return_example
