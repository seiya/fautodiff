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
end module return_example
