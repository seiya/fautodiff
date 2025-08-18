module stop_example
contains
  subroutine stop_sub(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    if (x < 0.0) then
      stop 'negative'
    end if
    if (x > 10.0) then
      error stop 1
    end if
    y = x
  end subroutine stop_sub
end module stop_example
