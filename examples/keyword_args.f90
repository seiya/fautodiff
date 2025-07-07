module keyword_args
  implicit none

contains

  subroutine inc(a, b)
    real, intent(inout) :: a
    real, intent(in) :: b
    a = a + b
  end subroutine inc

  subroutine do_inc(x, y)
    real, intent(inout) :: x
    real, intent(in) :: y
    call inc(a=x, b=y)
  end subroutine do_inc

end module keyword_args
