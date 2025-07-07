module cross_mod_b
  use cross_mod_a
  implicit none

contains

  subroutine call_inc(b)
    real, intent(inout) :: b
    real :: inc
    inc = 1.0
    call incval(b, inc)

    return
  end subroutine call_inc

  subroutine call_inc_kw(b)
    real, intent(inout) :: b
    real :: inc
    inc = 1.0
    call incval(inc=inc, a=b)

    return
  end subroutine call_inc_kw

end module cross_mod_b
