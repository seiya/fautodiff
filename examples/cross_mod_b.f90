module cross_mod_b
  use cross_mod_a
  implicit none

contains

  subroutine call_inc(b)
    real, intent(inout) :: b

    call incval(b)

    return
  end subroutine call_inc

end module cross_mod_b
