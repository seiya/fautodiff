module cross_mod_a
  implicit none

contains

  subroutine incval(a, inc)
    real, intent(inout) :: a
    real, intent(in) :: inc

    a = a + inc

    return
  end subroutine incval

end module cross_mod_a
