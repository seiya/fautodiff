module cross_mod_a
  implicit none
contains
  subroutine incval(a)
    real, intent(inout) :: a
    a = a + 1.0
  end subroutine incval
end module cross_mod_a
