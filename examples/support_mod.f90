module support_mod
  implicit none
contains
  subroutine add_one(val)
    real, intent(inout) :: val
    val = val + 1.0
    return
  end subroutine add_one
end module support_mod
