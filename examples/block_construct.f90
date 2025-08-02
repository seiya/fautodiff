module block_construct
  implicit none
  real :: z
contains
  subroutine compute_module(val)
    real, intent(in) :: val
    z = val + 1.0
  end subroutine compute_module

  subroutine use_block(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    real :: z
    z = x + 1.0
    block
      real :: z
      z = x + 2.0
      y = z + 1.0
    end block
    y = y + z
  end subroutine use_block
end module block_construct
