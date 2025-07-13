module exit_cycle
  implicit none

contains

  subroutine loop_exit_cycle(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    integer :: i

    res = 0.0
    do i = 1, n
      if (i == 2) cycle
      res = res + i * x
      if (i == 4) exit
    end do

    return
  end subroutine loop_exit_cycle

end module exit_cycle
