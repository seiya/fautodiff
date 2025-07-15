module exit_cycle
  implicit none

contains

  subroutine loop_exit_cycle(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    integer :: i

    res = x**2
    do i = 1, n
      res = res * x
      if (i == 2) cycle
      res = res * x
      if (i == 4) exit
      res = res * x
      if (res > 4.0) then
        res = res * x
        cycle
      end if
      res = res * x
      if (res > 2.0) then
        res = res * x
        exit
      end if
      res = res * x
    end do
    res = res * x

    return
end subroutine loop_exit_cycle

  subroutine loop_exit_cycle_while(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    integer :: i

    res = x**2
    i = 1
    do while (i <= n)
      res = res * x
      if (i == 2) then
        i = i + 1
        cycle
      end if
      res = res * x
      if (i == 4) exit
      res = res * x
      if (res > 4.0) then
        res = res * x
        i = i + 1
        cycle
      end if
      res = res * x
      if (res > 2.0) then
        res = res * x
        exit
      end if
      res = res * x
      i = i + 1
    end do
    res = res * x
  end subroutine loop_exit_cycle_while

end module exit_cycle
