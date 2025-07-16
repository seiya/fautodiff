module exit_cycle
  implicit none

contains

  subroutine do_exit_cycle(n, x, res)
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
end subroutine do_exit_cycle

  subroutine while_exit_cycle(n, x, res)
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
  end subroutine while_exit_cycle

  subroutine exit_cycle_with_labels(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    integer :: i
    integer :: j
    integer :: k

    res = x
    i = 0
    outer: do while (i <= n)
      i = i + 1
      res = res + 1.0
      middle: do j = 1, n
        res = res + 10.0
        if (res > 5000.0) then
          exit outer
        end if
        inner: do k = 1, n
          if (res > 4000.0) then
            exit outer
          end if
          res = res + 100.0
          if (res > 2400.0) then
            exit middle
          end if
          res = res + 100.0
          if (res > 2200.0) then
            cycle middle
          end if
          res = res + 100.0
          if (res > 1000.0) then
            cycle outer
          end if
          res = res * x
        end do inner
      end do middle
      res = res * x
    end do outer

    return
  end subroutine exit_cycle_with_labels

end module exit_cycle
