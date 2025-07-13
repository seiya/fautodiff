module exit_cycle_ad
  use exit_cycle
  implicit none

contains

  subroutine loop_exit_cycle_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    integer :: i

    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do i = 1, n
      if (i == 2) then
        cycle
      end if
      res_ad = res_ad + x_ad * i ! res = res + i * x
      res = res + i * x
      if (i == 4) then
        exit
      end if
    end do

    return
  end subroutine loop_exit_cycle_fwd_ad

  subroutine loop_exit_cycle_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    integer :: i

    do i = 1, n
      if (i == 2) then
        cycle
      end if
      if (i == 4) then
        exit
      end if
    end do

    x_ad = 0.0

    do i = min(n, 4), 1, - 1
      if (i == 2) then
        cycle
      end if
      if (i == 4) then
        exit
      end if
      if (i == 4) then
        exit
      end if
      x_ad = res_ad * i + x_ad ! res = res + i * x
      if (i == 2) then
        cycle
      end if
    end do
    res_ad = 0.0 ! res = 0.0

    return
  end subroutine loop_exit_cycle_rev_ad

end module exit_cycle_ad
