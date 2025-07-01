module control_flow_ad
  use control_flow
  implicit none

contains

  subroutine if_example_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    real, intent(inout) :: z_ad

    x_ad = 0.0

    if (x > 0.0) then
      x_ad = z_ad ! z = x
      z_ad = 0.0 ! z = x
    else if (x < 0.0) then
      x_ad = - z_ad ! z = -x
      z_ad = 0.0 ! z = -x
    else
      z_ad = 0.0 ! z = 0.0
    end if

    return
  end subroutine if_example_ad

  subroutine select_example_ad(i, x, x_ad, z_ad)
    integer, intent(in)  :: i
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: z_ad

    x_ad = 0.0

    select case (i)
    case (1)
      x_ad = z_ad ! z = x + 1.0
      z_ad = 0.0 ! z = x + 1.0
    case (2, 3)
      x_ad = z_ad ! z = x - 1.0
      z_ad = 0.0 ! z = x - 1.0
    case default
      z_ad = 0.0 ! z = 0.0
    end select

    return
  end subroutine select_example_ad

  subroutine do_example_ad(n, x, x_ad, sum_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: sum_ad
    integer :: i

    x_ad = 0.0

    do i = n, 1, - 1
      x_ad = sum_ad * i + x_ad ! sum = sum + i * x
    end do
    sum_ad = 0.0 ! sum = 0.0

    return
  end subroutine do_example_ad

  subroutine do_while_example_ad(x, x_ad, limit, limit_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: limit
    real, intent(out) :: limit_ad

    x_ad = 0.0
    limit_ad = 0.0

    return
  end subroutine do_while_example_ad

end module control_flow_ad
