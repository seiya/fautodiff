module control_flow_ad
  use fautodiff_stack
  implicit none


contains

  subroutine if_example(x, y, z)
    real, intent(in) :: x
    real, intent(inout) :: y
    real, intent(out) :: z

    if (x > 0.0) then
      y = y + 1.0
      z = x
    else if (x < 0.0) then
      y = y - 1.0
      z = - x
    else
      z = 0.0
    end if

    return
  end subroutine if_example

  subroutine if_example_fwd_ad(x, x_ad, y, z, z_ad)
    real, intent(in) :: x
    real, intent(in) :: x_ad
    real, intent(inout) :: y
    real, intent(out) :: z
    real, intent(out) :: z_ad

    if (x > 0.0) then
      y = y + 1.0
      z_ad = x_ad ! z = x
      z = x
    else if (x < 0.0) then
      y = y - 1.0
      z_ad = - x_ad ! z = -x
      z = - x
    else
      z_ad = 0.0 ! z = 0.0
      z = 0.0
    end if

    return
  end subroutine if_example_fwd_ad

  subroutine if_example_rev_ad(x, x_ad, z_ad)
    real, intent(in) :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: z_ad

    if (x > 0.0) then
      x_ad = z_ad + x_ad ! z = x
      z_ad = 0.0 ! z = x
    else if (x < 0.0) then
      x_ad = - z_ad + x_ad ! z = -x
      z_ad = 0.0 ! z = -x
    else
      z_ad = 0.0 ! z = 0.0
    end if

    return
  end subroutine if_example_rev_ad

  subroutine select_example(i, x, z)
    integer, intent(in) :: i
    real, intent(in) :: x
    real, intent(out) :: z

    select case (i)
    case (1)
      z = x + 1.0
    case (2, 3)
      z = x - 1.0
    case default
      z = 0.0
    end select

    return
  end subroutine select_example

  subroutine select_example_fwd_ad(i, x, x_ad, z, z_ad)
    integer, intent(in) :: i
    real, intent(in) :: x
    real, intent(in) :: x_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad

    select case (i)
    case (1)
      z_ad = x_ad ! z = x + 1.0
      z = x + 1.0
    case (2, 3)
      z_ad = x_ad ! z = x - 1.0
      z = x - 1.0
    case default
      z_ad = 0.0 ! z = 0.0
      z = 0.0
    end select

    return
  end subroutine select_example_fwd_ad

  subroutine select_example_rev_ad(i, x_ad, z_ad)
    integer, intent(in) :: i
    real, intent(inout) :: x_ad
    real, intent(inout) :: z_ad

    select case (i)
    case (1)
      x_ad = z_ad + x_ad ! z = x + 1.0
      z_ad = 0.0 ! z = x + 1.0
    case (2, 3)
      x_ad = z_ad + x_ad ! z = x - 1.0
      z_ad = 0.0 ! z = x - 1.0
    case default
      z_ad = 0.0 ! z = 0.0
    end select

    return
  end subroutine select_example_rev_ad

  subroutine do_example(n, x, sum)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: sum
    integer :: i

    sum = 0.0
    do i = 1, n
      sum = sum + i * x
    end do

    return
  end subroutine do_example

  subroutine do_example_fwd_ad(n, x, x_ad, sum, sum_ad)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(in) :: x_ad
    real, intent(out) :: sum
    real, intent(out) :: sum_ad
    integer :: i

    sum_ad = 0.0 ! sum = 0.0
    sum = 0.0
    do i = 1, n
      sum_ad = sum_ad + x_ad * i ! sum = sum + i * x
      sum = sum + i * x
    end do

    return
  end subroutine do_example_fwd_ad

  subroutine do_example_rev_ad(n, x_ad, sum_ad)
    integer, intent(in) :: n
    real, intent(inout) :: x_ad
    real, intent(inout) :: sum_ad
    integer :: i

    do i = n, 1, - 1
      x_ad = sum_ad * i + x_ad ! sum = sum + i * x
    end do
    sum_ad = 0.0 ! sum = 0.0

    return
  end subroutine do_example_rev_ad

  subroutine do_while_example(x, limit, count)
    real, intent(in) :: x
    real, intent(in) :: limit
    integer, intent(out) :: count
    real :: y

    y = x
    count = 0
    do while (y < limit)
      y = y * 2.0
      count = count + 1
    end do

    return
  end subroutine do_while_example

  subroutine do_while_example_rev_ad(x, x_ad, limit)
    real, intent(in) :: x
    real, intent(inout) :: x_ad
    real, intent(in) :: limit
    real :: y_ad
    real :: y

    y = x
    call fautodiff_stack_l%push(.false.)
    do while (y < limit)
      call fautodiff_stack_l%push(.true.)
      y = y * 2.0
    end do

    y_ad = 0.0

    do while (fautodiff_stack_l%get())
      y_ad = y_ad * 2.0 ! y = y * 2.0
    end do
    x_ad = y_ad + x_ad ! y = x

    return
  end subroutine do_while_example_rev_ad

end module control_flow_ad
