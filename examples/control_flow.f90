module control_flow
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
      z = -x
    else
      z = 0.0
    end if

    return
  end subroutine if_example

  subroutine select_example(i, x, z)
    integer, intent(in) :: i
    real, intent(in) :: x
    real, intent(out) :: z

    select case(i)
    case(1)
      z = x + 1.0
    case(2)
      z = x - 1.0
    case default
      z = 0.0
    end select

    return
  end subroutine select_example

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

end module control_flow
