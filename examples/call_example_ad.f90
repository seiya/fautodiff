module call_example_ad
  implicit none

contains

  subroutine foo_ad(a, a_ad, b, b_ad)
    real, intent(inout) :: a
    real, intent(inout) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad

    b_ad = a_ad ! a = a * 2.0 + b
    a_ad = a_ad * 2.0 ! a = a * 2.0 + b

    return
  end subroutine foo_ad

  subroutine bar_ad(a, a_ad, b_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(inout) :: b_ad

    a_ad = b_ad * 2.0 * a ! b = a**2
    b_ad = 0.0 ! b = a**2

    return
  end subroutine bar_ad

  subroutine call_subroutine_ad(x, x_ad, y, y_ad)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad

    call foo_ad(x, x_ad, y, y_ad) ! call foo(x, y)

    return
  end subroutine call_subroutine_ad

  subroutine call_fucntion_ad(x_ad, y, y_ad)
    real, intent(inout) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad
    real :: bar0_save_33_ad

    bar0_save_33_ad = x_ad ! x = bar(y)
    x_ad = 0.0 ! x = bar(y)
    call bar_ad(y, y_ad, bar0_save_33_ad) ! x = bar(y)

    return
  end subroutine call_fucntion_ad

  subroutine arg_operation_ad(x, x_ad, y, y_ad)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad
    real :: foo_arg1_save_41_ad

    call foo_ad(x, x_ad, y * 2.0, foo_arg1_save_41_ad) ! call foo(x, y * 2.0)
    y_ad = foo_arg1_save_41_ad * 2.0 ! call foo(x, y * 2.0)

    return
  end subroutine arg_operation_ad

  subroutine arg_function_ad(x, x_ad, y, y_ad)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad
    real :: bar0_save_49_ad
    real :: foo_arg1_save_49_ad

    call foo_ad(x, x_ad, bar(y), foo_arg1_save_49_ad) ! call foo(x, bar(y))
    bar0_save_49_ad = foo_arg1_save_49_ad ! call foo(x, bar(y))
    call bar_ad(y, y_ad, bar0_save_49_ad) ! call foo(x, bar(y))

    return
  end subroutine arg_function_ad

end module call_example_ad
