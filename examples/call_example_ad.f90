module call_example_ad
  implicit none

contains

  subroutine foo_ad(a, a_ad)
    real, intent(inout) :: a
    real, intent(inout) :: a_ad

    a_ad = a_ad * 2.0 ! a = a * 2.0

    return
  end subroutine foo_ad

  subroutine bar_ad(x, x_ad)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad

    call foo(x)

    call foo_ad(x, x_ad)

    return
  end subroutine bar_ad

end module call_example_ad
