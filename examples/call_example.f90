module call_example
  implicit none

contains

  subroutine foo(a)
    real, intent(inout) :: a

    a = a * 2.0

    return
  end subroutine foo

  subroutine bar(x)
    real, intent(inout) :: x

    call foo(x)

    return
  end subroutine bar

end module call_example
