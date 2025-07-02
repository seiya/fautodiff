module call_example
  implicit none

contains

  subroutine foo(a, b)
    real, intent(inout) :: a
    real, intent(in) :: b

    a = a * 2.0 + b

    return
  end subroutine foo

  function bar(a) result(b)
    real, intent(in) :: a
    real :: b

    b = a**2

    return
  end function bar

  subroutine call_subroutine(x, y)
    real, intent(inout) :: x
    real, intent(in) :: y

    call foo(x, y)

    return
  end subroutine call_subroutine

  subroutine call_fucntion(x, y)
    real, intent(out) :: x
    real, intent(in) :: y

    x = bar(y)

    return
  end subroutine call_fucntion

  subroutine arg_operation(x, y)
    real, intent(inout) :: x
    real, intent(in) :: y

    call foo(x, y * 2.0)

    return
  end subroutine arg_operation

  subroutine arg_function(x, y)
    real, intent(inout) :: x
    real, intent(in) :: y

    call foo(x, bar(y))

    return
  end subroutine arg_function

end module call_example
