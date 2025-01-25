module operations_module
    implicit none

    ! Define a complex number type
    type complex_number
        real(kind=4) :: real, imag
    end type complex_number
    integer :: z
    contains

        ! Add two numbers
        function add(a, b) result(c)
            type(complex_number) :: a, b, c
            c%real = a%real + b%real
            c%imag = a%imag + b%imag
        end function add

        ! Subtract two numbers
        function sub(a, b) result(c)
            type(complex_number) :: a, b, c
            c%real = a%real - b%real
            c%imag = a%imag - b%imag
        end function sub

        ! Multiply two numbers
        function mul(a, b) result(c)
            type(complex_number) :: a, b, c
            c%real = a%real * b%real - a%imag * b%imag
            c%imag = a%real * b%imag + a%imag * b%real
        end function mul

        ! Divide two numbers
        function div(a, b) result(c)
            ! To divide complex numbers, we need to divide the real and imaginary parts separately
            ! To do this, we need to calculate the denominator which is the square of the imaginary part of b and the real part of b (complex conjugate)
            ! Then we can divide the real and imaginary parts
            type(complex_number) :: a, b, c
            real(kind=4) :: denom
            denom = b%real ** 2 + b%imag ** 2
            c%real = (a%real * b%real + a%imag * b%imag) / denom
            c%imag = (a%imag * b%real - a%real * b%imag) / denom
        end function div

        ! Raise a number to a power
        ! Add code to raise a number to a negative power
        function pow(a, b) result(c)
            type(complex_number) :: a, c
            integer :: b
            c = a

            if (b == 0) then ! If b is 0, return 1
                c%real = 1
                c%imag = 0
                return
            else if (b == 1) then ! If b is 1, return the original number
                return
            end if


            ! If b is not 0 or 1, raise the number to the power of b
            do z = 1, (b-1) ! Raise the number to the power of b
                c = mul(c, a) ! Multiply the current result by the original number
            end do
        end function pow

        ! Take conjugate of a number
        function conj(a) result(c)
            type(complex_number) :: a, c
            c%real = a%real
            c%imag = -a%imag
        end function conj
end module operations_module