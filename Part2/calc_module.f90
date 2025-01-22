module calc_module
    implicit none

    ! Define a complex number type
    type complex_number
        real(kind=4) :: real, imag
    end type complex_number
    integer :: n

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
        function pow(a, b) result(c)
            type(complex_number) :: a, c
            integer :: b
            c%real = 1 ! Set real part to 1
            c%imag = 0 ! Set imaginary part to 0

            if (b == 0) then ! If b is 0, return 1
                c%real = 1
                c%imag = 0
                return
            end if
            ! If b is not 0, raise the number to the power of b
            do n = 1, b ! Raise the number to the power of b
                c = mul(c, a) ! Multiply the current result by the original number
            end do
        end function pow

        ! Take conjugate of a number
        function conj(a) result(c)
            type(complex_number) :: a, c
            c%real = a%real
            c%imag = -a%imag
        end function conj

        subroutine parse_complex_number(input, a)
            character(len=*), intent(in):: input
            type(complex_number) :: a
            integer :: position_plus, position_i
            real(kind=8) :: real_part, imag_part

            ! Find the position of the plus sign
            position_plus = index(input, "+")
            ! Find the position of the i
            position_i = index(input, "i")
            
            if (position_plus > 0 .and. position_i > 0) then 
                ! Get the real and imaginary parts
                read(input(1:position_plus - 1), *) real_part
                read(input(position_plus + 1:position_i - 1), *) imag_part

                a%real = real_part
                a%imag = imag_part

            else
                print *, "Invalid input"
                stop
            end if
        end subroutine parse_complex_number

end module calc_module