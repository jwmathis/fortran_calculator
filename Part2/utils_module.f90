module utils_module
    use calc_module
    implicit none

    contains

        ! Print the menu
        subroutine print_menu()
            print *, "Complex Number Calculator"
            print *, "-------------------------"
            print *, "Enter the operation you want to perform:"
            print *, "1. Add"
            print *, "2. Subtract"
            print *, "3. Multiply"
            print *, "4. Divide"
            print *, "5. Power"
            print *, "6. Exit"
        end subroutine print_menu

        ! Print format menu
        subroutine print_format_menu()
            print *, "Enter the format you want to use:" 
            print *, "1. (real, imag)"
            print *, "2. real + imag*i"
            print *, "3. real"
        end subroutine print_format_menu

        ! Parse complex number
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

        ! Get formatted input
        subroutine get_formatted_input(n, j, a, b, x)
            implicit none
            integer, intent(in) :: n, j
            type(complex_number), intent(out) :: a, b
            integer, intent(out), optional :: x
        
            character(len=50) :: user_input
        
            select case (n)
                case (1)
                    ! Get user input in the form (real, imag)
                    print *, "Enter the first complex number in the form (real, imag): "
                    read(*, "(F8.3, F8.3)") a%real, a%imag
                    if (j == 5) then
                        print *, "Enter the power of the complex number: "
                        read(*, *) x
                    else
                        print *, "Enter the second complex number in the form (real, imag): "
                        read(*, "(F8.3, F8.3)") b%real, b%imag
                    end if
        
                case (2)
                    ! Get user input in the form real + imag*i
                    if (j == 5) then
                        print *, "Enter the complex number in the form real + imag*i: "
                        read(*, '(A)') user_input
                        call parse_complex_number(user_input, a)
                        print *, "Enter the power of the complex number: "
                        read(*, *) x
                    else
                        print *, "Enter the first complex number in the form real + imag*i: "
                        read(*, '(A)') user_input
                        call parse_complex_number(user_input, a)
                        print *, "Enter the second complex number in the form real + imag*i: "
                        read(*, '(A)') user_input
                        call parse_complex_number(user_input, b)
                    end if
        
                case (3)
                    ! Get user input in the form real
                    if (j == 5) then
                        print *, "Enter the number: "
                        read(*, *) a%real
                        a%imag = 0.0
                        print *, "Enter the power of the complex number: "
                        read(*, *) x
                    else
                        print *, "Enter the first number: "
                        read(*, *) a%real
                        a%imag = 0.0
                        print *, "Enter the second number: "
                        read(*, *) b%real
                        b%imag = 0.0
                    end if
            end select
        end subroutine get_formatted_input

        ! Print output
        subroutine print_formatted_output(c, n)
            type(complex_number) :: c
            integer :: n

            if (n==1) then ! If the user wants to enter the complex numbers in the form (real, imag)
                print *, "The result is: (", c%real, ", ", c%imag, ")"
            else if (n==2) then ! If the user wants to enter the complex numbers in the form real + imag*i
                print *, "The result is: ", c%real, "+", c%imag, "i"
            else if (n==3) then ! If the user wants to enter the complex numbers in the form real
                print *, "The result is: ", c%real
            end if
        end subroutine print_formatted_output

end module utils_module