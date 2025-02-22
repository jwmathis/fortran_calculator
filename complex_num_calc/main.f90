program complex_num_calculator
    ! Import the calc_module
    use operations_module
    use utils_module
    implicit none
    
    ! Define variables
    type(complex_number) :: a, b, c
    integer :: x, j, n
    logical :: exit = .false.
    character (len=50) :: user_input
    logical :: valid

    do while (.not. exit)
        ! Reset the user input variables
        j = 0
        n = 0
        
        ! Print the title
        do while (j < 1 .or. j > 7) ! Loop until the input is valid
            call print_menu()
            read(*, *) j

            if (j < 1 .or. j > 7) then
                print *, "Invalid input. Please select a number between 1 and 6 from the menu."
            end if
        end do

        ! Check if the user wants to exit
        if (j == 7) then
            exit = .true.
            stop
        else
        
        do while (n < 1 .or. n > 3) ! Loop until the input is valid
            call print_format_menu()
            read(*, *) n

            if (n < 1 .or. n > 3) then
                print *, "Invalid input. Please select a number between 1 and 3 from the menu."
            end if
        end do


        ! DEBUGGING STATEMENT: Print the complex number
        ! print *, "The first complex number is: ", a%real, "+", a%imag, "i"
        ! print *, "The second complex number is: ", b%real, "+", b%imag, "i"

        ! Perform the operation
        select case(j)
            case(1)
                call get_formatted_input(n, j, a, b)
                c = add(a, b)
                call print_formatted_output(c, n);
            case(2)
                call get_formatted_input(n, j, a, b)
                c = sub(a, b)
                call print_formatted_output(c, n);
            case(3)
                call get_formatted_input(n, j, a, b)
                c = mul(a, b)
                call print_formatted_output(c, n);
            case(4)
                call get_formatted_input(n, j, a, b)
                ! Check if the denominator is zero
                if (b%real == 0.0 .and. b%imag == 0.0) then
                    print *, "Undefined. Cannot divide by zero."
                else ! If the denominator is not zero
                    c = div(a, b)
                    call print_formatted_output(c, n);
                end if
            case(5)
                call get_formatted_input(n, j, a, b, x)
                c = pow(a, x)
                call print_formatted_output(c, n);  
            case(6)
                if (n == 1) then ! If the user wants to enter the complex numbers in the form (real, imag)
                    print *, "Enter the first complex number in the form (real, imag): "
                    read(*, "(F8.3, F8.3)") a%real, a%imag
                else if (n == 2) then ! If the user wants to enter the complex numbers in the form real + imag*i
                    print *, "Enter the first complex number in the form real + imag*i: "
                    read(*, '(A)') user_input
                    valid = parse_complex_number(user_input, a)
                else if (n == 3) then ! If the user wants to enter the complex numbers in the form real
                    print *, "Enter the first number: "
                    read(*, *) a%real
                    a%imag = 0.0
                end if
                c = conj(a)
                call print_formatted_output(c, n);
            case default
                print *, "Invalid input. Please select a valid option from the menu."
        end select
    end if
end do  
end program complex_num_calculator
