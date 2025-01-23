program complex_num_calculator
    ! Import the calc_module
    use calc_module
    use utils_module
    implicit none
    
    ! Define variables
    type(complex_number) :: a, b, c
    integer :: x, j, n
    logical :: exit = .false.
    
    do while (.not. exit)
        ! Print the title
        call print_menu()
        read(*, *) j

        ! Check if the user wants to exit
        if (j == 6) then
            exit = .true.
            stop
        else
        
        call print_format_menu()
        read(*, *) n


        ! Print the complex number DEBUGGING STATEMENT
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
            case default
                print *, "Invalid input. Please select a valid option from the menu."
        end select
    end if
end do  
end program complex_num_calculator
