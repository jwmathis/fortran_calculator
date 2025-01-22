program complex_num_calculator
    ! Import the calc_module
    USE calc_module
    implicit none
    
    ! Define variables
    type(complex_number) :: a, b, c
    integer :: x
    character(len=50) :: user_input
    integer :: j
    logical :: exit = .false.
    
    do while (.not. exit)
        ! Print the title
        print *, "Complex Number Calculator"
        print *, "-------------------------"
        print *, "Enter the operation you want to perform:"
        print *, "1. Add"
        print *, "2. Subtract"
        print *, "3. Multiply"
        print *, "4. Divide"
        print *, "5. Power"
        print *, "6. Conjugate"
        print *, "7. Exit"
        read(*, *) j

        if (j == 7) then
            exit = .true.
            stop
        else
        ! Get user input
        print *, "How do you want to enter the complex numbers?"
        print *, "1. (real, imag)"
        print *, "2. real + imag*i"
        print *, "3. real (for real numbers)"
        read(*, *) n

        if (n == 1) then ! If the user wants to enter the complex numbers in the form (real, imag)
            ! Get user input
            print *, "Enter the first complex number in the form (real, imag) (e.g. 2.0, 3.0): "
            read(*, "(F8.3, F8.3)") a%real, a%imag

            if (j == 5) then ! If the user wants to raise a number to a power
                print *, "Enter the power of the complex number: "
                read(*, *) x
            else
                print *, "Enter the second complex number in the form (real, imag) (e.g. 2.0, 3.0): "
                read(*, "(F8.3, F8.3)") b%real, b%imag
            end if

        else if (n == 2) then ! If the user wants to enter the complex numbers in the form real + imag*i
            print *, "Enter the first complex number in the form real + imag*i: (e.g. 1 + 2i) "
            read(*, '(A)') user_input
            call parse_complex_number(user_input, a)

            if (j == 5) then
                print *, "Enter the power of the complex number: "
                read(*, *) x
            else
                print *, "Enter the second complex number in the form real + imag*i: (e.g. 1 + 2i) "
                read(*, '(A)') user_input
                call parse_complex_number(user_input, b)
            end if

        else if (n == 3) then ! If the user wants to enter the complex numbers in the form real
            print *, "Enter the first number: "
            read(*, *) a%real
            a%imag = 0.0 ! Set the imaginary part to 0
            if (j == 5) then
                print *, "Enter the power of the complex number: "
                read(*, *) x
            else
                print *, "Enter the second number: "
                read(*, *) b%real
                b%imag = 0.0 ! Set the imaginary part to 0
            end if

        end if

        ! Print the complex number DEBUGGING STATEMENT
        ! print *, "The first complex number is: ", a%real, "+", a%imag, "i"
        ! print *, "The second complex number is: ", b%real, "+", b%imag, "i"

        ! Perform the operation
        select case(j)
            case(1)
                c = add(a, b)
                if (n==1) then ! If the user wants to enter the complex numbers in the form (real, imag)
                    print *, "The result of the addition is: (", c%real, ", ", c%imag, ")"
                else if (n==2) then ! If the user wants to enter the complex numbers in the form real + imag*i
                    print *, "The result of the addition is: ", c%real, "+", c%imag, "i"
                else if (n==3) then ! If the user wants to enter the complex numbers in the form real
                    print *, "The result of the addition is: ", c%real
                end if
            case(2)
                c = sub(a, b)
                if (n==1) then ! If the user wants to enter the complex numbers in the form (real, imag)
                    print *, "The result of the addition is: (", c%real, ", ", c%imag, ")"
                else if (n==2) then ! If the user wants to enter the complex numbers in the form real + imag*i
                    print *, "The result of the addition is: ", c%real, "+", c%imag, "i"
                else if (n==3) then ! If the user wants to enter the complex numbers in the form real
                    print *, "The result of the addition is: ", c%real
                end if
            case(3)
                c = mul(a, b)
                if (n==1) then ! If the user wants to enter the complex numbers in the form (real, imag)
                    print *, "The result of the addition is: (", c%real, ", ", c%imag, ")"
                else if (n==2) then ! If the user wants to enter the complex numbers in the form real + imag*i
                    print *, "The result of the addition is: ", c%real, "+", c%imag, "i"
                else if (n==3) then ! If the user wants to enter the complex numbers in the form real
                    print *, "The result of the addition is: ", c%real
                end if
            case(4)
                c = div(a, b)
                if (n==1) then ! If the user wants to enter the complex numbers in the form (real, imag)
                    print *, "The result of the addition is: (", c%real, ", ", c%imag, ")"
                else if (n==2) then ! If the user wants to enter the complex numbers in the form real + imag*i
                    print *, "The result of the addition is: ", c%real, "+", c%imag, "i"
                else if (n==3) then ! If the user wants to enter the complex numbers in the form real
                    print *, "The result of the addition is: ", c%real
                end if
            case(5)
                c = pow(a, x)
                if (n==1) then ! If the user wants to enter the complex numbers in the form (real, imag)
                    print *, "The result of the addition is: (", c%real, ", ", c%imag, ")"
                else if (n==2) then ! If the user wants to enter the complex numbers in the form real + imag*i
                    print *, "The result of the addition is: ", c%real, "+", c%imag, "i"
                else if (n==3) then ! If the user wants to enter the complex numbers in the form real
                    print *, "The result of the addition is: ", c%real
                end if   
        end select
    end if
end do  
end program complex_num_calculator
