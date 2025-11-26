program polynomial_solver

    real, allocatable:: coefficients(:)
    integer:: degree, i
    real:: fv

    write(*,*) 'Enter the degree of the polynomial:'
    read(*,*) degree
    allocate(coefficients(0:degree))
    write(*,*) 'Enter the coefficients (from highest degree to constant term):'
    do i = 0, degree, 1
        write(*,"('Coefficient for x^',I1)") degree - i
        read(*,*) coefficients(i)
    end do

    write(*,*) 'Entered polynomial is:'//new_line("A")//"f(x) = "

    fv= evaluate_polynomial(coefficients, 2.0)
    write(*,*) 'The value of the polynomial at x = 2.0 is:',  fv

    contains 
        real function evaluate_polynomial(coeffs, x_val) 
            real, intent(in):: coeffs(:)
            integer, allocatable::deg
            real, intent(in):: x_val
            integer:: j
            allocate(deg)
            deg = size(coeffs)
            evaluate_polynomial = 0.0
            do j = 1, deg, 1
                ! write(*,*) 'Adding term: ', coeffs(j), '*x^', deg - j
                evaluate_polynomial = evaluate_polynomial + coeffs(j) * x_val**(deg - j)
            end do
        end function evaluate_polynomial

        character(len=100) function write_polynomial(coefficients)
            real, intent(in):: coefficients(:)
            real::c
            integer:: k, deg
            character(:), allocatable:: str
            deg = size(coefficients)
            str = ""
            do k = 1, deg, 1
                c = coefficients(k)
                if (c /= 0.0) then
                    write(str, '(F6.2)') c
                end if
            end do
            write_polynomial = str
        end function write_polynomial

end program