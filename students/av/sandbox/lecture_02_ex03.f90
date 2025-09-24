program lecture_02_ex03
    implicit none

    real(kind=8) :: fact, r

    ! Loop for validate the user input
    do
        ! Requesting the user to introduce a number using the terminal
        write(*, '(/,a)', advance='no') "Introduce a non-negative real number: "
        read *, r

        ! Exit loop if the number is correct
        if (r >= 0.0_8) exit ! Adding _8 precision

        ! Requesting to repeat the input number with a valid one
        print *, "Error: number must be non-negative. Try it again."
    end do

    ! Computing factorial value using gamma function
    fact = gamma(r + 1.0_8)
    print '("Factorial of ", f0.6, " = ", g0.6)', r, fact

end program lecture_02_ex03