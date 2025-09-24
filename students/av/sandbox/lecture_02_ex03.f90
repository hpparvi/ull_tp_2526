PROGRAM lecture_02_ex03
    IMPLICIT NONE
    REAL(kind=8) :: fact, r

    ! Loop for validate the user input
    DO
        ! Requesting the user tu introduce a number using the terminal
        WRITE(*, '(/,A)', advance='no') "Introduce a non-negative real number: "
        READ *, r

        ! Exit loop if the number is correct
        IF (r >= 0.0_8) EXIT

        ! Requesting to repeat the input number with a valid one
        PRINT *, "Error: Number must be non-negative. Try it again."
    END DO

    ! Computing factorial value using gamma function
    fact = gamma(r + 1.0_8)
    PRINT '("Factorial of ", F0.6, " = ", G0.6)', r, fact
END PROGRAM lecture_02_ex03