PROGRAM lecture_02_ex02_validado
    IMPLICIT NONE
    INTEGER :: i, n
    INTEGER(kind=8) :: fact

    ! Loop for validate the user input
    DO
        ! Requesting the user tu introduce a nautural number using the terminal
        WRITE(*, '(/,A)', advance='no') "Introduce a natural number (>= 0): "
        READ * , n

        ! Exit loop if the number is correct
        IF (n >= 0) EXIT

        ! Requesting to repeat the input number with a valid one
        PRINT *, "Error: Number must be non-negative (>= 0). Try it again."
    END DO


    fact = 1
    PRINT '(/,"Factorial numbers from 0 to ", I0, " are:")', n

    ! Factorial of 0
    PRINT '(I0, "! = ", I0)', 0, 1

    ! Computing the factorial numbers from 1 to n
    DO i = 1, n
        fact = fact * i
        PRINT '(I0, "! = ", I0)', i, fact
    END DO

END PROGRAM lecture_02_ex02_validado