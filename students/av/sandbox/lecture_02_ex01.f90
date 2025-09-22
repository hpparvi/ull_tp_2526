PROGRAM lecture_02_ex01
    IMPLICIT NONE
    INTEGER :: i, n

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

    PRINT *
    DO i = 1, n, 1
        WRITE(*, '(A)') "Hello World!"
    END DO
END PROGRAM lecture_02_ex01


