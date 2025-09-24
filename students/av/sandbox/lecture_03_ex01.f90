PROGRAM lecture_03_ex01
    IMPLICIT NONE

    ! Defining the matrix
    REAL, DIMENSION(3, 4) :: A
    REAL, DIMENSION(3, 3) :: B
    REAL, DIMENSION(3, 4) :: C  ! C=BxA with shape (3x4)
    INTEGER :: i

    ! Defining matrix values with two methods
    ! Input data values in matrix form (by rows) using ORDER
    A = RESHAPE((/ 3.0, 2.0, 4.0, 1.0,  &
                   2.0, 4.0, 2.0, 2.0,  &
                   1.0, 2.0, 3.0, 7.0 /), & 
                   (/ 3, 4 /), ORDER=[2, 1])

    ! Print matrix A
    WRITE(*, '(/,A)') "Matrix A is:"
    DO i = 1, 3
        PRINT '(4F5.0)', A(i, :)
    END DO

    ! Input values in matrix by columns 
    B = RESHAPE((/ 3.0, 2.0, 3.0, &
                   2.0, 1.0, 0.0, &
                   4.0, 2.0, 2.0 /), (/ 3, 3 /))

    ! Print matrix B
    WRITE(*, '(/,A)') "Matrix B is:"
    DO i = 1, 3
        PRINT '(3F5.0)', B(i, :)
    END DO

    ! Computing matrix product
    C = MATMUL(B,A)

    ! Print the result
    WRITE(*, '(/,A)') "Result of matrix product C = B x A is:"
    DO i = 1, 3
        PRINT '(4F5.0)', C(i, :)
    END DO

END PROGRAM lecture_03_ex01