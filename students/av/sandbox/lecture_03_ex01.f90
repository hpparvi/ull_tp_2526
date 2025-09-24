program lecture_03_ex01
    implicit none

    ! Defining the matrix
    real, dimension(3, 4) :: a
    real, dimension(3, 3) :: b
    real, dimension(3, 4) :: c  ! C = B x A with shape (3x4)
    integer :: i

    ! Defining matrix values with two methods
    ! Input data values in matrix form (by rows) using order
    A = reshape((/ 3.0, 2.0, 4.0, 1.0,  &
                   2.0, 4.0, 2.0, 2.0,  &
                   1.0, 2.0, 3.0, 7.0 /), & 
                   (/ 3, 4 /), order=[2, 1])

    ! Print matrix a
    write(*, '(/,a)') "Matrix A is:"
    do i = 1, 3
        print '(4f5.0)', A(i, :)
    end do

    ! Input values in matrix by columns 
    B = reshape((/ 3.0, 2.0, 3.0, &
                   2.0, 1.0, 0.0, &
                   4.0, 2.0, 2.0 /), (/ 3, 3 /))

    ! Print matrix b
    write(*, '(/,a)') "Matrix B is:"
    do i = 1, 3
        print '(3f5.0)', B(i, :)
    end do

    ! Computing matrix product
    C = matmul(B,A)

    ! Print the result
    write(*, '(/,a)') "Result of matrix product C = B x A is:"
    do i = 1, 3
        print '(4f5.0)', C(i, :)
    end do

end program lecture_03_ex01