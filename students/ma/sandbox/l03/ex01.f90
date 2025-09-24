program matx_mult
    implicit none

    ! We declare the matrices
    real, dimension(3,4) :: A
    real, dimension(3,3) :: B
    real, dimension(3,4) :: C ! The matrix result
    integer :: i,j,k ! the counters for the loops

    ! We can initialize the matrix using the reshape function
    ! The order is to specify that fills the matrix by rows
    A = reshape([3.0, 2.0, 4.0, 1.0, &
             2.0, 4.0, 2.0, 2.0, &
             1.0, 2.0, 3.0, 7.0], shape=[3,4], order=[2,1])

    B =reshape([3.0, 2.0, 4.0, &
             2.0, 1.0, 2.0, &
             3.0, 0.0, 2.0], shape=[3,3], order=[2,1])
    
    
    ! We can't multiply A and B, but B and A yes

    ! We can multiply matrices using matmul or loops

    C = 0.0 ! We initialize the matrix to 0
    
    do i=1,3
        do j=1,4
            do k=1,3
                C(i,j) = C(i,j) + B(i,k)*A(k,j)
            end do
        end do
    end do

    print *, "The result of the matrix multiplication is", C


end program matx_mult