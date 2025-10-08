! Create module geometry stored in geometry.f90 file that contains:
! ▶ types vector3d and point3d, both with (64-bit) real components x, y, and z.
! ▶ functions sumvp, sumpv, subvp, subpv, mulrv, mulvr, divvr for adding and subtracting
!   points and vectors, and for multiplying and dividing vectors with reals.
! ▶ operators matching these functions.
! ▶ function distance that calculates the distance between two 3d points.
! ▶ function angle that calculates the angle between two vectors a and b (in radians).
! ▶ function normalize that takes a vector a and returns it divided by its length.
! ▶ function cross product that takes two 3d vectors (a and b) and returns their cross product.

module geometry
    use iso_fortran_env, only: real64
    implicit none

    ! Defining types
    type vector3d
        real(real64) :: x, y, z
    end type vector3d

    type point3d
        real(real64) :: x, y, z
    end type point3d

    ! Custom operators
    interface operator (+)
        module procedure sumvp , sumpv
    end interface

    interface operator (-)
        module procedure subvp , subpv
    end interface

    interface operator (*)
        module procedure mulrv , mulvr
    end interface

    interface operator (/)
        module procedure divvr
    end interface

    contains

        ! sum functions
        pure type(point3d) function sumvp(a, b)
            type(vector3d), intent(in) :: a
            type(point3d), intent(in) :: b
            sumvp = point3d(a%x + b%x, a%y + b%y, a%z + b%z)
        end function sumvp

        pure type(point3d) function sumpv(a, b)
            type(point3d), intent(in) :: a
            type(vector3d), intent(in) :: b
            sumpv = point3d(a%x + b%x, a%y + b%y, a%z + b%z)
        end function sumpv

        ! subtract functions
        pure type(point3d) function subvp(a, b)
            type(vector3d), intent(in) :: a
            type(point3d), intent(in) :: b
            subvp = point3d(a%x - b%x, a%y - b%y, a%z - b%z)
        end function subvp

        pure type(point3d) function subpv(a, b)
            type(point3d), intent(in) :: a
            type(vector3d), intent(in) :: b
            subpv = point3d(a%x - b%x, a%y - b%y, a%z - b%z)
        end function subpv

        ! multiplication functions
        pure type(vector3d) function mulrv(a, b)
            real(real64), intent(in) :: a
            type(vector3d), intent(in) :: b
            mulrv = vector3d ( a * b%x, a * b%y, a * b%z)
        end function mulrv

        pure type(vector3d) function mulvr(a, b)
            type(vector3d), intent(in) :: a
            real(real64), intent(in) :: b
            mulvr = vector3d ( a%x * b, a%y * b, a%z * b)
        end function mulvr
        
        ! division function
        pure type(vector3d) function divvr(a, b)
            type(vector3d), intent(in) :: a
            real(real64), intent(in) :: b
            divvr = vector3d ( a%x / b, a%y / b, a%z / b)
        end function divvr

        ! function distance that calculates the distance between two 3d points
        pure real(real64) function distance(a, b)
            type(point3d), intent(in) :: a, b
            distance = sqrt( (b%x - a%x)**2 + (b%y - a%y)**2 + (b%z - a%z)**2 )
        end function distance

        ! function angle that calculates the angle between two vectors a and b (in radians)
        pure real(real64) function angle(a, b)
            type(vector3d), intent(in) :: a, b
            real(real64) :: norm_a, norm_b
            norm_a = sqrt( a%x**2 + a%y**2 + a%z**2 )
            norm_b = sqrt( b%x**2 + b%y**2 + b%z**2 )
            angle = acos( (a%x * b%x + a%y * b%y + a%z * b%z) / (norm_a * norm_b) )
        end function angle

        ! function normalize that takes a vector a and returns it divided by its length
        pure type(vector3d) function normalize(a)
            type(vector3d), intent(in) :: a
            real(real64) :: norm_a
            norm_a = sqrt( a%x**2 + a%y**2 + a%z**2 )
            normalize = a / norm_a
        end function normalize

        ! function cross product that takes two 3d vectors (a and b) and returns their cross product
        pure type(vector3d) function cross_product(a, b)
            type(vector3d), intent(in) :: a, b
            cross_product%x = a%y*b%z - a%z*b%y
            cross_product%y = a%z*b%x - a%x*b%z
            cross_product%z = a%x*b%y - a%y*b%x
        end function cross_product


end module geometry


