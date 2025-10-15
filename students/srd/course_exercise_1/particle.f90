module particle
  use geometry
  implicit none
  
  type :: particle3d
     type(point3d) :: p   ! position with a point3d
     type(vector3d) :: v  ! velocity with a vector3d
     real(kind=8) :: m            ! mass with a real number (can also use SELECTED_REAL_KIND for portability)
  end type particle3d

end module particle

