! Tests for CABLE I/O modifications for GSWP3

MODULE test_io_mod
  IMPLICIT NONE
  PUBLIC

!  !ccc Define met field variable type.
!  TYPE GSWP3_MET_TYPE
!     REAL, DIMENSION(:), ALLOCATABLE :: VAL           ! Values
!     CHARACTER(len=12)               :: VAR_NAME      ! Variable name
!     CHARACTER(len=200)              :: MetFile       ! Complete path to file
!     INTEGER                         :: F_ID, V_ID    ! File and variable IDs    
!  END TYPE GSWP3_MET_TYPE
!
!  TYPE GSWP3_TYPE
!     INTEGER  :: mland, NMET, xdimsize, ydimsize, tdimsize
!     INTEGER  :: CYEAR, MetStart, MetEnd, CTSTEP, DT, ktau
!     REAL,   DIMENSION(:)  ,ALLOCATABLE :: AVG_LWDN, CO2VALS
!     LOGICAL  :: DirectRead, LeapYears
!     LOGICAL,DIMENSION(:,:),ALLOCATABLE :: LandMask
!     CHARACTER(len=15) :: Run,Forcing,RCP, CO2, NDEP,RCPdir
!     CHARACTER(len=200):: BasePath, MetPath, LandMaskFile
!     TYPE(GSWP3_MET_TYPE), DIMENSION(9) :: MET
!  END TYPE GSWP3_TYPE
!
!  TYPE(GSWP3_TYPE) :: METFORC
contains

   
!@test
  subroutine test_read_basepath()
     ! To test if BasePath is correctly read in.
     use pfunit_mod
     use cable_gswp3_mod, only: read_settings, GSWP3_TYPE

     TYPE(GSWP3_TYPE) :: METFORC
     
     call read_settings(METFORC)
#line 39 "test_io.pf"
  call assertEqual(trim(METFORC%BasePath),"TheMetFieldPath", &
 & location=SourceLocation( &
 & 'test_io.pf', &
 & 39) )
  if (anyExceptions()) return
#line 40 "test_io.pf"
  end subroutine test_read_basepath

!@test
  subroutine test_read_landmaskfile()
     ! To test if landmaskfile is correctly read in.
     use pfunit_mod
     use cable_gswp3_mod, only: read_settings, GSWP3_TYPE

     TYPE(GSWP3_TYPE) :: METFORC
     
     call read_settings(METFORC)
#line 51 "test_io.pf"
  call assertEqual(trim(METFORC%LandMaskFile),"TheLandMaskFile", &
 & location=SourceLocation( &
 & 'test_io.pf', &
 & 51) )
  if (anyExceptions()) return
#line 52 "test_io.pf"
  end subroutine test_read_landmaskfile
  

end MODULE test_io_mod

module Wraptest_io_mod
   use pFUnit_mod
   use test_io_mod
   implicit none
   private

contains


end module Wraptest_io_mod

function test_io_mod_suite() result(suite)
   use pFUnit_mod
   use test_io_mod
   use Wraptest_io_mod
   type (TestSuite) :: suite

   suite = newTestSuite('test_io_mod_suite')

   call suite%addTest(newTestMethod('test_read_basepath', test_read_basepath))

   call suite%addTest(newTestMethod('test_read_landmaskfile', test_read_landmaskfile))


end function test_io_mod_suite

