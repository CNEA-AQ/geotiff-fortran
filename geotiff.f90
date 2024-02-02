program test_tiff

  implicit none
  integer :: iUnit=101, ios
  character(10)      :: inpFile='test.tif'
  
  type TIFF_header
     character(len=2) :: byteOrder   !II/MM
     integer          :: magic_num   !42
     integer          :: offset
  endtype
  
  !type TIFF_IFD
  !   integer :: records
  !   integer, dimension (records) :: id,tagType,cnt,offset
  !endtype
  
  type TIFF_file
    type (TIFF_header) :: head        ! 8-byte  Header
  ! type (TIFF_IFD)    :: IFD(:)      ! 12-byte Image File Directory (IFD)
  ! type (GEOparams)   :: geo         ! GeoTIFF parameters
  ! real               :: data(:,:,:) ! data itself
  endtype
  
  type(TIFF_file)   :: tiff
  character(len=2)  :: cpu_End
  integer           :: thisRec       !position on file 

   print*,"CPU ENDIANNES:", cpuEnd()
   print*,'Opening tiff file: ',inpFile,'...'
   open(unit=iUnit, iostat=ios, file=inpFile,form='UNFORMATTED',            &
        action='READ',status='OLD',access='DIRECT', CONVERT="NATIVE", recl=1)

       call TIFF_readHeader(iUnit,tiff%head)  
       print*,'HEADER:                          '
       print*,'  > Byte-Order  : ',tiff%head%byteOrder
       print*,'  > Magic number: ',tiff%head%magic_num
       print*,'  > Offset      : ',tiff%head%offset   
   
       !If byteOrder is diffrent to CPU byte-order change the way of reading the tiff file:
       if ( tiff%head%byteOrder /= cpuEnd() ) then
         print*, "SWAPING BYTE ORDER ! "
         close(iUnit)
         open(unit=iUnit, iostat=ios, file=inpFile,form='UNFORMATTED',       &
             action='READ',status='OLD',access='DIRECT',CONVERT="SWAP",recl=1)
       endif 
       !Check magic-number (TIFF format identifier)
       if ( tiff%head%magic_num /= 42 ) stop 'This is not a TIFF file..'

      !If offset is negative change to unsigned-integer
      if (tiff%head%offset<0)  tiff%head%offset = tiff%head%offset + 4294967296_8  ! adjustment for 4-byte unsigned integers


      !!thisrec = IFDOffset

      !!!read 2-byte IFD entry count; number of tiff tags
      !!read(unit=iUnit, rec=thisrec+1, err=9010) tmpInt1(1)
      !!read(unit=iUnit, rec=thisrec+2, err=9010) tmpInt1(2)
      !!! increment record by 2 bytes
      !!thisrec = thisrec + 2_8



   close(iUnit)
contains

subroutine TIFF_readHeader(iUnit, head)
   implicit none

   integer,           intent(in) :: iUnit
   type(tiff_header), intent(inout) :: head

   character(len=1) :: cha_1(2)
   integer (kind=1) :: int_1(6)

   print*,'  Reading 8-byte header..'
   read(unit=iUnit, rec=1) cha_1(1)! endianness        
   read(unit=iUnit, rec=2) cha_1(2)! endianness        
   read(unit=iUnit, rec=3) int_1(1)! magic number (42) 
   read(unit=iUnit, rec=4) int_1(2)! magic number (42) 
   read(unit=iUnit, rec=5) int_1(3)! IFD offset (1st byte) 
   read(unit=iUnit, rec=6) int_1(4)! IFD offset (2nd byte) 
   read(unit=iUnit, rec=7) int_1(5)! IFD offset (3rd byte) 
   read(unit=iUnit, rec=8) int_1(6)! IFD offset (4th byte)  
   !! ---------------------------------------------------------------------
   head%byteOrder = transfer([cha_1(1),cha_1(2)]                     , head%byteOrder)
   head%magic_num = transfer([int_1(1),int_1(2)]                     , head%magic_num)
   head%offset    = transfer([int_1(3), int_1(4), int_1(5), int_1(6)], head%offset   )

end subroutine


function cpuEnd()
   ! Determines the endian-ess of the CPU
   implicit none
   character (len=2)  :: CpuEnd
   integer  (kind=2)   :: i2=1
   !i2 = 1
   if (btest(i2,0)) then
      cpuend = 'II'             !(L) little-endian
   elseif (btest(i2,8)) then
      cpuend = 'MM'             !(B)    big-endian
   else
      cpuend = 'XX'             !Something else.. Error?
   end if
   return
end function  

end program
