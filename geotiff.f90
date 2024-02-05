program test_tiff

  implicit none
  !INCLUDE 'table.ext'

  character (len=9) :: typeName(12) !=                     &
  ![  'byte     ', 'ascii    ', 'short    ', 'long     ',  &
  !   'rational ', 'sbyte    ', 'undefined', 'sshort   ',  &
  !   'slong    ', 'srational', 'float    ', 'double   '   ]

  integer (kind=1) :: typeSize(12) !=                   &
  !                  [  int(1,1), int(1,1), int(2,1),      &
  !                     int(4,1), int(8,1), int(1,1),      &
  !                     int(1,1), int(2,1), int(4,1),      &
  !                     int(8,1), int(4,1), int(8,1)       ]
  !TABLE:
  data typeName( 1), typeSize( 1)  /  'byte     ' ,  1 / !int(,1)  /
  data typeName( 2), typeSize( 2)  /  'ascii    ' ,  1 / !int(,1)  /
  data typeName( 3), typeSize( 3)  /  'short    ' ,  2 / !int(,1)  /
  data typeName( 4), typeSize( 4)  /  'long     ' ,  4 / !int(,1)  /
  data typeName( 5), typeSize( 5)  /  'rational ' ,  8 / !int(,1)  /
  data typeName( 6), typeSize( 6)  /  'sbyte    ' ,  1 / !int(,1)  /
  data typeName( 7), typeSize( 7)  /  'undefined' ,  1 / !int(,1)  /
  data typeName( 8), typeSize( 8)  /  'sshort   ' ,  2 / !int(,1)  /
  data typeName( 9), typeSize( 9)  /  'slong    ' ,  4 / !int(,1)  /
  data typeName(10), typeSize(10)  /  'srational' ,  8 / !int(,1)  /
  data typeName(11), typeSize(11)  /  'float    ' ,  4 / !int(,1)  /
  data typeName(12), typeSize(12)  /  'double   ' ,  8 / !int(,1)  /

  !type TIFF_header
  !   character(len=2) :: byteOrder   !II/MM
  !   integer          :: magic_num   !42
  !   integer          :: offset
  !endtype

  type TIFF_tag
      integer :: id,typ,cnt,offset !12-bytes (2,2,4,4) tag
  end type  

  type TIFF_IFD
     integer :: n_tags
     integer :: offset
     type(TIFF_TAG) :: tag(50)
     !integer, dimension(:),allocatable :: id,tagType,cnt,offset
  endtype
  
  type TIFF_FILE
    character(50)       :: path              !id of file (for open and close commands)
    integer             :: iUnit             !id of file (for open and close commands)
    character(len=2)    :: byteOrder         !header II/MM
    integer             :: magic_num,offset  !header 42; xxxx
    type (TIFF_IFD)     :: IFD(20)           !Image File Directory (IFD)
    !real               :: data(:,:,:) ! data itself
    integer             :: n_tags, n_ifds
    !integer             :: ifd_len
    !type (TIFF_tag)     :: tag_list
  endtype


  type(TIFF_file)      :: tiff
  integer :: ios !iUnit=101, 

   call TIFF_Open('test.tif',101,tiff,ios) 
   if (ios==0) then
       ![OK] Read header: (8-bytes long)
       call TIFF_readHeader(tiff)  
    
       ![OK] Read all IFDs:
       call TIFF_read_IFDs(tiff)
    

   call TIFF_Close(tiff)
   endif

contains

subroutine TIFF_Open(inpFile,iUnit,tiff,iost)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   integer        , intent(inout) :: iost
   integer        , intent(in)    :: iUnit
   character(*)   , intent(in)    :: inpFile

   tiff%path=inpFile
   tiff%iUnit=iUnit

   print*,'Opening tiff file: ',inpFile,'...'
   open(unit=tiff%iUnit, file=trim(tiff%path),form='UNFORMATTED',            &
        action='READ',status='OLD',access='DIRECT', recl=1,iostat=iost)
   
end subroutine
subroutine TIFF_Close(tiff)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   close(tiff%iUnit)
endsubroutine

subroutine TIFF_readHeader(tiff)
   implicit none
   type(TIFF_FILE), intent(inout):: tiff
   character(len=2)  :: cpu_End

   character(len=1) :: cha_1(2)
   integer (kind=1) :: int_1(6)
   integer :: iUnit 

   iUnit=tiff%iUnit

   print*,'  Reading 8-byte header..'
   read(unit=iUnit, rec=1) cha_1(1)! endianness        
   read(unit=iUnit, rec=2) cha_1(2)! endianness        
   read(unit=iUnit, rec=3) int_1(1)! magic number (42) 
   read(unit=iUnit, rec=4) int_1(2)! magic number (42) 
   read(unit=iUnit, rec=5) int_1(3)! IFD offset (1st byte) 
   read(unit=iUnit, rec=6) int_1(4)! IFD offset (2nd byte) 
   read(unit=iUnit, rec=7) int_1(5)! IFD offset (3rd byte) 
   read(unit=iUnit, rec=8) int_1(6)! IFD offset (4th byte)  
   !! -------------------------------------------------------------------------------
   tiff%byteOrder = transfer([cha_1(1),cha_1(2)]                     , tiff%byteOrder)
   tiff%magic_num = transfer([int_1(1),int_1(2)]                     , tiff%magic_num)
   tiff%offset    = transfer([int_1(3), int_1(4), int_1(5), int_1(6)], tiff%offset   )

   !CHECKS ==========================!
   !If byteOrder is diffrent to CPU byte-order change the way of reading the tiff file:
   if ( tiff%byteOrder /= cpuEnd() ) then
     print*, "SWAPING BYTE ORDER ! "
     close(iUnit)
     open(unit=tiff%iUnit, iostat=ios, file=tiff%path,form='UNFORMATTED',       &
         action='READ',status='OLD',access='DIRECT', convert="SWAP",recl=1)
   endif 
   !Check magic-number (TIFF format identifier)
   if ( tiff%magic_num /= 42 ) stop 'This is not a TIFF file..'
                                                                                                                          
   !If offset is negative change to unsigned-integer
   if (tiff%offset<0)  tiff%offset = tiff%offset + 4294967296_8  ! adjustment for 4-byte unsigned integers
   !=================================!

   print '("   Header: ",A3, I4, I12,".")',tiff%byteOrder,tiff%magic_num,tiff%offset
end subroutine


subroutine TIFF_read_IFDs(tiff)
  implicit none
  integer                       :: iUnit
  type(TIFF_FILE),intent(inout) :: tiff
  integer(kind=1) :: tmpInt(18)
  type(TIFF_IFD ) :: IFD
  integer :: i,t

  iUnit=tiff%iUnit
  IFD%offset=tiff%Offset
  tiff%n_ifds=0
  do while (IFD%offset /= 0) !!!read 12-byte IFD entry count; number of tiff tags in IFD
    
     read(unit=iUnit, rec=IFD%offset+1) tmpInt( 1) !E#      (1)
     read(unit=iUnit, rec=IFD%offset+2) tmpInt( 2) !E#      (2)

     IFD%n_tags     = transfer([tmpInt(1),tmpInt(2)], IFD%n_tags)
     if (IFD%n_tags > 50) print*,"PROBLEMA: n tags > 20!!"
     print*," N TAGS in IFD:",IFD%n_Tags
     
     do t=1,IFD%n_Tags
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+1 ) tmpInt( 3) !TagId   (1)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+2 ) tmpInt( 4) !TagId   (2)

        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+3 ) tmpInt( 5) !TagType (1)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+4 ) tmpInt( 6) !TagType (2)

        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+5 ) tmpInt( 7) !Count   (1)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+6 ) tmpInt( 8) !Count   (2)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+7 ) tmpInt( 9) !Count   (3)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+8 ) tmpInt(10) !Count   (4)

        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+9 ) tmpInt(11) !Offset  (1)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+10) tmpInt(12) !Offset  (2)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+11) tmpInt(13) !Offset  (3)
        read(unit=iUnit, rec=IFD%offset+2+(t-1)*12+12) tmpInt(14) !Offset  (4)
     
        IFD%tag(t)%Id     = transfer([tmpInt(3) ,tmpInt(4)]                        , IFD%tag(t)%Id    )   
        IFD%tag(t)%Typ    = transfer([tmpInt(5) ,tmpInt(6)]                        , IFD%tag(t)%Typ   )   
        IFD%tag(t)%Cnt    = transfer([tmpInt(7) ,tmpInt(8) ,tmpInt(9) , tmpInt(10)], IFD%tag(t)%Cnt   )   
        IFD%tag(t)%Offset = transfer([tmpInt(11),tmpInt(12),tmpInt(13), tmpInt(14)], IFD%tag(t)%Offset)   

        print '("Tag ",i2,":",i9,i10,"(",A6,")",i16,i16,".")',t,IFD%tag(t)%Id,IFD%tag(t)%Typ,trim(typeName(IFD%tag(t)%Typ)),IFD%tag(t)%Cnt, IFD%tag(t)%Offset
        tiff%n_tags=tiff%n_tags+1
     enddo

     read(unit=iUnit, rec=IFD%offset+2+IFD%n_Tags*12+1) tmpInt(15) !Offset  (1)
     read(unit=iUnit, rec=IFD%offset+2+IFD%n_Tags*12+2) tmpInt(16) !Offset  (2)
     read(unit=iUnit, rec=IFD%offset+2+IFD%n_Tags*12+3) tmpInt(17) !Offset  (3)
     read(unit=iUnit, rec=IFD%offset+2+IFD%n_Tags*12+4) tmpInt(18) !Offset  (4)

     tiff%IFD(tiff%n_ifds) =  IFD

     IFD%Offset    = transfer([tmpInt(15),tmpInt(16),tmpInt(17), tmpInt(18)], IFD%Offset)
     !print*,"IFD offset:",IFD%Offset

     tiff%n_ifds=tiff%n_ifds+1
  end do


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


!!function TIFF_get_field(tiff,id)
!!   implicit none
!!   type(TIFF_FILE),intent(in) :: tiff
!!   integer        ,intent(in) :: id
!!   integer         :: i,t
!!   logical         :: found=.false.
!!   do i=1,tiff%n_ifds
!!      do t=1,tiff%IFD%n_tags
!!
!!          if ( tiff%IFD(i)%tag(t)%Id == id ) then
!!               call TIFF_get_value(tag)
!!          else
!!
!!          endif
!!      enddo
!!   enddo
!!end function
!!
!!
!!subroutine TIFF_get_value(tag)
!!   implicit none
!!   type(TIFF_TAG) :: tag
!!
!!
!!
!!    
!!     read(unit=iUnit, rec=IFD%offset+2+IFD%n_Tags*12+1) tmpInt(15) !Offset  (1)
!!
!!
!!
!!
!!
!!end subroutine









!  ImageWidth        =256   
!  ImageLength       =257  
!  BitsPerSample     =258  
!  Compression       =259  
!  StripOffsets      =273  
!  Orientation       =274  
!  SamplesPerPixel   =277  
!  RowsPerStrip      =278  
!  TileWidth         =322  
!  TileLength        =323  
!  TileOffsets       =324  
!  SampleFormat      =339  
!  ModelPixelScale   =33550
!  ModelTiePoint     =33922
!  
!  GTModelType       =1024  !geoTiff
!  GTRasterType      =1025 
!  GeographicType    =2048 
!  GeogGeodeticDatum =2050 
!  GeogEllipsoid     =2056 
!  ProjectedCSType   =3072 
!  ProjCoordTrans    =3075 
!  ProjLinearUnits   =3076 
!  ProjStdParallel1  =3078 
!  ProjStdParallel2  =3079 
!  ProjNatOriginLong =3080 
!  ProjNatOriginLat  =3081 
!  ProjFalseEasting  =3082 
!  ProjFalseNorthing =3083 
!  ProjCenterLong    =3088 
!  ProjCenterLat     =3089 



end program
