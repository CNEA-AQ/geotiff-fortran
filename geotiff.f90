module GeoTIFF
  implicit none
  !save
  !private
  !public TIFF_Open
  !public TIFF_Close
  !public TIFF_GET_FIELD
  !public TIFF_FILE!,TIFF_IFD,TIFF_TAG
  !INCLUDE 'table.ext'

  character (len=9) :: typeName(12)
  integer           :: typeSize(12)
  data typeName( 1), typeSize( 1)  /  'byte     ' ,  1 / 
  data typeName( 2), typeSize( 2)  /  'ascii    ' ,  1 / 
  data typeName( 3), typeSize( 3)  /  'short    ' ,  2 / 
  data typeName( 4), typeSize( 4)  /  'long     ' ,  4 / 
  data typeName( 5), typeSize( 5)  /  'rational ' ,  8 / 
  data typeName( 6), typeSize( 6)  /  'sbyte    ' ,  1 / 
  data typeName( 7), typeSize( 7)  /  'undefined' ,  1 / 
  data typeName( 8), typeSize( 8)  /  'sshort   ' ,  2 / 
  data typeName( 9), typeSize( 9)  /  'slong    ' ,  4 / 
  data typeName(10), typeSize(10)  /  'srational' ,  8 / 
  data typeName(11), typeSize(11)  /  'float    ' ,  4 / 
  data typeName(12), typeSize(12)  /  'double   ' ,  8 / 

  type TIFF_TAG
      integer         :: Id,Typ,Cnt,Offset  !12-bytes (2,2,4,4) tag
  end type  

  type TIFF_IFD !Image File Directory
     integer            :: n_tags
     type(TIFF_TAG)     :: tag(20)
     integer            :: offset
     !type(TIFF_TAG), allocatable :: tag(:) !tag(50)
  end type
  
  type TIFF_FILE
    integer(kind=4)     :: iUnit             !id of file (for open and close commands)
    character(50)       :: path              !path to file

    character(len=2)    :: byteOrder         !header II/MM
    integer             :: magic_num         !42
    integer             :: offset            !offset (location from begining in bytes) of 1st IFD
        
    type (TIFF_IFD)     :: IFD(5)            !Image File Directory (IFD) list
    integer             :: n_tags, n_imgs
    !real               :: imgs(n_imgs,width,length) ! data itself
  end type

contains

subroutine TIFF_Open(inpFile,iUnit,tiff,iost)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   integer        , intent(inout) :: iost
   integer        , intent(in)    :: iUnit
   character(*)   , intent(in)    :: inpFile

   tiff%path=inpFile
   tiff%iUnit=iUnit

   print'("Opening tiff file: ",A20)',inpFile
   open(unit=tiff%iUnit, file=trim(tiff%path), form='UNFORMATTED', &
        action='READ',status='OLD',access='DIRECT', recl=1,iostat=iost)

   ![OK] Read header: (8-bytes long) 
   call TIFF_read_Header(tiff)  
   ![OK] Read all IFDs:
   call TIFF_read_IFDs(tiff)
   ![  ] Read GeoDir
   !call GeoTIFF_read_GeoDir(tiff)
end subroutine

subroutine TIFF_Close(tiff)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   close(tiff%iUnit)
endsubroutine

subroutine TIFF_read_Header(tiff)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   character(len=1) :: cha_1(2)
   integer (kind=1) :: int_1(6)

   print*,'  Reading 8-byte header..'
   read(unit=tiff%iUnit, rec=1) cha_1(1)! endianness        
   read(unit=tiff%iUnit, rec=2) cha_1(2)! endianness        
   read(unit=tiff%iUnit, rec=3) int_1(1)! magic number (42) 
   read(unit=tiff%iUnit, rec=4) int_1(2)! magic number (42) 
   read(unit=tiff%iUnit, rec=5) int_1(3)! IFD offset (1st byte) 
   read(unit=tiff%iUnit, rec=6) int_1(4)! IFD offset (2nd byte) 
   read(unit=tiff%iUnit, rec=7) int_1(5)! IFD offset (3rd byte) 
   read(unit=tiff%iUnit, rec=8) int_1(6)! IFD offset (4th byte)  
   !! -------------------------------------------------------------------------------
   tiff%byteOrder = transfer([cha_1(1), cha_1(2)], tiff%byteOrder)
   tiff%magic_num = transfer([int_1(1), int_1(2)], tiff%magic_num)
   tiff%offset    = transfer([int_1(3), int_1(4), int_1(5), int_1(6)], tiff%offset   )

   !CHECKS ==========================!
   !If byteOrder is diffrent to CPU byte-order change the way of reading the tiff file:
   if ( tiff%byteOrder /= cpuEnd() ) then
     print*, "SWAPING BYTE ORDER ! "
     close(tiff%iUnit)
     open(unit=tiff%iUnit, file=trim(tiff%path),form='UNFORMATTED',       &
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
  type(TIFF_FILE), intent(inout) :: tiff
  type(TIFF_IFD ) :: IFD
  integer(kind=1) :: int_1(18)=0
  integer         :: t

  IFD%offset=tiff%offset
  tiff%n_imgs=1
  do while (IFD%offset /= 0) !
     print '("   Image File Directory (IFD): ",I6)', tiff%n_imgs

     read(unit=tiff%iUnit, rec=IFD%offset+1) int_1( 1) !E#      (1)
     read(unit=tiff%iUnit, rec=IFD%offset+2) int_1( 2) !E#      (2)
     IFD%n_tags = transfer([int_1(1),int_1(2)], IFD%n_tags)

     print '("    # of Tags in IFD: ",I6)', IFD%n_Tags !,size(IFD%tag)
     do t=1,IFD%n_tags
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+1 ) int_1(3)  !TagId   (1)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+2 ) int_1(4)  !TagId   (2)

        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+3 ) int_1(5)  !TagType (1)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+4 ) int_1(6)  !TagType (2)

        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+5 ) int_1(7)  !Count   (1)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+6 ) int_1(8)  !Count   (2)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+7 ) int_1(9)  !Count   (3)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+8 ) int_1(10) !Count   (4)

        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+9 ) int_1(11) !Offset  (1)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+10) int_1(12) !Offset  (2)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+11) int_1(13) !Offset  (3)
        read(unit=tiff%iUnit, rec=IFD%offset+2+(t-1)*12+12) int_1(14) !Offset  (4)
     
        IFD%tag(t)%Id     = transfer([int_1(3) ,int_1(4)]                      , IFD%tag(t)%Id    )
        IFD%tag(t)%Typ    = transfer([int_1(5) ,int_1(6)]                      , IFD%tag(t)%Typ   )
        IFD%tag(t)%Cnt    = transfer([int_1(7) ,int_1(8) ,int_1(9) , int_1(10)], IFD%tag(t)%Cnt   )
        IFD%tag(t)%Offset = transfer([int_1(11),int_1(12),int_1(13), int_1(14)], IFD%tag(t)%Offset)
       print '("    Tag ",i2,":",i9," (",A16,")",i10," (",A6,")",i16,i16,".")',  t,IFD%tag(t)%Id,tagName(IFD%tag(t)%Id),IFD%tag(t)%Typ,trim(typeName(IFD%tag(t)%Typ)),IFD%tag(t)%Cnt, IFD%tag(t)%Offset
     enddo
     tiff%n_tags=tiff%n_tags+IFD%n_tags

     read(unit=tiff%iUnit, rec=IFD%offset+2+IFD%n_Tags*12+1) int_1(15) !Offset  (1)
     read(unit=tiff%iUnit, rec=IFD%offset+2+IFD%n_Tags*12+2) int_1(16) !Offset  (2)
     read(unit=tiff%iUnit, rec=IFD%offset+2+IFD%n_Tags*12+3) int_1(17) !Offset  (3)
     read(unit=tiff%iUnit, rec=IFD%offset+2+IFD%n_Tags*12+4) int_1(18) !Offset  (4)

     tiff%IFD(tiff%n_imgs) =  IFD

     IFD%Offset    = transfer([int_1(15),int_1(16),int_1(17), int_1(18)], IFD%Offset)
     print '("   IFD offset: ",i3)',IFD%Offset

     tiff%n_imgs=tiff%n_imgs+1
  end do

end subroutine

!subroutine GeoTIFF_read_GeoDir()
!
!endsubroutine


!subroutine TIFF_GET_FIELD(tiff,tagId) !values)
!   implicit none
!   type(TIFF_FILE),intent(in) :: tiff
!   integer        ,intent(in) :: tagId
!   integer         :: i,t,c,s     !loop indices
!   logical         :: found=.false.
!   integer         :: siz,cnt,off !size,count,offset
!
!   integer,         allocatable   :: values(:) !hole array value "cnt"-elements long
!   integer(kind=1), allocatable   ::  val_1(:) !unitary value    "siz"-bytes    long
!
!   !search idTag:
!   do i=1,tiff%n_imgs
!      do t=1,tiff%IFD(i)%n_tags
!          if ( tiff%IFD(i)%tag(t)%Id == tagId ) then
!
!               off=tiff%IFD(i)%tag(t)%offset
!               siz=typeSize(tiff%IFD(i)%tag(t)%typ)
!               cnt=tiff%IFD(i)%tag(t)%cnt
!
!               allocate(values(cnt))
!               allocate( val_1(siz))
!
!               if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
!                  values= off
!               else
!                    do c=1,cnt
!                       do s=1,siz
!                         read(unit=tiff%iUnit, rec=off+(c-1)*siz+s ) val_1(s)
!                         !print*,val(s)
!                       end do
!                       values(c)=transfer(val_1,values(c))
!                    end do
!               endif
!               print*,values
!               found=.true.
!               return
!          endif
!      enddo
!   enddo
!   if (.not. found) print '("Error: TagId not found:",I5)',tagId
!
!end subroutine

!subroutine get_value(iUnit,off,siz,cnt,val)
!    implicit none
!    integer,intent(in) :: iUnit
!    integer,intent(in) :: off,siz,cnt
!    integer,intent(inout) :: val(:)
!    integer, allocatable :: tmp(:)
!    integer :: c,s
!    allocate(tmp(siz))
!    do c=1,cnt
!       do s=1,siz
!         print*,off,c,s,off+c*s
!         read(unit=iUnit, rec=off+c*s-1 ) tmp(s)
!       end do
!       val(c) = transfer(tmp(:),val(c))
!    end do
!
!end subroutine


function tagName(tagId)
  implicit none
  integer,intent(in) :: tagId
  character(len=20)  :: tagName
  select case (tagId)
     case (256 )  ;tagName='ImageWidth       '    
     case (257 )  ;tagName='ImageLength      '
     case (258 )  ;tagName='BitsPerSample    '
     case (259 )  ;tagName='Compression      '
     case (273 )  ;tagName='StripOffsets     '
     case (274 )  ;tagName='Orientation      '
     case (277 )  ;tagName='SamplesPerPixel  '
     case (278 )  ;tagName='RowsPerStrip     '
     case (322 )  ;tagName='TileWidth        '
     case (323 )  ;tagName='TileLength       '
     case (324 )  ;tagName='TileOffsets      '
     case (339 )  ;tagName='SampleFormat     '
     case (3355)  ;tagName='ModelPixelScale  '
     case (3392)  ;tagName='ModelTiePoint    '
     
     case (1024)  ;tagName='GTModelType      '
     case (1025)  ;tagName='GTRasterType     '
     case (2048)  ;tagName='GeographicType   '
     case (2050)  ;tagName='GeogGeodeticDatum'
     case (2056)  ;tagName='GeogEllipsoid    '
     case (3072)  ;tagName='ProjectedCSType  '
     case (3075)  ;tagName='ProjCoordTrans   '
     case (3076)  ;tagName='ProjLinearUnits  '
     case (3078)  ;tagName='ProjStdParallel1 '
     case (3079)  ;tagName='ProjStdParallel2 '
     case (3080)  ;tagName='ProjNatOriginLong'
     case (3081)  ;tagName='ProjNatOriginLat '
     case (3082)  ;tagName='ProjFalseEasting '
     case (3083)  ;tagName='ProjFalseNorthing'
     case (3088)  ;tagName='ProjCenterLong   '
     case (3089)  ;tagName='ProjCenterLat    '
     case default ;tagName='IdontKnow        '
  end select
end function

function cpuEnd()
   ! Determines the endian-ess of the CPU
   implicit none
   character (len=2)  :: CpuEnd
   integer  (kind=2)   :: i2=1
   if (btest(i2,0)) then
      cpuend = 'II'             !(L) little-endian
   elseif (btest(i2,8)) then
      cpuend = 'MM'             !(B)    big-endian
   else
      cpuend = 'XX'             !Something else.. Error?
   end if
   return
end function  

end module  
