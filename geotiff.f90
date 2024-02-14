module GeoTIFF
  implicit none
  !save
  !private
  !public TIFF_Open
  !public TIFF_Close
  !public TIFF_GET_TAG_VALUE
  !public TIFF_FILE,TIFF_IFD,TIFF_TAG

  interface TIFF_GET_TAG_VALUE
          module procedure get_field_single_int, get_field_array_int, get_field_single_float, & 
          get_field_array_float,get_field_array_char
  end interface TIFF_GET_TAG_VALUE

  !List of supported TIFF Tags (and it's TagID)
  integer :: TIFF_ImageWidth         =256
  integer :: TIFF_ImageLength        =257
  integer :: TIFF_BitsPerSample      =258
  integer :: TIFF_Compression        =259
  integer :: TIFF_PhotometricInt     =262
  integer :: TIFF_Threshholding      =263
  integer :: TIFF_CellWidth          =264
  integer :: TIFF_CellLength         =265
  integer :: TIFF_FillOrder          =266
  integer :: TIFF_ImageDescription   =270
  integer :: TIFF_StripOffsets       =273 !Strip Images
  integer :: TIFF_Orientation        =274 !orientation - default value is 1 (0,0 = northwest corner)
  integer :: TIFF_SamplesPerPixel    =277
  integer :: TIFF_RowsPerStrip       =278 !Strip Images
  integer :: TIFF_StripByteCounts    =279 !Strip Images
  integer :: TIFF_MinSampleValue     =280
  integer :: TIFF_MaxSampleValue     =281
  integer :: TIFF_XResolution        =282
  integer :: TIFF_YResolution        =283
  integer :: TIFF_PlanarConfiguration=284
  integer :: TIFF_FreeOffsets        =288
  integer :: TIFF_FreeByteCounts     =289
  integer :: TIFF_GrayResponseUn     =290
  integer :: TIFF_GrayResponseCu     =291
  integer :: TIFF_ResolutionUnit     =296
  integer :: TIFF_Software           =305
  integer :: TIFF_DateTime           =306
  integer :: TIFF_HostComputer       =316
  integer :: TIFF_ColorMap           =320
  integer :: TIFF_TileWidth          =322  !Tiled Images  
  integer :: TIFF_TileLength         =323  !Tiled Images
  integer :: TIFF_TileOffsets        =324  !Tiled Images
  integer :: TIFF_TileByteCounts     =325  !Tiled Images
  integer :: TIFF_ExtraSamples       =338
  integer :: TIFF_SampleFormat       =339 

  integer :: GTIFF_GeoKeyDirectoryTag    = 34735 !(mandatory)
  integer :: GTIFF_GeoDoubleParamsTag    = 34736 !(optional)
  integer :: GTIFF_GeoAsciiParamsTag     = 34737 !(optional)
  integer :: GTIFF_ModelPixelScaleTag    = 33550 !(optional)
  integer :: GTIFF_ModelTiepointTag      = 33922 !(conditional)
  integer :: GTIFF_ModelTransformationTag= 34264 !(conditional)
  !The conditional tags shall follow the following rules:
  !- One of ModelTiepointTag or ModelTransformationTag SHALL be included in an Image File Directory (IFD)
  !- If the ModelTransformationTag is included in an IFD, then a ModelPixelScaleTag SHALL NOT be included
  !- If the ModelPixelScaleTag is included in an IFD, then a ModelTiepointTag SHALL also be included.

  integer :: GKey_GTModelType             =1024 
  integer :: GKey_GTRasterType            =1025 
  integer :: GKey_GTCitation              =1026 
  integer :: GKey_GeographicType          =2048 
  integer :: GKey_GeogCitation            =2049 
  integer :: GKey_GeogGeodeticDatum       =2050 
  integer :: GKey_GeogPrimeMeridian       =2051 
  integer :: GKey_GeogLinearUnits         =2052 
  integer :: GKey_GeogLinearUnitSize      =2053 
  integer :: GKey_GeogAngularUnits        =2054 
  integer :: GKey_GeogAngularUnitSize     =2055 
  integer :: GKey_GeogEllipsoid           =2056 
  integer :: GKey_GeogSemiMajorAxis       =2057 
  integer :: GKey_GeogSemiMinorAxis       =2058 
  integer :: GKey_GeogInvFlattening       =2059 
  integer :: GKey_GeogAzimuthUnits        =2060 
  integer :: GKey_GeogPrimeMeridianLong   =2061 
  integer :: GKey_ProjectedCSType         =3072  !EPSG!
  integer :: GKey_PCSCitation             =3073 
  integer :: GKey_Projection              =3074 
  integer :: GKey_ProjCoordTrans          =3075 
  integer :: GKey_ProjLinearUnits         =3076 
  integer :: GKey_ProjLinearUnitSize      =3077 
  integer :: GKey_ProjStdParallel1        =3078 
  integer :: GKey_ProjStdParallel2        =3079 
  integer :: GKey_ProjNatOriginLong       =3080 
  integer :: GKey_ProjNatOriginLat        =3081 
  integer :: GKey_ProjFalseEasting        =3082 
  integer :: GKey_ProjFalseNorthing       =3083 
  integer :: GKey_ProjFalseOriginLong     =3084 
  integer :: GKey_ProjFalseOriginLat      =3085 
  integer :: GKey_ProjFalseOriginEasting  =3086 
  integer :: GKey_ProjFalseOriginNorthing =3087 
  integer :: GKey_ProjCenterLong          =3088 
  integer :: GKey_ProjCenterLat           =3089 
  integer :: GKey_ProjCenterEasting       =3090 
  integer :: GKey_ProjCenterNorthing      =3091 
  integer :: GKey_ProjScaleAtNatOrigin    =3092 
  integer :: GKey_ProjScaleAtCenter       =3093 
  integer :: GKey_ProjAzimuthAngle        =3094 
  integer :: GKey_ProjStraightVertPoleLong=3095 
  integer :: GKey_VerticalCSType          =4096 
  integer :: GKey_VerticalCitation        =4097 
  integer :: GKey_VerticalDatum           =4096 
  integer :: GKey_VerticalUnits           =4099 


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

  integer (kind=8), parameter :: intAdj4 = 4294967296_8  ! adjustment for 4-byte unsigned integers
  integer (kind=4), parameter :: intAdj2 = 65536         ! adjustment for 2-byte unsigned integers
  integer (kind=2), parameter :: intAdj1 = 256           ! adjustment for 1-byte unsigned integers

  !GeoTIFF Dir and Keys:  --------------------
  type Geo_KEY 
      integer         :: Id,typ,Cnt,Offset  !
  end type  
  
  type GEO_DIR
      integer                    :: nKeys,version,revision,minor_revision
      type(Geo_Key), allocatable :: Key(:)  
  end type
  !-------------------------------------------
  type TIFF_TAG
      integer         :: Id,Typ,Cnt,Offset  !12-bytes (2,2,4,4) tag
  end type  
  type TIFF_IFD                     !Image File Directory
     integer            :: n_tags   ! # of Tags in this IFD            !2-bytes
     type(TIFF_TAG)     :: tag(20)  ! [tagId tagTyp tagCnt tagOffset]  !12-bytes [2,2,4,4]
     integer            :: offset   ! next IFD offset or 0 (end IFD)   !4-bytes
  end type
  type TIFF_FILE                      !Representation of TIFF file
     integer(kind=4)    :: iUnit      ! id of file (for open and close commands)
     character(50)      :: path       ! path to file
                                      ! Header
     character(len=2)   :: byteOrder  !  II/MM
     integer            :: magic_num  !  42
     integer            :: offset     !  offset (location from begining in bytes) of 1st IFD
         
     type (TIFF_IFD)    :: IFD(5)     ! Image File Directory (IFD) list
     integer            :: n_tags, n_imgs !total number of tags, total number of ifds
     !real              :: imgs(n_imgs,width,length) ! data itself
     
     type (GEO_DIR)     :: gDir
     !extra parameters:
     logical            :: swapByte=.false.
     integer            :: bitsPerSample,samplesPerPixel

     character(5)       :: ImgType                      !'strip' / 'tile'
  end type

contains

subroutine TIFF_Open(iUnit,inpFile,action,tiff,iost)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   integer        , intent(inout) :: iost
   integer        , intent(in)    :: iUnit
   character(*)   , intent(in)    :: inpFile
   character(1)   , intent(in)    :: action  !'r' or 'w'

   tiff%path=inpFile
   tiff%iUnit=iUnit

   select case (action)
     case ('r','R','read','READ','Read')
        print'("Opening tiff file: ",A20)',inpFile
        open(unit=tiff%iUnit, file=trim(tiff%path), form='UNFORMATTED', &
             action='READ',status='OLD',access='DIRECT', recl=1,iostat=iost)

        ![OK] Read header: (8-bytes long) 
        call TIFF_read_Header(tiff)  
        ![OK] Read all IFDs and them corresponding tags parameters
        call TIFF_read_IFDs(tiff)
        
        ![  ] Read GeoKeys from GeoDir
        if ( gotTag(tiff, GTIFF_GeoKeyDirectoryTag ) ) then 
            print*," GeoTIFF file!"
            call GTIFF_read_GeoDir(tiff)
        endif
        call TIFF_GET_TAG_VALUE(tiff, TIFF_BitsPerSample  , tiff%bitsPerSample  )
        call TIFF_GET_TAG_VALUE(tiff, TIFF_SamplesPerPixel, tiff%samplesPerPixel)

        if ( gotTag(tiff, TIFF_TileOffsets ) ) then 
           print*, " Tiled type!"; tiff%ImgType='tile'
        else if ( gotTag(tiff, TIFF_StripOffsets) ) then
           print*, " Strip type!"; tiff%ImgType='strip'
        else

        end if
     case ('w','W','write','WRITE','Write')
        
         
        
     case default
      
   end select
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
     tiff%swapByte=.true.
   endif 
   !Check magic-number (TIFF format identifier)
   if ( tiff%magic_num /= 42 ) stop 'This is not a TIFF file..'

   !If offset is negative change to unsigned-integer
   if (tiff%offset<0)  tiff%offset = tiff%offset + intAdj4       ! adjustment for 4-byte unsigned integers
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
        if ( IFD%tag(t)%Cnt    < 0 )  IFD%tag(t)%Cnt   =IFD%tag(t)%Cnt    +  intAdj4
        if ( IFD%tag(t)%Offset < 0 )  IFD%tag(t)%Offset=IFD%tag(t)%Offset +  intAdj4
        print '("    Tag ",i2,":",i9," (",A22,")",i10," (",A6,")",i12,i12,".")', t,IFD%tag(t)%Id,tagName(IFD%tag(t)%Id),IFD%tag(t)%Typ,trim(typeName(IFD%tag(t)%Typ)),IFD%tag(t)%Cnt, IFD%tag(t)%Offset
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

! Read GeoTIFF directory ===========================================
subroutine GTIFF_read_GeoDir(tiff)
      implicit none
      type(TIFF_FILE),intent(inout) :: tiff
      integer         :: gDirOffset, k,typ,cnt
      logical         :: found
      integer(kind=1) :: Head(8), key(8)

      print '("   GeoTIFF Keys Directory (geoDir): ",I6)'
      ! set record number
      call get_tag_parameters(tiff,GTIFF_geoKeyDirectoryTag,typ,cnt,gDirOffset,found)
      read(unit=tiff%iUnit, rec=gDirOffset+1) Head(1)  ! version  - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+2) Head(2)  ! version  - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+3) Head(3)  ! revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+4) Head(4)  ! revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+5) Head(5)  ! minor revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+6) Head(6)  ! minor revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+7) Head(7)  ! number of GeoKeys (1)
      read(unit=tiff%iUnit, rec=gDirOffset+8) Head(8)  ! number of GeoKeys (2)
      tiff%gDir%nkeys = transfer( [head(1), head(2)], tiff%gDir%version       )
      tiff%gDir%nkeys = transfer( [head(3), head(4)], tiff%gDir%revision      )
      tiff%gDir%nkeys = transfer( [head(5), head(6)], tiff%gDir%minor_revision)
      tiff%gDir%nkeys = transfer( [head(7), head(8)], tiff%gDir%nKeys         )
      allocate(tiff%gDir%key(tiff%gDir%nKeys))
      print '("version: ",I2, "revision: ",I2," minor-revision: ",I2," # keys: ",I2)',tiff%gDir%version, tiff%gDir%revision,tiff%gDir%minor_revision,tiff%gDir%nKeys
      print '("    # of Keys in gDir: ",I6)', tiff%gDir%nKeys
      do k=1,tiff%gDir%nkeys
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+1) Key(1) !id
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+2) Key(2) !id
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+3) Key(3) !tiffTagLocation (kind)
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+4) Key(4) !tiffTagLocation (kind)
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+5) Key(5) !count
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+6) Key(6) !count
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+7) Key(7) !offset/value
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+8) Key(8) !offset/value

         tiff%gDir%key(k)%id    = transfer([ key(1),key(2) ],   tiff%gDir%key(k)%id    ) 
         tiff%gDir%key(k)%typ   = transfer([ key(3),key(4) ],   tiff%gDir%key(k)%typ   )
         tiff%gDir%key(k)%cnt   = transfer([ key(5),key(6) ],   tiff%gDir%key(k)%cnt   )
         tiff%gDir%key(k)%offset= transfer([ key(7),key(8) ],   tiff%gDir%key(k)%offset)

         if ( tiff%gDir%key(k)%Id     < 0 )  tiff%gDir%key(k)%Id    = tiff%gDir%key(k)%Id     +  intAdj4
         if ( tiff%gDir%key(k)%Cnt    < 0 )  tiff%gDir%key(k)%Cnt   = tiff%gDir%key(k)%Cnt    +  intAdj4
         if ( tiff%gDir%key(k)%Offset < 0 )  tiff%gDir%key(k)%Offset= tiff%gDir%key(k)%Offset +  intAdj4

         print '("    Key ",i2,":",i9," (",A22,")",i10," (",A6,")", i12,i12,".")',k, &
                           tiff%gDir%key(k)%Id,                                      &
                           keyName(tiff%gDir%key(k)%Id),                             &
                           tiff%gDir%key(k)%Typ,                                     &
                           keyTypeName(tiff%gDir%key(k)%Typ),                        &
                           int(tiff%gDir%key(k)%Cnt),                                     &
                           tiff%gDir%key(k)%Offset       

      enddo

end subroutine


!=== TIFF_GET_TAG_VALUE  ====================
subroutine get_field_single_int(tiff,tagId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: tagId
   integer        , intent(inout)  :: val
   integer                         :: tmp_arr(1)
   tmp_arr(1)= val
   call get_field_array_int(tiff,tagId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_field_single_float(tiff,tagId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: tagId
   real           , intent(inout)  :: val
   real                            :: tmp_arr(1)
   tmp_arr(1)= val
   call get_field_array_float(tiff,tagId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_field_array_int(tiff,tagId,values)  !for INTEGERs
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: tagId
   integer        , intent(inout)  :: values(:)
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=off
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            values(c)=transfer(values_1(1+(c-1)*siz:c*siz), values(1))
            if ( values(c) < 0) then
                if ( typ == 3 ) values(c)=values(c)+intAdj2   !short (2-bytes) unsigned
                if ( typ == 4 ) values(c)=values(c)+intAdj4   !long  (4-bytes) unsigned
            endif
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine

subroutine get_field_array_float(tiff,tagId,values)  !for REAL (float)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: tagId
   real           , intent(inout)  :: values(:)
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=real(off)
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            values(c)=transfer(values_1(1+(c-1)*siz:c*siz), values(1))
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine

subroutine get_field_array_char(tiff,tagId,values)  !for REAL (float)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: tagId
   character(*)   , intent(inout)  :: values
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=char(off)
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            values(c:c)=transfer(values_1(1+(c-1)*siz:c*siz), values(1:1))
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine

subroutine get_tag_parameters(tiff,tagId,typ,cnt,off,found)
   implicit none
   type(TIFF_FILE),intent(in)    :: tiff
   integer        ,intent(in)    :: tagId
   integer        ,intent(inout) :: typ,cnt,off
   logical        ,intent(inout) :: found!=.false.
   integer :: i,t  !loop indices

   do i=1,tiff%n_imgs
      do t=1,tiff%IFD(i)%n_tags
          if ( tiff%IFD(i)%tag(t)%Id == tagId ) then
             typ=tiff%IFD(i)%tag(t)%typ
             cnt=tiff%IFD(i)%tag(t)%cnt
             off=tiff%IFD(i)%tag(t)%offset
             found=.true.
             return
          endif
      enddo
   enddo
   if (.not. found) print '("Error: TagId not found:",I5," (",A,")")',tagId,trim(tagName(tagId))

end subroutine

subroutine get_field_as_byte_array(tiff,offset,values_1)!,siz,cnt
  implicit none
  type(TIFF_FILE),intent(in)     :: tiff
  integer        ,intent(in)     :: offset
  integer(kind=1),intent(inout)  :: values_1(:) !1-byte elements array
  integer :: i
  do i=1,size(values_1)
        read(unit=tiff%iUnit, rec=offset+1+i-1 ) values_1(i)
  enddo
end subroutine
!=== END TIFF_GET_TAG_VALUE =============

 

!MISC Functions =====================
logical function gotTag(tiff,tagId)
   implicit none
   type(TIFF_FILE), intent(in) ::tiff
   integer        , intent(in) ::tagId
   integer :: i,t
   gotTag=.false.
   do i=1,tiff%n_imgs
      do t=1,tiff%IFD(i)%n_tags
          if ( tiff%IFD(i)%tag(t)%Id == tagId ) then
             gotTag=.true.
             return
          endif
      enddo
   enddo
end function


character(len=22) function tagName(tagId)
  implicit none
  integer,intent(in) :: tagId
  select case (tagId)
     case(256 )   ;tagName='ImageWidth            '
     case(257 )   ;tagName='ImageLength           '
     case(258 )   ;tagName='BitsPerSample         '
     case(259 )   ;tagName='Compression           '
     case(262 )   ;tagName='PhotometricInt        '
     case(263 )   ;tagName='Threshholding         '
     case(264 )   ;tagName='CellWidth             '
     case(265 )   ;tagName='CellLength            '
     case(266 )   ;tagName='FillOrder             '
     case(270 )   ;tagName='ImageDescription      '
     case(271 )   ;tagName='Make                  '
     case(272 )   ;tagName='Model                 '
     case(273 )   ;tagName='StripOffsets          '
     case(274 )   ;tagName='Orientation           '
     case(277 )   ;tagName='SamplesPerPixel       '
     case(278 )   ;tagName='RowsPerStrip          '
     case(279 )   ;tagName='StripByteCounts       '
     case(280 )   ;tagName='MinSampleValue        '
     case(281 )   ;tagName='MaxSampleValue        '
     case(282 )   ;tagName='XResolution           '
     case(283 )   ;tagName='YResolution           '
     case(284 )   ;tagName='PlanarConfig          ' !PlanarConfiguration
     case(306 )   ;tagName='DateTime              '
     case(322 )   ;tagName='TileWidth             '
     case(323 )   ;tagName='TileLength            '
     case(324 )   ;tagName='TileOffsets           '
     case(339 )   ;tagName='SampleFormat          '
     case(3355)   ;tagName='ModelPixelScale       '
     case(3392)   ;tagName='ModelTiePoint         '
     case(34735)  ;tagName='GeoKeyDirectoryTag    ' !GeoTiff-tag
     case(34736)  ;tagName='GeoDoubleParamsTag    ' !GeoTiff-tag
     case(34737)  ;tagName='GeoAsciiParamsTag     ' !GeoTiff-tag
     case(33550)  ;tagName='ModelPixelScaleTag    ' !GeoTiff-tag
     case(33922)  ;tagName='ModelTiepointTag      ' !GeoTiff-tag
     case(34264)  ;tagName='ModelTransformationTag' !GeoTiff-tag
     case default ;tagName='Not Recognized        '
  end select
end function

character(len=22) function keyName(keyId)
  implicit none
  integer,intent(in) :: keyId
  select case (keyId)
     !GeoTiff-tag
     case(34735); keyName='GeoKeyDirectoryTag       '
     case(34736); keyName='GeoDoubleParamsTag       ' 
     case(34737); keyName='GeoAsciiParamsTag        ' 
     case(33550); keyName='ModelPixelScaleTag       ' 
     case(33922); keyName='ModelTiepointTag         ' 
     case(34264); keyName='ModelTransformationTag   ' 
     !Config Keys:
     case (1024); keyName= 'GTModelType             '
     case (1025); keyName= 'GTRasterType            ' 
     case (1026); keyName= 'GTCitation              ' 
     !Geodetic CRS Params:
     case (2048); keyName= 'GeographicType          ' 
     case (2049); keyName= 'GeogCitation            '
     case (2050); keyName= 'GeogGeodeticDatum       '
     case (2051); keyName= 'GeogPrimeMeridian       '
     case (2052); keyName= 'GeogLinearUnits         '
     case (2053); keyName= 'GeogLinearUnitSize      '
     case (2054); keyName= 'GeogAngularUnits        '
     case (2055); keyName= 'GeogAngularUnitSize     '
     case (2056); keyName= 'GeogEllipsoid           '
     case (2057); keyName= 'GeogSemiMajorAxis       '
     case (2058); keyName= 'GeogSemiMinorAxis       '
     case (2059); keyName= 'GeogInvFlattening       '
     case (2060); keyName= 'GeogAzimuthUnits        '
     case (2061); keyName= 'GeogPrimeMeridianLong   '
     !Projected CRS Params: 
     case (3072); keyName= 'ProjectedCSType         ' !EPSG CODE!
     case (3073); keyName= 'PCSCitation             '
     case (3074); keyName= 'Projection              '
     case (3075); keyName= 'ProjCoordTrans          ' 
     case (3076); keyName= 'ProjLinearUnits         '
     case (3077); keyName= 'ProjLinearUnitSize      ' 
     case (3078); keyName= 'ProjStdParallel1        ' 
     case (3079); keyName= 'ProjStdParallel2        ' 
     case (3080); keyName= 'ProjNatOriginLong       ' 
     case (3081); keyName= 'ProjNatOriginLat        ' 
     case (3082); keyName= 'ProjFalseEasting        ' 
     case (3083); keyName= 'ProjFalseNorthing       ' 
     case (3084); keyName= 'ProjFalseOriginLong     ' 
     case (3085); keyName= 'ProjFalseOriginLat      ' 
     case (3086); keyName= 'ProjFalseOriginEasting  ' 
     case (3087); keyName= 'ProjFalseOriginNorthing '
     case (3088); keyName= 'ProjCenterLong          '
     case (3089); keyName= 'ProjCenterLat           '
     case (3090); keyName= 'ProjCenterEasting       '
     case (3091); keyName= 'ProjCenterNorthing      '
     case (3092); keyName= 'ProjScaleAtNatOrigin    '
     case (3093); keyName= 'ProjScaleAtCenter       '
     case (3094); keyName= 'ProjAzimuthAngle        '
     case (3095); keyName= 'ProjStraightVertPoleLong'
     case (4096); keyName= 'VerticalCSType          '  !Vertical CRS Params:
     case (4097); keyName= 'VerticalCitation        '
     case (4098); keyName= 'VerticalDatum           '
     case (4099); keyName= 'VerticalUnits           '
     case default ;keyName='Not recognized geoKey   '
  end select
end function

character(6) function keyTypeName(keyType)
  implicit none
  integer,intent(in) :: keyType
  select case (keyType)
     case (0    ) ; keyTypeName= 'short '
     case (34736) ; keyTypeName= 'double'
     case (34737) ; keyTypeName= 'ascii '
     case default; 
  end select
end function

integer function keyTypeSize(keyType)
  implicit none
  integer,intent(in) :: keyType
  select case (keyType)
     case (0    ) ; keyTypeSize= 2
     case (34736) ; keyTypeSize= 8
     case (34737) ; keyTypeSize= 1
     case default; 
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

! END MISC Functions =================



  !=== TIFF_GET_IMAGE     =============
   subroutine TIFF_GET_IMAGE(tiff,img)
      implicit none
      type(TIFF_FILE)     :: tiff
      real   , intent(inout) :: img(:)
      !integer, allocatable   :: img_i(:)
      integer :: i,j,k,b,e,recNum
      integer(kind=1), allocatable :: value_1(:)
      !if strips:
      integer, allocatable :: stripOffSets(:), stripsByteCounts(:)
      integer              :: nstrips,rps,wid,len,bytesPerSample

      bytesPerSample=tiff%bitsPerSample/8

      call TIFF_GET_TAG_VALUE(tiff, TIFF_ImageWidth     ,wid )
      call TIFF_GET_TAG_VALUE(tiff, TIFF_ImageLength    ,len )
      !allocate(img_i(size(img)))
      allocate(value_1(bytesPerSample))
      
      SELECT CASE(tiff%ImgType)
       CASE("strip")
           call TIFF_GET_TAG_VALUE(tiff, TIFF_RowsPerStrip   ,rps )
           nstrips=floor(real((len+rps-1)/rps))
           allocate(StripOffsets    (nstrips))
           allocate(StripsByteCounts(nstrips))
           call TIFF_GET_TAG_VALUE(tiff, TIFF_StripOffsets   ,StripOffsets   )
           call TIFF_GET_TAG_VALUE(tiff, TIFF_StripByteCounts,StripsByteCounts)
           e=0
           do i=1,size(stripOffsets)
              do j=1,rps
                 do k=1,wid !StripsByteCounts(i),bytesPerSample
                    do b=1,bytesPerSample
                        recNum=stripOffsets(i)+1+( (j-1)*wid + (k-1) )*bytesPerSample + b
                        read(tiff%iUnit, rec=recNum) value_1(b)
                    enddo
                 if ( e >= wid*len ) cycle 
                 e=e+1
                 img(e)   = transfer( value_1, img(1)   )
                 !print*, recNum,e,int(img(e) )
                enddo
              enddo
           enddo
   
       CASE("tile")
        ! loop over offsets array
        !TOffsets: DO idos=1_8, size(vars%dataOS)

        !  ! compute number of current tile, tile row, tile col
        !  ! first row in tile, first col in tile
        !  tileNum = tileNum+1_8
        !  tileRow = int((tileNum-1_8)/tilesAcross)+1_8
        !  tileCol = mod(tileNum-1_8,tilesAcross)+1_8
        !  tileFirstRow = ((tileRow-1_8)*vars%tileLen)+1_8
        !  tileFirstCol = ((tileCol-1_8)*vars%tileWid)+1_8

        !  ! compute current data row, decrement by 1 since incremented at 
        !  ! beginning of loop below
        !  tmprow = tileFirstRow-1_8

        !  ! loop over rows in tile
        !  TRowLoop: DO jrow=1_8, vars%tileLen
  
   
        !!       END DO TColLoop ! tileWid, cols
         !   END DO TRowLoop ! tileLen, rows
         !END DO TOffsets ! tile offsets         
   
      END SELECT
    
   end subroutine TIFF_GET_IMAGE
  !=== END TIFF_GET_IMAGE =============




!=============================================================== 





end module  
