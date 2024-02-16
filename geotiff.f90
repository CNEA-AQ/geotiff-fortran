module GeoTIFF
  implicit none
  !save
  !private
  !public TIFF_Open
  !public TIFF_Close
  !public TIFF_GET_TAG_VALUE
  !public TIFF_GET_IMAGE
  !public GTIFF_GET_KEY_VALUE
  !public TIFF_FILE,TIFF_IFD,TIFF_TAG

  interface TIFF_GET_TAG_VALUE
          module procedure get_tag_value_int, get_tag_values_int, get_tag_value_float, get_tag_values_float,get_tag_values_char
  end interface TIFF_GET_TAG_VALUE

  interface GTIFF_GET_KEY_VALUE
          module procedure get_key_value_short, get_key_values_ascii, get_key_value_double, get_key_values_double
  end interface GTIFF_GET_KEY_VALUE

  !List of supported TIFF Tags (and it's TagID)
  integer :: TIFF_ImageWidth              = 256
  integer :: TIFF_ImageLength             = 257
  integer :: TIFF_BitsPerSample           = 258
  integer :: TIFF_Compression             = 259
  integer :: TIFF_PhotometricInt          = 262
  integer :: TIFF_Threshholding           = 263
  integer :: TIFF_CellWidth               = 264
  integer :: TIFF_CellLength              = 265
  integer :: TIFF_FillOrder               = 266
  integer :: TIFF_ImageDescription        = 270
  integer :: TIFF_StripOffsets            = 273  !Striped Images
  integer :: TIFF_Orientation             = 274  !orientation - default value is 1 (0,0 = northwest corner)
  integer :: TIFF_SamplesPerPixel         = 277
  integer :: TIFF_RowsPerStrip            = 278  !Striped Images
  integer :: TIFF_StripByteCounts         = 279  !Striped Images
  integer :: TIFF_MinSampleValue          = 280
  integer :: TIFF_MaxSampleValue          = 281
  integer :: TIFF_XResolution             = 282
  integer :: TIFF_YResolution             = 283
  integer :: TIFF_PlanarConfiguration     = 284
  integer :: TIFF_FreeOffsets             = 288
  integer :: TIFF_FreeByteCounts          = 289
  integer :: TIFF_GrayResponseUn          = 290
  integer :: TIFF_GrayResponseCu          = 291
  integer :: TIFF_ResolutionUnit          = 296
  integer :: TIFF_Software                = 305
  integer :: TIFF_DateTime                = 306
  integer :: TIFF_HostComputer            = 316
  integer :: TIFF_ColorMap                = 320
  integer :: TIFF_TileWidth               = 322  !Tiled Images  
  integer :: TIFF_TileLength              = 323  !Tiled Images
  integer :: TIFF_TileOffsets             = 324  !Tiled Images
  integer :: TIFF_TileByteCounts          = 325  !Tiled Images
  integer :: TIFF_ExtraSamples            = 338
  integer :: TIFF_SampleFormat            = 339 

  integer :: GTIFF_GeoKeyDirectoryTag     = 34735 !(mandatory)
  integer :: GTIFF_GeoDoubleParamsTag     = 34736 !(optional)
  integer :: GTIFF_GeoAsciiParamsTag      = 34737 !(optional)     !The conditional tags shall follow the following rules:
  integer :: GTIFF_ModelPixelScaleTag     = 33550 !(optional)     !- One of ModelTiepointTag or ModelTransformationTag SHALL be included in an Image File Directory (IFD)  
  integer :: GTIFF_ModelTiepointTag       = 33922 !(conditional)  !- If the ModelTransformationTag is included in an IFD, then a ModelPixelScaleTag SHALL NOT be included  
  integer :: GTIFF_ModelTransformationTag = 34264 !(conditional)  !- If the ModelPixelScaleTag is included in an IFD, then a ModelTiepointTag SHALL also be included.      
                                                                    
  integer :: GKey_GTModelType             = 1024 
  integer :: GKey_GTRasterType            = 1025 
  integer :: GKey_GTCitation              = 1026 
  integer :: GKey_GeographicType          = 2048 
  integer :: GKey_GeogCitation            = 2049 
  integer :: GKey_GeogGeodeticDatum       = 2050 
  integer :: GKey_GeogPrimeMeridian       = 2051 
  integer :: GKey_GeogLinearUnits         = 2052 
  integer :: GKey_GeogLinearUnitSize      = 2053 
  integer :: GKey_GeogAngularUnits        = 2054 
  integer :: GKey_GeogAngularUnitSize     = 2055 
  integer :: GKey_GeogEllipsoid           = 2056 
  integer :: GKey_GeogSemiMajorAxis       = 2057 
  integer :: GKey_GeogSemiMinorAxis       = 2058 
  integer :: GKey_GeogInvFlattening       = 2059 
  integer :: GKey_GeogAzimuthUnits        = 2060 
  integer :: GKey_GeogPrimeMeridianLong   = 2061 
  integer :: GKey_ProjectedCSType         = 3072  !EPSG!
  integer :: GKey_PCSCitation             = 3073 
  integer :: GKey_Projection              = 3074 
  integer :: GKey_ProjCoordTrans          = 3075 
  integer :: GKey_ProjLinearUnits         = 3076 
  integer :: GKey_ProjLinearUnitSize      = 3077 
  integer :: GKey_ProjStdParallel1        = 3078 
  integer :: GKey_ProjStdParallel2        = 3079 
  integer :: GKey_ProjNatOriginLong       = 3080 
  integer :: GKey_ProjNatOriginLat        = 3081 
  integer :: GKey_ProjFalseEasting        = 3082 
  integer :: GKey_ProjFalseNorthing       = 3083 
  integer :: GKey_ProjFalseOriginLong     = 3084 
  integer :: GKey_ProjFalseOriginLat      = 3085 
  integer :: GKey_ProjFalseOriginEasting  = 3086 
  integer :: GKey_ProjFalseOriginNorthing = 3087 
  integer :: GKey_ProjCenterLong          = 3088 
  integer :: GKey_ProjCenterLat           = 3089 
  integer :: GKey_ProjCenterEasting       = 3090 
  integer :: GKey_ProjCenterNorthing      = 3091 
  integer :: GKey_ProjScaleAtNatOrigin    = 3092 
  integer :: GKey_ProjScaleAtCenter       = 3093 
  integer :: GKey_ProjAzimuthAngle        = 3094 
  integer :: GKey_ProjStraightVertPoleLong= 3095 
  integer :: GKey_VerticalCSType          = 4096 
  integer :: GKey_VerticalCitation        = 4097 
  integer :: GKey_VerticalDatum           = 4096 
  integer :: GKey_VerticalUnits           = 4099 

  character (len=9) :: tagTypeName(12)
  integer           :: typeSize(12)
  data tagTypeName( 1), typeSize( 1)  / 'byte     ' ,  1 / 
  data tagTypeName( 2), typeSize( 2)  / 'ascii    ' ,  1 / 
  data tagTypeName( 3), typeSize( 3)  / 'short    ' ,  2 / 
  data tagTypeName( 4), typeSize( 4)  / 'long     ' ,  4 / 
  data tagTypeName( 5), typeSize( 5)  / 'rational ' ,  8 / 
  data tagTypeName( 6), typeSize( 6)  / 'sbyte    ' ,  1 / 
  data tagTypeName( 7), typeSize( 7)  / 'undefined' ,  1 / 
  data tagTypeName( 8), typeSize( 8)  / 'sshort   ' ,  2 / 
  data tagTypeName( 9), typeSize( 9)  / 'slong    ' ,  4 / 
  data tagTypeName(10), typeSize(10)  / 'srational' ,  8 / 
  data tagTypeName(11), typeSize(11)  / 'float    ' ,  4 / 
  data tagTypeName(12), typeSize(12)  / 'double   ' ,  8 / 

  integer (kind=8), parameter :: intAdj4 = 4294967296_8  ! adjustment for 4-byte unsigned integers
  integer (kind=4), parameter :: intAdj2 = 65536         ! adjustment for 2-byte unsigned integers
  integer (kind=2), parameter :: intAdj1 = 256           ! adjustment for 1-byte unsigned integers

  !GeoTIFF Dir and Keys:  --------------------
  type GEO_KEY
      integer         :: Id,Typ,Cnt,Offset  !8-bytes (2,2,2,2) key 
  end type  
  type GEO_DIR
      integer                    :: n_Keys, version,revision,minor_revision
      type(GEO_KEY), allocatable :: Key(:)  
      integer                    :: offsetAscii,offsetFloat
  end type
  !Image File Dir and Tags: ------------------
  type TIFF_TAG
      integer         :: Id,Typ,Cnt,Offset  !12-bytes (2,2,4,4) tag
  end type  
  type TIFF_IFD                    !Image File Directory
     integer          :: n_tags    ! # of Tags in this IFD            !2-bytes
     type(TIFF_TAG), allocatable :: Tag(:) 
     integer          :: offset    ! next IFD offset or 0 (end IFD)   !4-bytes
  end type
  !-------------------------------------------
  type TIFF_FILE                     !Representation of TIFF file
     integer(kind=4)    :: iUnit     ! id of file (for open and close commands)
     character(50)      :: path      ! path to file
     !Header
     character(len=2)   :: byteOrder ! II/MM
     integer            :: magic_num ! 42
     integer            :: offset    ! offset (location from begining in bytes) of 1st IFD (IMPORTANT: first byte = 0!)
     type (TIFF_IFD)    :: IFD(5)    ! Image File Directory (IFD) list
     type (GEO_DIR)     :: gDir      ! GeoDir with GeoKeys
     integer            :: n_imgs    !total number of tags, total number of ifds
     !Important extra parameters:
     logical            :: swapByte=.false.
     character(5)       :: ImgType='strip'       !'strip' / 'tile'
  end type
  !-------------------------------------------
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

        ![OK] Read Header
        call TIFF_READ_HEADER(tiff)  
        ![OK] Read all IFDs
        call TIFF_READ_IFDS(tiff)
        
        ![  ] Read GeoKeys from GeoDir
        if ( gotTag(tiff, 1, GTIFF_GeoKeyDirectoryTag ) ) then 
            call GTIFF_READ_GDIR(tiff)
        else
            print*, "Error: Not a GeoTIFF File!"
        endif

        
        

        if      ( gotTag(tiff,1, TIFF_TileOffsets ) ) then 
           print*, " Tiled type!"; tiff%ImgType='tile'
        else if ( gotTag(tiff, 1, TIFF_StripOffsets) ) then
           print*, " Strip type!"; tiff%ImgType='strip'
        else
           stop "Error Image type not identified!"
        end if
     case ('w','W','write','WRITE','Write')
        stop "Write option not supported yet!"
        
     case default
      
   end select
end subroutine

subroutine TIFF_Close(tiff)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   close(tiff%iUnit)
endsubroutine

subroutine TIFF_READ_HEADER(tiff)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   character(len=1) :: cha_1(2)
   integer (kind=1) :: magic_1(2), offset_1(4)

   print*,'  Reading 8-byte header..'
   read(unit=tiff%iUnit, rec=1) cha_1(1)     ! endianness 
   read(unit=tiff%iUnit, rec=2) cha_1(2)     ! endianness 
   tiff%byteOrder = transfer([cha_1(1), cha_1(2)], tiff%byteOrder)
   if ( tiff%byteOrder /= cpuEnd() ) tiff%swapByte=.true.   !If byteOrder is diffrent to CPU byte-order change the way of reading the tiff file:

   read(unit=tiff%iUnit, rec=3) magic_1(1)   ! magic number (42) 
   read(unit=tiff%iUnit, rec=4) magic_1(2)   ! magic number (42) 
   read(unit=tiff%iUnit, rec=5) offset_1(1)  ! IFD offset   (1st byte) 
   read(unit=tiff%iUnit, rec=6) offset_1(2)  ! IFD offset   (2nd byte) 
   read(unit=tiff%iUnit, rec=7) offset_1(3)  ! IFD offset   (3rd byte) 
   read(unit=tiff%iUnit, rec=8) offset_1(4)  ! IFD offset   (4th byte)  
   
   if ( tiff%swapByte ) then
      tiff%magic_num = transfer([magic_1(2), magic_1(1)], tiff%magic_num)                    
      tiff%offset    = transfer([offset_1(4), offset_1(3), offset_1(2), offset_1(1)], int(1) )
   else
      tiff%magic_num = transfer([magic_1(1), magic_1(2)], tiff%magic_num)             
      tiff%offset    = transfer([offset_1(1), offset_1(2), offset_1(3), offset_1(4)], int(1) )
   end if
   !CHECKS ==========================!
   if ( tiff%magic_num /= 42       ) stop 'Not a TIFF file..'            !Check magic-number (TIFF format identifier)
   if ( tiff%offset     < 0        ) tiff%offset = tiff%offset + intAdj4 !Adjustment for 4-byte unsigned integers
   !=================================!
   print '("   Header: ",A3, I4, I12)',tiff%byteOrder,tiff%magic_num,tiff%offset

end subroutine 

subroutine TIFF_READ_IFDs(tiff)
  implicit none
  type(TIFF_FILE), intent(inout) :: tiff
  integer(kind=1) :: ntags_1(2), id_1(2), typ_1(2)
  integer(kind=1) :: cnt_1(4), off_1(4), ioff_1(4)
  integer         :: i,t,tmp_offset
  
  tmp_offset=tiff%offset
  i=1
  do while (tmp_offset /= 0) !
     print '("   Image File Directory (IFD): ",I6)', i
     tiff%IFD(i)%offset=tmp_offset 

     read(unit=tiff%iUnit, rec=tmp_offset+1) ntags_1(1) ! # tags (1)
     read(unit=tiff%iUnit, rec=tmp_offset+2) ntags_1(2) ! # tags (2)
     !print*,ntags_1
     if (tiff%swapByte) then
        tiff%IFD(i)%n_tags = transfer(ntags_1(2:1:-1), int(1))
     else
        tiff%IFD(i)%n_tags = transfer(ntags_1        , int(1))
     end if
     print '("    # of Tags in IFD: ",I12)', tiff%IFD(i)%n_Tags 
     allocate(tiff%IFD(i)%tag( tiff%IFD(i)%n_Tags ))

     do t=1,tiff%IFD(i)%n_tags

        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+1 )  id_1(1) !TagId   (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+2 )  id_1(2) !TagId   (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+3 ) typ_1(1) !TagType (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+4 ) typ_1(2) !TagType (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+5 ) cnt_1(1) !Count   (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+6 ) cnt_1(2) !Count   (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+7 ) cnt_1(3) !Count   (3)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+8 ) cnt_1(4) !Count   (4)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+9 ) off_1(1) !Offset  (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+10) off_1(2) !Offset  (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+11) off_1(3) !Offset  (3)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+12) off_1(4) !Offset  (4)

        if ( tiff%swapByte ) then
            tiff%IFD(i)%tag(t)%Id     = transfer( id_1(2:1:-1), int(1))
            tiff%IFD(i)%tag(t)%Typ    = transfer(typ_1(2:1:-1), int(1))
            tiff%IFD(i)%tag(t)%Cnt    = transfer(cnt_1(4:1:-1), int(1))
            tiff%IFD(i)%tag(t)%Offset = transfer(off_1(4:1:-1), int(1))
        else 
            tiff%IFD(i)%tag(t)%Id     = transfer( id_1        , int(1))
            tiff%IFD(i)%tag(t)%Typ    = transfer(typ_1        , int(1))
            tiff%IFD(i)%tag(t)%Cnt    = transfer(cnt_1        , int(1))
            tiff%IFD(i)%tag(t)%Offset = transfer(off_1        , int(1))
        end if

        if ( tiff%IFD(i)%tag(t)%Cnt    < 0 )  tiff%IFD(i)%tag(t)%Cnt   =tiff%IFD(i)%tag(t)%Cnt    +  intAdj4 
        if ( tiff%IFD(i)%tag(t)%Offset < 0 )  tiff%IFD(i)%tag(t)%Offset=tiff%IFD(i)%tag(t)%Offset +  intAdj4 

        call print_tag(t,tiff%IFD(i)%tag(t))
        
     end do

     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+1) ioff_1(1) !IFD Offset  (1)
     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+2) ioff_1(2) !IFD Offset  (2)
     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+3) ioff_1(3) !IFD Offset  (3)
     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+4) ioff_1(4) !IFD Offset  (4)

     if (tiff%swapByte) then
        tmp_offset    = transfer(ioff_1(4:1:-1), tmp_offset)
     else
        tmp_offset    = transfer(ioff_1        , tmp_offset)
     end if
     print '("   IFD offset: ",i3)',tmp_offset
     i=i+1
  end do
  tiff%n_imgs=i-1

end subroutine

subroutine GTIFF_READ_GDIR(tiff)
      implicit none
      type(TIFF_FILE),intent(inout) :: tiff
      integer         :: gDirOffset,k,typ,cnt
      logical         :: found
      integer(kind=1) :: v_1(2),  r_1(2), mr_1(2), nk_1(2)  !Head(8)
      integer(kind=1) ::id_1(2),typ_1(2),cnt_1(2),off_1(2)  !Key(8)

      call get_tag_parameters(tiff,1,GTIFF_GeoDoubleParamsTag,typ,cnt,tiff%gDir%OffsetFloat,found) !offset of geoDir
      call get_tag_parameters(tiff,1,GTIFF_GeoAsciiParamsTag ,typ,cnt,tiff%gDir%OffsetAscii,found) !offset of geoDir

      print '("   GeoTIFF Keys Directory (geoDir): ",I6)'
      call get_tag_parameters(tiff,1,GTIFF_geoKeyDirectoryTag,typ,cnt,gDirOffset,found) !offset of geoDir

      print*, tiff%gDir%offsetFloat, tiff%gDir%offsetAscii 

      read(unit=tiff%iUnit, rec=gDirOffset+1)  v_1(1)  ! version        - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+2)  v_1(2)  ! version        - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+3)  r_1(1)  ! revision       - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+4)  r_1(2)  ! revision       - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+5) mr_1(1)  ! minor revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+6) mr_1(2)  ! minor revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+7) nk_1(1)  ! number of GeoKeys (1)
      read(unit=tiff%iUnit, rec=gDirOffset+8) nk_1(2)  ! number of GeoKeys (2)

      if (tiff%swapByte) then
          tiff%gDir%version       = transfer(  v_1(2:1:-1), int(1) )
          tiff%gDir%revision      = transfer(  r_1(2:1:-1), int(1) )
          tiff%gDir%minor_revision= transfer( mr_1(2:1:-1), int(1) )
          tiff%gDir%n_keys        = transfer( nk_1(2:1:-1), int(1) )
      else
          tiff%gDir%version       = transfer(  v_1        , int(1) )
          tiff%gDir%revision      = transfer(  r_1        , int(1) )
          tiff%gDir%minor_revision= transfer( mr_1        , int(1) )
          tiff%gDir%n_keys        = transfer( nk_1        , int(1) )
      end if
      allocate(tiff%gDir%key(tiff%gDir%n_Keys))
      print '("    # of Keys in gDir: ",I6)', tiff%gDir%n_Keys
      do k=1,tiff%gDir%n_Keys
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+1)  id_1(1) !id
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+2)  id_1(2) !id
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+3) typ_1(1) !tiffTagLocation (kind)
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+4) typ_1(2) !tiffTagLocation (kind)
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+5) cnt_1(1) !count
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+6) cnt_1(2) !count
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+7) off_1(1) !offset/value
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+8) off_1(2) !offset/value

         if (tiff%swapByte) then
            tiff%gDir%key(k)%Id    = transfer( id_1(2:1:-1), int(1) ) 
            tiff%gDir%key(k)%Typ   = transfer(typ_1(2:1:-1), int(1) )
            tiff%gDir%key(k)%Cnt   = transfer(cnt_1(2:1:-1), int(1) )
            tiff%gDir%key(k)%Offset= transfer(off_1(2:1:-1), int(1) )
         else
            tiff%gDir%key(k)%Id    = transfer( id_1        , int(1) ) 
            tiff%gDir%key(k)%Typ   = transfer(typ_1        , int(1) )
            tiff%gDir%key(k)%Cnt   = transfer(cnt_1        , int(1) )
            tiff%gDir%key(k)%Offset= transfer(off_1        , int(1) )
         end if

         if ( tiff%gDir%key(k)%Id     < 0 )  tiff%gDir%key(k)%Id    = tiff%gDir%key(k)%Id     +  intAdj4
         if ( tiff%gDir%key(k)%Typ    < 0 )  tiff%gDir%key(k)%Typ   = tiff%gDir%key(k)%Typ    +  intAdj4
         if ( tiff%gDir%key(k)%Cnt    < 0 )  tiff%gDir%key(k)%Cnt   = tiff%gDir%key(k)%Cnt    +  intAdj4
         if ( tiff%gDir%key(k)%Offset < 0 )  tiff%gDir%key(k)%Offset= tiff%gDir%key(k)%Offset +  intAdj4 
         
         call print_key(k,tiff%gDir%key(k))
      enddo

end subroutine


!=== TIFF_GET_TAG_VALUE  ====================
subroutine get_tag_value_int(tiff,i,tagId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i      !image number
   integer        , intent(in)     :: tagId
   integer        , intent(inout)  :: val
   integer                         :: tmp_arr(1)
   tmp_arr(1)=val
   call get_tag_values_int(tiff,i,tagId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_tag_value_float(tiff,i,tagId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i      !image number
   integer        , intent(in)     :: tagId
   real           , intent(inout)  :: val
   real                            :: tmp_arr(1)
   tmp_arr(1)=val
   call get_tag_values_float(tiff,i,tagId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_tag_values_int(tiff,i,tagId,values)  !for INTEGERs
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i      !image number
   integer        , intent(in)     :: tagId
   integer        , intent(inout)  :: values(:)
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=off
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            !values(c)=transfer(values_1(1+(c-1)*siz:c*siz), values(1))
            if (tiff%swapByte) then
                values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), int(1))
            else
                values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), int(1))
            end if
            if ( values(c) < 0) then
                if ( typ == 3 ) values(c)=values(c)+intAdj4   !short (2-bytes) unsigned
                if ( typ == 4 ) values(c)=values(c)+intAdj4   !long  (4-bytes) unsigned
            endif
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine

subroutine get_tag_values_float(tiff,i,tagId,values)  !for REAL (float)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i      !image number
   integer        , intent(in)     :: tagId
   real           , intent(inout)  :: values(:)
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=real(off)
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            !values(c)=transfer(values_1(1+(c-1)*siz:c*siz), values(1))
            if (tiff%swapByte) then
                values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), float(1))
            else
                values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), float(1))
            end if
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine

subroutine get_tag_values_char(tiff,i,tagId,values)  !for CHARACTER
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i      !image number
   integer        , intent(in)     :: tagId
   character(*)   , intent(inout)  :: values
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=char(off)
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            !values(c:c)=transfer(values_1(1+(c-1)*siz:c*siz), values(1:1))
            values(c:c)=transfer(values_1(c)                 , values(1:1))
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine
!=== END TIFF_GET_TAG_VALUE =============

!=== GTIFF_GET_KEY_VALUE     ============
subroutine get_key_value_short(tiff, keyId, values)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   integer        , intent(inout)  :: values
   integer                         :: typ,cnt,off
   logical                         :: found
   call get_key_parameters(tiff,keyId,typ,cnt,off,found)
   values=off
end subroutine 

subroutine get_key_value_double(tiff,keyId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   real           , intent(inout)  :: val
   real                            :: tmp_arr(1)
   tmp_arr(1)= val
   call get_key_values_double(tiff,keyId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_key_values_double(tiff, keyId, values)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   real           , intent(inout)  :: values(:)
   integer                         :: typ,cnt,off,siz
   logical                         :: found
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_key_parameters(tiff,keyId,typ,cnt,off,found)
   if (found) then
      siz=8; allocate(values_1(siz*cnt)) !double (float)
      call get_field_as_byte_array(tiff, tiff%gDir%offsetFloat+siz*off, values_1)
      do c=1,cnt
         if (tiff%swapByte) then
             values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), float(1))
         else
             values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), float(1))
         end if
      enddo
      deallocate(values_1)
   endif
end subroutine 

subroutine get_key_values_ascii(tiff, keyId, values)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   character(*)   , intent(inout)  :: values
   integer                         :: typ,cnt,off,siz
   logical                         :: found
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_key_parameters(tiff,keyId,typ,cnt,off,found)
   if (found) then
      siz=1; allocate(values_1(siz*cnt)) !ascii
      call get_field_as_byte_array(tiff, tiff%gDir%offsetAscii+siz*off, values_1)
      do c=1,cnt
         values(c:c)=transfer(values_1(c), char(1))
      enddo
      deallocate(values_1)
   endif
end subroutine 

!=== END GTIFF GET_KEY_VALUE ============
 

!MISC Functions =====================
logical function gotTag(tiff,i,tagId)
   implicit none
   type(TIFF_FILE), intent(in) ::tiff
   integer        , intent(in) ::tagId
   integer        , intent(in) :: i
   integer :: t
   gotTag=.false.
   if ( i <= tiff%n_imgs ) then
      do t=1,tiff%IFD(i)%n_tags
          if ( tiff%IFD(i)%tag(t)%Id == tagId ) then
             gotTag=.true.
             return
          endif
      enddo
   endif
end function

subroutine get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   implicit none
   type(TIFF_FILE),intent(in)    :: tiff
   integer        , intent(in)   :: i      !image index (number)
   integer        ,intent(in)    :: tagId
   integer        ,intent(inout) :: typ,cnt,off
   logical        ,intent(inout) :: found
   integer :: t  !loop indices

   do t=1,tiff%IFD(i)%n_tags
       if ( tiff%IFD(i)%tag(t)%Id == tagId ) then
          typ=tiff%IFD(i)%tag(t)%typ
          cnt=tiff%IFD(i)%tag(t)%cnt
          off=tiff%IFD(i)%tag(t)%offset
          found=.true.
          return
       endif
   enddo
   if (.not. found) print '("Error: TagId not found:",I5," (",A,")")',tagId,trim(tagName(tagId))
end subroutine

subroutine get_key_parameters(tiff,keyId,typ,cnt,off,found)
   implicit none
   type(TIFF_FILE),intent(in)    :: tiff
   integer        ,intent(in)    :: keyId
   integer        ,intent(inout) :: typ,cnt,off
   logical        ,intent(inout) :: found
   integer :: k    !loop indices
   do k=1,tiff%gDir%n_Keys
       if ( tiff%gDir%key(k)%Id == keyId ) then
          typ=tiff%gDir%key(k)%typ
          cnt=tiff%gDir%key(k)%cnt
          off=tiff%gDir%key(k)%offset
          found=.true.
          return
       endif
   enddo
   if (.not. found) print '("Error: KeyId not found:",I5," (",A,")")',keyId,trim(keyName(keyId))
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
     case default ;tagName='Not Recognized TagId  '
  end select
end function

character(len=25) function keyName(keyId)
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
     case default;keyName= 'Not recognized geoKey   '
  end select
end function

character(6) function keyTypeName(keyType)
  implicit none
  integer,intent(in) :: keyType
  select case (keyType)
     case (0    ) ; keyTypeName= 'short '
     case (34736) ; keyTypeName= 'double'
     case (34737) ; keyTypeName= 'ascii '
     case default;  keyTypeName= 'short*'
  end select
end function

integer function keyTypeSize(keyType)
  implicit none
  integer,intent(in) :: keyType
  select case (keyType)
     case (0    ) ; keyTypeSize= 2 !short 
     case (34736) ; keyTypeSize= 8 !double
     case (34737) ; keyTypeSize= 1 !ascii 
     case default;  keyTypeSize=-99
  end select
end function

! Informative DEBUG support functions:
subroutine print_tag(t,tag)
 implicit none
 integer      , intent(in) :: t
 type(TIFF_TAG), intent(in) :: tag
 print '("    Tag ",i2,":",i9," (",A22,")",i10," (",A6,")", i12,i12)',t, tag%Id, tagName(tag%Id), tag%Typ, tagTypeName(tag%Typ), tag%Cnt, tag%Offset 
end subroutine

subroutine print_key(k,key)
 implicit none
 integer      , intent(in) :: k
 type(GEO_KEY), intent(in) :: key
 print '("    Key ",i2,":",i9," (",A22,")",i10," (",A6,")", i12,i12)',k, key%Id, keyName(key%Id), key%Typ, keyTypeName(key%Typ), key%Cnt, key%Offset 
end subroutine

subroutine debug_print_tiff_content(tiff)
   implicit none
   type(TIFF_FILE), intent(in) ::tiff
   integer :: i,t,k
   print*, "DEBUG: Check Tiff Content:"
   print*, "TIFF: ",tiff%path,  tiff%iUnit
   print*, "  Header:",tiff%byteOrder, tiff%magic_num, tiff%offset
   do i=1,tiff%n_imgs
   print*, "  IFS ",i, tiff%IFD(i)%n_Tags
   do t=1,tiff%IFD(i)%n_Tags
      call print_tag(t,tiff%IFD(i)%tag(t))
   enddo
   enddo
   print*, "  GDir", tiff%gDir%n_Keys
   do k=1,tiff%gDir%n_Keys
      call print_key(k,tiff%gDir%key(k))
   enddo
end subroutine

function cpuEnd()   ! Determines the endian-ness of the CPU
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
subroutine TIFF_GET_IMAGE(tiff,img_num,img)
   implicit none
   type(TIFF_FILE), intent(in)   :: tiff
   integer        , intent(in)   :: img_num
   real           , intent(inout):: img(:)
   integer :: i,j,k,b,e,recNum
   integer(kind=1), allocatable :: value_1(:)
   integer                      :: imageWidth,imageLength,bytesPerSample,bitsPerSample
   !if strips:
   integer, allocatable :: stripOffSets(:), stripByteCounts(:)
   integer              :: nstrips,rps
   !if tiles:
   integer, allocatable :: tileOffSets(:), tileByteCounts(:)              
   integer              :: tileWidth,tileLength, tilesAcross,tilesDown,tilesPerImage

   !get some image parameters:
   call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_ImageWidth     ,imageWidth )
   call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_ImageLength    ,imageLength )
   !call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_SamplesPerPixel, tiff%samplesPerPixel)
   call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_BitsPerSample  , bitsPerSample      )
   bytesPerSample=bitsPerSample/8
 
   !allocate 1-sample bytes array  
   allocate(value_1(bytesPerSample))
   
   SELECT CASE(tiff%ImgType)
    CASE("strip")
        call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_RowsPerStrip   ,rps )
        nstrips=floor(real((imageLength+rps-1)/rps))
        allocate(StripOffsets    (nstrips))
        allocate(StripByteCounts(nstrips))
        call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_StripOffsets   ,StripOffsets   )
        call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_StripByteCounts,StripByteCounts)
        e=0
        do i=1,size(stripOffsets)
           do j=1,rps
              do k=1,imageWidth !StripByteCounts(i),bytesPerSample
                 do b=1,bytesPerSample
                     recNum=stripOffsets(i)+1+( (j-1)*imageWidth + (k-1) )*bytesPerSample + b
                     read(tiff%iUnit, rec=recNum) value_1(b)
                 enddo
                 if ( e >= imageWidth*imageLength ) cycle 
                 e=e+1
                 if (tiff%swapByte) then
                     img(e)   = transfer( value_1(bytesPersample:1:-1), img(1) )
                 else
                     img(e)   = transfer( value_1                     , img(1) )
                 end if
             enddo
           enddo
        enddo

    CASE("tile")
       stop "ERROR: Tiled images not supported yet.."

       call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileWidth      ,tileWidth    )
       call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileLength     ,tileLength   )

       TilesAcross   = (ImageWidth  + TileWidth  - 1) / TileWidth  
       TilesDown     = (ImageLength + TileLength - 1) / TileLength 
       TilesPerImage = TilesAcross * TilesDown

       allocate(TileOffsets   (tilesPerImage))
       !allocate(TileByteCounts(tilesPerImage))
       call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileOffsets    ,tileOffsets    )
       !call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileByteCounts ,tileBytecounts )

       e=0
      do i=1,size(tileOffsets)
         ! compute number of current tile, tile row, tile col
         ! first row in tile, first col in tile
         !tileNum = tileNum+1_8
         !tileRow = int((tileNum-1_8)/tilesAcross)+1_8
         !tileCol = mod(tileNum-1_8,tilesAcross)+1_8
         !tileFirstRow = ((tileRow-1_8)*vars%tileLen)+1_8
         !tileFirstCol = ((tileCol-1_8)*vars%tileWid)+1_8
         
         do j=1, tileLength  !! loop over rows in tile
            do k=1, tileWidth
                 do b=1,bytesPerSample
                  !---  Compute record number (implied 1 byte per record)
                  !thisrec =  vars%dataOS(idos) + ((((tmprow-tileFirstRow)*vars%tileWid) +   (tmpcol-tileFirstCol))* vars%bytesPerSample) + 1_8
                  recNum=tileOffsets(i)+1+( (j-1)*tileWidth + (k-1) )*bytesPerSample + b
                 enddo
                 !Hay que ver como los organizo en el array img(e) de forma que queden bien..
                 !e=e+1   
                 !if (tiff%swapByte) then
                 !    img(e)   = transfer( value_1(bytesPersample:1:-1), img(1) )
                 !else
                 !    img(e)   = transfer( value_1                     , img(1) )
                 !end if

            end do ! tileWid, cols
         end do    ! tileLen, rows
      END DO ! tile offsets         

   END SELECT
 
end subroutine TIFF_GET_IMAGE
!=== END TIFF_GET_IMAGE =============




!=============================================================== 


end module  
