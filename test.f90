program test_tiff

  use GeoTIFF

  implicit none
  integer            :: ierr
  type(TIFF_file)    :: my_tiff

  integer              :: wid,len,bps,rps
  integer, allocatable :: sof(:),sbc(:)
  character(100)       :: words=''
  real,allocatable     :: image(:)
  !----------------------------------------
  !(1 PART) Read and extract data from TIFF:
   !call TIFF_Open(124,"files/tire.tif",'r', my_tiff, ierr)
   call TIFF_Open(124,"files/test.tif",'r', my_tiff, ierr)
   if (ierr==0) then

       ![ ] TIFF_Get_FIELD(tiff   , tagId                , Value)
       call TIFF_GET_FIELD(my_tiff, TIFF_ImageWidth      , wid  )
       call TIFF_GET_FIELD(my_tiff, TIFF_ImageLength     , len  )
       call TIFF_GET_FIELD(my_tiff, TIFF_BitsPerSample   , bps  )

       call TIFF_GET_FIELD(my_tiff, TIFF_RowsPerStrip    , rps  )

       allocate(sof(floor(real((len+rps-1)/rps) )))                              !esta mal wid/rps
       allocate(sbc(floor(real((len+rps-1)/rps) )))                              !esta mal wid/rps
       call TIFF_GET_FIELD(my_tiff, TIFF_StripOffsets    , sof  )
       call TIFF_GET_FIELD(my_tiff, TIFF_StripByteCounts , sbc  )
       !call TIFF_GET_FIELD(my_tiff, TIFF_ImageDescription, words)

       print*,"---"
       print '("wid:",i5,". len:",i5,". bps:",i3,". rps:",i5)',wid,len,bps,rps
       print*,"sof:",sof(1:10)
       print*,"sbc:",sbc(1:10)
       print*,"tif description:",words
       print*, "Tiff type: ",my_tiff%ImgType
       print*,"---"

       ![ ] TIFF_GET_IMG_VALUES(tiff,   img(i,:,:))
       !allocate(image(wid*len))
       !call TIFF_GET_IMAGE(my_tiff,  image )

       !GeoTIFF procedures:
       ![ ] GeoTIFF_get_PROJ(tiff, proj4string)
       ![ ] GeoTIFF_get_GRID(tiff, nx,ny,dx,dy,xmin,xmax,ymin,ymax)


   call TIFF_Close(my_tiff)
   else
      stop 'Failed to read TIFF file'
   endif

   !----------------------------------------
   !(2nd PART) Create/Write a TIFF:
   !la idea seria creat un TIFF_FILE, ir poniendo todo ahi y una vez completo escribirlo a un archivo.

   !call TIFF_Create_TIFF_FILE(tif)                  
   !call TIFF_Write_TIFF_FILE ('file_name.tif',tiff)



end program
