program test_tiff

  use GeoTIFF

  implicit none

  !INCLUDE 'table.ext'
  integer            :: ios
  type(TIFF_file)    :: my_tiff

  !----------------------------------------
  !(1 PART) Read and extract data from TIFF:
  
   call TIFF_Open("tire.tif",124, my_tiff, ios) 
   if (ios==0) then
       ![ ] TIFF_Get_Tag_Value(tiff, tagId, tagValue) (tag value)
       !call TIFF_GET_FIELD(my_tiff, TIFF_ImageWidth    )
       !call TIFF_GET_FIELD(my_tiff, TIFF_ImageLength   )
       !call TIFF_GET_FIELD(my_tiff, TIFF_BitsPerSample )
       !call TIFF_GET_FIELD(my_tiff, TIFF_StripOffsets  )

       ![ ] TIFF_Get_Img_Value(tiff,   img(i,:,:))

       ![ ] GeoTIFF_get_PROJ(tiff, proj4string)
       ![ ] GeoTIFF_get_GRID(tiff)

   call TIFF_Close(my_tiff)
   else
      stop 'Failed to read TIFF file'
   endif

   !----------------------------------------
   !(2 PART) Create/Write a TIFF:


end program
