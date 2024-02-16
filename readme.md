# GeoTIFF Fortran Module

> Minimalistic GeoTIFF reader module for Fortran. Sintax was inspired on LIBTIFF C-Library.


## List of functions available:

Open/close file:
- [x] `TIFF_Open  (iunit, fileName, 'r', tiff, ierror)` 
- [x] `TIFF_Close (tiff)               `

Read commands:
- [x] `TIFF_Get_Tag_Value (tiff, tagId, value)`
- [x] `TIFF_Get_Image     (tiff, *nimg, image)`
- [ ] `GTIFF_Get_Key_Value(tiff, keyId, value)`


## Example of use:

```fortran
program my_program
   use geoTiff
   implicit none  
   type(TIFF_FILE) :: my_tiff
   integer              :: ierr,wid,len,bps
   character(100)       :: descr
   real   , allocatable :: image(:)
   integer, allocatable :: sof
   
   call TIFF_Open(124,"files/cea.tif",'r', my_tiff, ierr)
   if (ierr==0) then
   
       call TIFF_GET_TAG_VALUE(my_tiff, TIFF_ImageWidth      , wid  )
       call TIFF_GET_TAG_VALUE(my_tiff, TIFF_ImageLength     , len  )
       call TIFF_GET_TAG_VALUE(my_tiff, TIFF_BitsPerSample   , bps  )
       call TIFF_GET_TAG_VALUE(my_tiff, TIFF_ImageDescription, descr)
   
       allocate(image(wid*len))
       call TIFF_GET_IMAGE(my_tiff,  image )

      call TIFF_Close(my_tiff)
   else
      stop 'Failed to read TIFF file'
   endif

end program
```


## To-do list:

General:
- [x] GeoKey's values access (`gtiff_get_key_value`)
- [x] Add big-Enddian support (swap byte-order)
- [~] Add support for 'tiles'
- [~] Swap-vertical order of Image-Array so [0,0] == southern-west pixel
- [~] Extend `TIFF_Get_Image` for categorical (integer) fields
- [ ] Find how to get XY coordinates
- [~] Add support for multi-image tiff
- [ ] Add support for multi-band image tiff
- [ ] Write/Create functions

Be complaint with standards:
- [ ] TIFF 6.0
- [ ] [OGC GEOTIFF 1.1](https://docs.ogc.org/is/19-008r4/19-008r4.html)

Performance:
- [ ] Memory (RSS)
- [ ] CPU usage testing.
- [ ] Parallel access? is it possible?

