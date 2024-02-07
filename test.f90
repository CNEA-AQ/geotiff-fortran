program test_tiff

  use GeoTIFF

  implicit none

  integer            :: ierr
  type(TIFF_file)    :: my_tiff

  !character(20)       :: inp_file
  !read(*,*) inp_file

  integer              :: wid,len,bps,rps
  integer, allocatable :: sof(:)
  character(100)       :: words=''
  !----------------------------------------
  !(1 PART) Read and extract data from TIFF:
    
   !call TIFF_Open(trim(inp_file),124, my_tiff, ios) 
   call TIFF_Open("files/cell.tif",124, my_tiff, ierr)
   if (ierr==0) then
       ![ ] TIFF_Get_Tag_Value(tiff, tagId, tagValue) (tag value)
       call TIFF_GET_FIELD(my_tiff, TIFF_ImageWidth    ,wid)
       call TIFF_GET_FIELD(my_tiff, TIFF_ImageLength   ,len)
       call TIFF_GET_FIELD(my_tiff, TIFF_BitsPerSample ,bps)

       call TIFF_GET_FIELD(my_tiff, TIFF_RowsPerStrip  ,rps)

       allocate(sof(int(wid/rps) )) !esta mal wid/rps
       call TIFF_GET_FIELD(my_tiff, TIFF_StripOffsets  ,sof)

       call TIFF_GET_FIELD(my_tiff,270,words)

       print*,"prints:  "
       print '("wid:",i5,". len:",i5,". bps:",i3,". rps:",i5)',wid,len,bps,rps
       print*,"sof:",sof
       print*,"my ascii words:",words
       !GeoTIFF procedures:
       ![ ] GeoTIFF_get_PROJ(tiff, proj4string)
       ![ ] GeoTIFF_get_GRID(tiff, nx,ny,dx,dy,xmin,xmax,ymin,ymax)


       ![ ] TIFF_GET_IMG_VALUES(tiff,   img(i,:,:))


   call TIFF_Close(my_tiff)
   else
      stop 'Failed to read TIFF file'
   endif

   !----------------------------------------
   !(2 PART) Create/Write a TIFF:




end program
