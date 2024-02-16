program test_tiff

  use GeoTIFF
  use netcdf  !debug
  implicit none
  integer            :: ierr
  type(TIFF_file)    :: my_tiff

  integer              :: wid,len,bps,rps
  integer, allocatable :: sof(:),sbc(:)
  character(100)       :: words='', citation=''
  real,allocatable     :: image(:)
  real    :: par1,par2,par3,par4
  integer :: i1,i2,i3,i4,i5

  !netcdf
  integer :: ncid,x_dim_id,y_dim_id,var_id
  character(len=100) :: file_path
    
  ! Check if the command line argument is provided
  if (command_argument_count() /= 1) then
      print*, "Usage: ./program <file_path>";stop
  else
      call get_command_argument(1, file_path)
  endif

  !----------------------------------------
  !(1 PART) Read and extract data from TIFF:
   call TIFF_Open(124,trim(file_path) ,'r', my_tiff, ierr)      !TIFF little-endian
   !call TIFF_Open(124,"files/tire.tif",'r', my_tiff, ierr)      !TIFF little-endian
   !call TIFF_Open(124,"files/bali.tif"    ,'r', my_tiff, ierr)  !TIFF big-endian  
   !call TIFF_Open(124,"files/balloons.tif",'r', my_tiff, ierr)  !TIFF big-endian  multiband
   !call TIFF_Open(124,"files/at3_1m4_01.tif",'r', my_tiff, ierr)
   !call TIFF_Open(124,"files/FAA_UTM18N_NAD83.tif",'r', my_tiff, ierr)  !multiband geoTiff
   !call TIFF_Open(124,"files/sinus.tif",'r', my_tiff, ierr)
   !call TIFF_Open(124,"files/cea.tif",'r', my_tiff, ierr)
   !call TIFF_Open(124,"files/erdas_spnad83.tif",'r', my_tiff, ierr)
   if (ierr==0) then

       call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_ImageWidth      , wid  )
       call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_ImageLength     , len  )
       call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_BitsPerSample   , bps  )

       !call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_RowsPerStrip    , rps  )
       !allocate(sof(floor(real((len+rps-1)/rps) )))        
       !allocate(sbc(floor(real((len+rps-1)/rps) )))        
       !call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_StripOffsets    , sof  )
       !call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_StripByteCounts , sbc  )
       call TIFF_GET_TAG_VALUE(my_tiff,  1, TIFF_ImageDescription, words)

       print*,"---"
       print '("wid:",i5,". len:",i5,". bps:",i3,". rps:",i5)',wid,len,bps,rps
       !print*,"sof:",sof(1:10)
       !print*,"sbc:",sbc(1:10)
       print*,"tif description:",words
       print*, "Tiff type: ",my_tiff%ImgType
       print*,"---"

       call debug_print_tiff_content(my_tiff)

       ![ ] TIFF_GET_IMG_VALUES(tiff,   img(i,:,:))
       allocate(image(wid*len))
       call TIFF_GET_IMAGE(my_tiff, 1, image )

       !GeoTIFF procedures:
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_GTCitation       , citation)
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjStdParallel1 , par1    )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjNatOriginLong, par2    )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjFalseEasting , par3    )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjFalseNorthing, par4    )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_GeogAngularUnits , i1      )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjectedCSType  , i2      )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_Projection       , i3      )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjCoordTrans   , i4      )
       call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjLinearUnits  , i5      )
       print*,"***"
       print*,"Citaton:", citation
       print*,"Params (double):", par1,par2,par3,par4
       print*,"Shorts:        :", i1,i2,i3,i4,i5
       print*,"***"

      
       !call GTIFF_GET_KEY_VALUE(tiff, GKey_ProjectedCSType, proj4string)
       ![ ] GTIFF_get_PROJ(tiff, proj4string)
       ![ ] GTIFF_get_GRID(tiff, nx,ny,dx,dy,xmin,xmax,ymin,ymax)
       ![ ] GTIFF_get_XCOORD(tiff, lon )
       ![ ] GTIFF_get_YCOORD(tiff, lat )

   call TIFF_Close(my_tiff)


   !DEBUG
   !Crear NetCDF
   call check(nf90_create('image.nc'    , NF90_CLOBBER, ncid))
      !Defino dimensiones
      call check(nf90_def_dim(ncid, "x"   , wid    , x_dim_id       ))
      call check(nf90_def_dim(ncid, "y"   , len    , y_dim_id       ))
      !Creo variables:
      call check( nf90_def_var(ncid, 'image'           , NF90_FLOAT, [x_dim_id,y_dim_id], var_id)   )
   call check(nf90_enddef(ncid))   !End NetCDF define mode

   !Abro NetCDF y guardo variables de salida
   call check(nf90_open('image.nc'    , nf90_write, ncid       ))
      call check(nf90_inq_varid(ncid,'image'          ,var_id)); call check(nf90_put_var(ncid, var_id,reshape(image,[wid,len]) )) 
   call check(nf90_close( ncid ))
   !DEBUG:

   else
      stop 'Failed to read TIFF file'
   endif

   !----------------------------------------
   !(2nd PART) Create/Write a TIFF:
   !la idea seria creat un TIFF_FILE, ir poniendo todo ahi y una vez completo escribirlo a un archivo.

   !call TIFF_Create_TIFF_FILE(tif)                  
   !call TIFF_Write_TIFF_FILE ('file_name.tif',tiff)


contains
subroutine check(status)            !netcdf error-check function
  integer, intent(in) :: status
  if (status /= nf90_noerr) then
    write(*,*) nf90_strerror(status); stop 'netcdf error'
  end if
end subroutine check
end program
