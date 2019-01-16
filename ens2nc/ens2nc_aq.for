C
C --------------------------------------------------------------ENS2NC_AQ---
C
      PROGRAM ENS2NC_AQ
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.

C --- PURPOSE      : This program converts the Ensemble encoded model output
C                    for 2D grids in a NetCDF file.
C                    Depending on parameter DATASET_FORMAT, format is 'NetCDF classic'
C                    of 'NetCDF version 4'
C
C --- AUTHOR       : R.Bianconi - http://www.enviroware.com
C
C --- CONTACT      : S.Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- INPUT        : .ens file encoded with ENCODEG (and standard Ensemble name)
C
C --- OUTPUT       : NetCDF file 
C
C --- NOTES        : Uncomment the include of getarg.for at the bottom
C                    of this file if GETARG is not available
C
C                    Set SLASH in DATA according to the operating system in use
C
C                    Compilation example in Ubuntu Linux:
C                    gfortran -o ens2nc -I/usr/include ens2nc.for -lnetcdff -lnetcdf
C                    where /usr/include is the path to netcdf.inc
C                    See http://stackoverflow.com/questions/13941549/compiling-fortran-netcdf-programs-on-ubuntu
C
C --- REFS         : http://www.unidata.ucar.edu/software/netcdf/
C
C --- VERSION      : 20131104 RBI Forked from deform_aq.for 
C                  : 20140228 RBI - Fix to variable name
C                                 - Update of USER_VAR length from 6 to 20
C                                 - Update to call arguments for DECODEG
C                  : 20171204 RBI - Accepting versions 2013, 204, 2.00
C
C---------------------------------------------------------------ENS2NC_AQ---
C
      IMPLICIT NONE
      INCLUDE '../inc/enform_aq.prm'
      INCLUDE '../inc/version.prm'
      INCLUDE '../inc/enform_aq.cmn'
      INCLUDE '../inc/ens2nc_aq.cmn'

      INCLUDE 'netcdf.inc'
C     
C http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f77.html#NF_005fCREATE
      CHARACTER*(*) DATASET_FORMAT
C     PARAMETER (DATASET_FORMAT = 'CLASSIC_MODEL')    ! writes NetCDF v.3 files
      PARAMETER (DATASET_FORMAT = 'NETCDF4')          ! writes NetCDF v.4 files
      INTEGER LUS, LUN
      INTEGER USER_MODEL_NUMBER, USER_SEQUENCE_NUMBER, USER_CASE_NUMBER
      INTEGER TMP_MODEL_NUMBER, TMP_SEQUENCE_NUMBER, TMP_CASE_NUMBER
      INTEGER TMP_MODEL_NUMBER_ORIGINAL
      INTEGER TMP_ITEM, TMP_VAR
      INTEGER USER_METEO_DATE(5)
      INTEGER TMP_METEO_DATE(5)
      INTEGER USER_ITEM, IV, IPOST
      INTEGER TMP, IOS, NTOUT
      INTEGER LSTRIM, I,J,K,IT,IX
      REAL VAL_MISSING, PREC
      REAL VALUES(NTMAX)
      CHARACTER STRING*8128
      CHARACTER NXFMT*4
      CHARACTER FORMSTRING*37
      CHARACTER USER_RANDOM_KEY*7
      CHARACTER*128 ST1, ST2, TMP_FILE
      CHARACTER*2 ST3

      CHARACTER*128 INPUT_FILE
      CHARACTER*128 SOURCE_FILE
      CHARACTER*128 CF_FILE
      CHARACTER*128 INPUT_PATH
      CHARACTER*128 INPUT_PATH_AND_FILE
      CHARACTER*128 OUTPUT_BASE_PATH
      CHARACTER*128 OUTPUT_BASE
      CHARACTER*128 OUTPUT_B
      CHARACTER*128 OUTPUT_FILE
!     CHARACTER*128 XML_FILE
      CHARACTER*20 USER_VAR
      CHARACTER*1 SLASH

      LOGICAL LERROR 
      LOGICAL LTEST

      INTEGER*8 IREC

!     see for example http://www.unidata.ucar.edu/software/netcdf/examples/programs/pres_temp_4D_wr.f

      INTEGER NDIMS
      PARAMETER (NDIMS = 4)
      INTEGER LVL_DIMID, LON_DIMID, LAT_DIMID, TIME_DIMID
      INTEGER VAL_MISSING_DIMID 
      INTEGER NCID

C     The START and COUNT arrays will tell the netCDF library where to
C     write our data.
      INTEGER START(NDIMS), COUNT(NDIMS)
      INTEGER VARID
      INTEGER DIMIDS(NDIMS)

C     These program variables hold the latitudes and longitudes.
      !real lats(NLATS), lons(NLONS)
      REAL, ALLOCATABLE, DIMENSION(:) :: LATS
      REAL, ALLOCATABLE, DIMENSION(:) :: LONS

      INTEGER LON_VARID, LAT_VARID, TIME_VARID

      REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ALLVAL
C     Error handling.
      INTEGER RETVAL

      INTEGER SHUFFLE
      INTEGER DEFLATE
      INTEGER DEFLATE_LEVEL
C http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f77.html#NF_005fDEF_005fVAR_005fDEFLATE
      PARAMETER (SHUFFLE = 0)
      PARAMETER (DEFLATE = 1)
      PARAMETER (DEFLATE_LEVEL = 9)

      INTEGER CHUNKS(4)

C     Global attributes (will be read from .e2n file and .cf file)
!     CHARACTER*(*) CONVENTIONS    ! Conventions 2.6 CF1.6
      CHARACTER*(*) LVL_NAME, LAT_NAME, LON_NAME, TIME_NAME
!     CHARACTER*(*) TIME_UNITS    ! Conventions 4.4 CF1.6
      CHARACTER*(*) TIME_LONG_NAME    ! Conventions 4.4 CF1.6
      INTEGER NLVLS, NLATS, NLONS

      PARAMETER (LVL_NAME = 'level')   ! We have 1 ens for each level. 
                                       ! We need to specify height somewhere
      PARAMETER (LAT_NAME = 'latitude', LON_NAME = 'longitude') !  These might become parameters
      PARAMETER (TIME_NAME = 'time')
      PARAMETER (TIME_LONG_NAME = 'time')


C --- Set some general parameters read from the E2N file
      CHARACTER*(*) INSTITUTION    ! Conventions 2.6 CF1.6
      CHARACTER*128 SOURCE         ! Conventions 2.6 CF1.6
      CHARACTER*128 COMMENT        ! Conventions 2.6 CF1.6
      CHARACTER*(*) REFERENCES     ! Conventions 2.6 CF1.6
      PARAMETER (INSTITUTION = ' ')
      PARAMETER 
     &    (REFERENCES = 'ENSEMBLE - http://ensemble.jrc.ec.europa.eu')

      DATA LUS, LUN / 27, 28 /
      DATA LERROR / .FALSE. /
C     DATA SLASH / '\' /   ! windows
      DATA SLASH / '/' /   ! linux
C
      DATA LTEST / .FALSE. /

      IF (LTEST) THEN
          INPUT_PATH = '../tests/test_0031/'
          INPUT_FILE = '03700-0031-001-01-01-201101061500.ens'
          SOURCE_FILE = '../tests/test_0031/0031-001.src'
          SOURCE = 'IT2'
          OUTPUT_BASE_PATH = 
     &    '/media/tera-storage/www/htdocs/ensemble/pvt/nc/'

          CF_FILE = 
     &    '/media/tera-storage/www/htdocs/ensemble/pvt/cf/0031-001.cf'
      ELSE
          CALL GETARG(1,INPUT_PATH)
          CALL GETARG(2,INPUT_FILE)
          CALL GETARG(3,SOURCE_FILE)
          CALL GETARG(4,CF_FILE)
          CALL GETARG(5,SOURCE)
          CALL GETARG(6,OUTPUT_BASE_PATH)
      ENDIF

C --- Parse metafile

      OPEN (LUS, FILE=SOURCE_FILE(1:LSTRIM(SOURCE_FILE)), 
     &           STATUS='OLD',ERR=905, IOSTAT= IOS)
      CALL PARSE_METAFILE(LUS)
      CLOSE(LUS)

C --- Parse cf_file

      OPEN (LUS, FILE=CF_FILE(1:LSTRIM(CF_FILE)), 
     &           STATUS='OLD',ERR=906, IOSTAT= IOS)
      CALL PARSE_CF_FILE(LUS)
      CLOSE(LUS)

      WRITE(NXFMT,'(I4.4)') NX

C --- Check Enform version consistency, with backward compatibility
      IF (SOURCE_VERSION .EQ. VERSION .OR.
     &    SOURCE_VERSION .EQ. '20130101' .OR.
     &    SOURCE_VERSION .EQ. '20140101' .OR.
     &    SOURCE_VERSION .EQ. '2.00') THEN
                ! version is ok
      ELSE
          GOTO 500
      ENDIF

C --- Get parameters from the input filename

      INPUT_PATH_AND_FILE = INPUT_PATH(1:LSTRIM(INPUT_PATH))//
     &             INPUT_FILE(1:LSTRIM(INPUT_FILE))
      IF (LDEBUG) THEN
          WRITE(6,*) 'Input file: ',INPUT_PATH_AND_FILE
      ENDIF
C
      READ(INPUT_FILE(1:5),'(I5.5)') TMP_MODEL_NUMBER_ORIGINAL
      IF (LDEBUG) THEN
          WRITE(6,*) 'Model: ',TMP_MODEL_NUMBER_ORIGINAL
      ENDIF
      TMP_MODEL_NUMBER = TMP_MODEL_NUMBER_ORIGINAL
      IF (TMP_MODEL_NUMBER .GT. 99999) GOTO 530
      
      READ(INPUT_FILE(7:10),'(I4.4)') TMP_SEQUENCE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Sequence: ',TMP_SEQUENCE_NUMBER
      ENDIF
      READ(INPUT_FILE(12:14),'(I3.3)') TMP_CASE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Case: ',TMP_CASE_NUMBER
      ENDIF
      READ(INPUT_FILE(16:17),'(I2.2)') TMP_VAR
      IF (LDEBUG) THEN
          WRITE(6,*) 'Variable number: ',TMP_VAR
      ENDIF
      READ(INPUT_FILE(19:20),'(I2.2)') TMP_ITEM
      IF (LDEBUG) THEN
          WRITE(6,*) 'Item number: ',TMP_ITEM
      ENDIF

      READ(INPUT_FILE(22:25),'(I4.4)') TMP
          TMP_METEO_DATE(1) = TMP
          METEO_DATE(1) = TMP
      READ(INPUT_FILE(26:27),'(I2.2)') TMP
          TMP_METEO_DATE(2) = TMP
          METEO_DATE(2) = TMP
      READ(INPUT_FILE(28:29),'(I2.2)') TMP
          TMP_METEO_DATE(3) = TMP
          METEO_DATE(3) = TMP
      READ(INPUT_FILE(30:31),'(I2.2)') TMP
          TMP_METEO_DATE(4) = TMP
          METEO_DATE(4) = TMP
      READ(INPUT_FILE(32:33),'(I2.2)') TMP
          TMP_METEO_DATE(5) = TMP
          METEO_DATE(5) = TMP

      IF (LDEBUG) THEN
          WRITE(6,*) 'Meteo date: ', TMP_METEO_DATE
      ENDIF

C     Now read the input file and make some checks...
      IREC = 11
      TMP_FILE = INPUT_PATH_AND_FILE
      OPEN (LUN, FILE=TMP_FILE, STATUS='OLD', ERR=960)
      IF (LDEBUG) THEN
          WRITE(6,*) 'Opened file: ', TMP_FILE
      ENDIF
      READ(LUN,'(I5.5)') USER_MODEL_NUMBER

      IF (LDEBUG) THEN
          WRITE(6,*) 'Model number in input file: ', USER_MODEL_NUMBER
      ENDIF

      IF (TMP_MODEL_NUMBER_ORIGINAL .NE. USER_MODEL_NUMBER) THEN
          ST1 = ' '
          WRITE(ST1(1:5),'(I5.5)') TMP_MODEL_NUMBER_ORIGINAL
          ST2 = ' '
          WRITE(ST2(1:5),'(I5.5)') USER_MODEL_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Model number in file name '//ST1(1:LSTRIM(ST1))//
     &    ' while model number in file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      MODEL_NUMBER = TMP_MODEL_NUMBER

      READ(LUN,'(A)') USER_RANDOM_KEY
      IF (LDEBUG) THEN
          WRITE(6,*) 'Random key in input file: ', USER_RANDOM_KEY
      ENDIF
      IF (USER_RANDOM_KEY .NE. SOURCE_RANDOM_KEY) THEN
          ST1 = ' '
          WRITE(ST1(1:7),'(A7)') SOURCE_RANDOM_KEY
          ST2 = ' '
          WRITE(ST2(1:7),'(A7)') USER_RANDOM_KEY
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Random key in source file '//ST1(1:LSTRIM(ST1))//
     &    ' while random key in file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      READ(LUN,'(I4)') USER_SEQUENCE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Sequence number in input file: ', 
     &                USER_SEQUENCE_NUMBER
      ENDIF

      IF (TMP_SEQUENCE_NUMBER .NE. USER_SEQUENCE_NUMBER) THEN
          ST1 = ' '
          WRITE(ST1(1:4),'(I4.4)') TMP_SEQUENCE_NUMBER
          ST2 = ' '
          WRITE(ST2(1:4),'(I4.4)') USER_SEQUENCE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Sequence number in file name is '//ST1(1:LSTRIM(ST1))//
     &    ' while sequence number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      IF (SOURCE_SEQUENCE_NUMBER .NE. USER_SEQUENCE_NUMBER) THEN
          ST1 = ' '
          WRITE(ST1(1:4),'(I4.4)') SOURCE_SEQUENCE_NUMBER
          ST2 = ' '
          WRITE(ST2(1:4),'(I4.4)') USER_SEQUENCE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Sequence number in source file is '//ST1(1:LSTRIM(ST1))//
     &    ' while sequence number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      READ(LUN,'(I3)') USER_CASE_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'Case number in input file: ', USER_CASE_NUMBER
      ENDIF

      IF (TMP_CASE_NUMBER .NE. USER_CASE_NUMBER) THEN
          ST1 = ' '
          WRITE(ST1(1:3),'(I3.3)') TMP_CASE_NUMBER
          ST2 = ' '
          WRITE(ST2(1:3),'(I3.3)') USER_CASE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Case number in file name is '//ST1(1:LSTRIM(ST1))//
     &    ' while case number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      IF (SOURCE_CASE_NUMBER .NE. USER_CASE_NUMBER) THEN
          ST1 = ' '
          WRITE(ST1(1:3),'(I3.3)') SOURCE_CASE_NUMBER
          ST2 = ' '
          WRITE(ST2(1:3),'(I3.3)') USER_CASE_NUMBER
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Case number in source file is '//ST1(1:LSTRIM(ST1))//
     &    ' while case number in user file is '//ST2(1:LSTRIM(ST2))
          GOTO 999
      END IF

      COMMENT = 'Case xxxx-xxx - '//TRIM(TITLE)
      WRITE(COMMENT(6:9),'(I4.4)') SOURCE_SEQUENCE_NUMBER

      WRITE(COMMENT(11:13),'(I3.3)') SOURCE_CASE_NUMBER

      READ(LUN,'(A)') USER_VAR
      IF (LDEBUG) THEN
          WRITE(6,*) 'Variable in input file: ', USER_VAR
      ENDIF

      CALL FIXNAME(USER_VAR)

C --- Check if Variable name and code in user file match with
C     name and code in source file
      ICODE = 0

      DO I = 1, NUC_MAX
         IF (USER_VAR .EQ. SOURCE_VAR_NAME(I)) THEN
             ICODE = I
         ENDIF
      ENDDO
      IF (ICODE .EQ. 0) THEN
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Variable name in user file does not appear in source file.'
          GOTO 999
      ENDIF

      IF (TMP_VAR .NE. ICODE) THEN
          ST1 = ' '
          WRITE(ST1(1:2),'(I2.2)') ICODE
          ST2 = ' '
          WRITE(ST2(1:2),'(I2.2)') TMP_VAR
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Variable number in source file: '//ST1(1:LSTRIM(ST1))//
     &    ' Variable number in user file name: '//ST2(1:LSTRIM(ST2))
          GOTO 999
      ENDIF

      READ(LUN,*) USER_ITEM
      IF (LDEBUG) THEN
          WRITE(6,*) 'Item index in input file: ', USER_ITEM
      ENDIF

      READ(LUN,*) VAL_MISSING
      IF (LDEBUG) THEN
          WRITE(6,*) 'Missing value in input file: ', VAL_MISSING
      ENDIF
      IF (VAL_MISSING .NE. VAR_ITEMS_VAL_MISSING(ICODE,USER_ITEM)) THEN
          ST1 = ' '
          WRITE(ST1(1:11),'(E11.4)') VAL_MISSING
          ST2 = ' '
          WRITE(ST2(1:11),'(E11.4)') 
     &           VAR_ITEMS_VAL_MISSING(ICODE,USER_ITEM)
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Missing value in user file: '//ST1(1:LSTRIM(ST1))//
     &    ' Missing value in src file : '//ST2(1:LSTRIM(ST2))
          GOTO 999
      ENDIF

      READ(LUN,*) PREC
      IF (LDEBUG) THEN
          WRITE(6,*) 'Precision in input file: ', PREC
      ENDIF
      IF (PREC .NE. VAR_ITEMS_PRECISION(ICODE,USER_ITEM)) THEN
          ST1 = ' '
          WRITE(ST1(1:11),'(E11.4)') PREC
          ST2 = ' '
          WRITE(ST2(1:11),'(E11.4)') 
     &           VAR_ITEMS_PRECISION(ICODE,USER_ITEM)
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Precision in source file: '//ST1(1:LSTRIM(ST1))//
     &    ' Precision in user file   : '//ST2(1:LSTRIM(ST2))
          GOTO 999
      ENDIF

      READ(LUN,'(I4.4)') NT

      IF (LDEBUG) THEN
          WRITE(6,*) 'Number of output times in model output: ', NT
      ENDIF
      IF (NT .GT. NTMAX) THEN
          ST1 = ' '
          WRITE(ST1(1:4),'(I4)') NT
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Time horizon exceeds NTMAX '//ST1(1:LSTRIM(ST1))
          GOTO 999
      END IF
      IF (NT .GT. NOUTPUTS) THEN
          ST1 = ' '
          WRITE(ST1(1:4),'(I4)') NOUTPUTS
          LERROR = .TRUE.
          ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &    ' Time horizon exceeds NT in source file '//ST1(1:LSTRIM(ST1))
          GOTO 999
      END IF

      READ(LUN,'(I4.4,4I2.2)') (USER_METEO_DATE(K),K=1,5)
      IF (LDEBUG) THEN
          WRITE(6,*) 'Meteo date in file :', (USER_METEO_DATE(K),K=1,5)
      ENDIF
      DO K = 1, 5
         IF (TMP_METEO_DATE(K) .NE. USER_METEO_DATE(K)) THEN
             ST1 = ' '
            WRITE(ST1(1:12),'(I4.4,4I2.2)') (TMP_METEO_DATE(K),J=1,5)
             ST2 = ' '
          WRITE(ST2(1:12),'(I4.4,4I2.2)') (USER_METEO_DATE(K),J=1,5)
             LERROR = .TRUE.
             ERROR_MESSAGE = 'ERROR (ENS2NC_AQ) - '//
     &       ' Meteo date in file name is '//ST1(1:LSTRIM(ST1))//
     &       ' while meteo date in user file is '//ST2(1:LSTRIM(ST2))
             GOTO 999
         END IF
      ENDDO

C --- Here we prepare some netCDF variables of general use
   
      LVL_DIMID = 1
      LON_DIMID = NX
      LAT_DIMID = NY
      VAL_MISSING_DIMID = 1

      NLVLS = 1
      NLONS = NX
      NLATS = NY

      ALLOCATE (LONS(NX))
      ALLOCATE (LATS(NY))
      ALLOCATE (ALLVAL(NX,NY,1))

      DO I = 1, NX
         LONS(I) = X_MIN + (I-1) * DX
      ENDDO
      DO I = 1, NY
         LATS(I) = Y_MIN + (I-1) * DY
      ENDDO

C --- Now loop on the statistics that the file might contain.
C     We create a netCDF file for each of them

      IV = USER_ITEM - 1

      DO IPOST = 1, SOURCE_ITEM_NPOSTS(ICODE,USER_ITEM)

         IV = IV + 1

C --- Open file

         ST1 = 's0000'//SLASH//'c000'//SLASH//'r00'//SLASH
         WRITE(ST1(2:5),'(I4.4)') TMP_SEQUENCE_NUMBER
         WRITE(ST1(8:10),'(I3.3)') TMP_CASE_NUMBER
         WRITE(ST1(13:14),'(I2.2)') TMP_VAR
      
         OUTPUT_BASE = OUTPUT_BASE_PATH(1:LSTRIM(OUTPUT_BASE_PATH))
     &              //ST1(1:LSTRIM(ST1))

         ST1 = '-00000-0000-000-00-'
         WRITE(ST1(2:6),'(I5.5)') TMP_MODEL_NUMBER
         WRITE(ST1(8:11),'(I4.4)') TMP_SEQUENCE_NUMBER
         WRITE(ST1(13:15),'(I3.3)') TMP_CASE_NUMBER
         WRITE(ST1(17:18),'(I2.2)') TMP_VAR

         ST3 = '00'
         WRITE(ST3(1:2),'(I2.2)') IV
         ST2 = ' '
         WRITE(ST2(1:12),'(I4.4,4I2.2)') (TMP_METEO_DATE(K),K=1,5)
         OUTPUT_B = OUTPUT_BASE(1:LSTRIM(OUTPUT_BASE))//'v'//
     &              ST3//SLASH//'v'//
     &              ST3//ST1(1:LSTRIM(ST1))//
     &              ST2

         READ(LUN,'(I4.4)') NTOUT

C --- The nc file
         OUTPUT_FILE = OUTPUT_B(1:LSTRIM(OUTPUT_B))//'.nc'
         IF (LDEBUG) THEN
             WRITE(6,*) 'NetCDF file: ',
     &                   OUTPUT_FILE(1:LSTRIM(OUTPUT_FILE))
         ENDIF

C --- NTOUT depends on the statistics and we set it here

         TIME_DIMID = NTOUT

C --- Create the file. 

         IF (DATASET_FORMAT .EQ. 'CLASSIC_MODEL') THEN
         RETVAL = NF_CREATE(OUTPUT_FILE,NF_CLOBBER,NCID)
         ELSE
         RETVAL = NF_CREATE(OUTPUT_FILE,OR(NF_CLOBBER,NF_NETCDF4),NCID)
         ENDIF
        IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C --- Define global attributes
         RETVAL = NF_PUT_ATT_TEXT(NCID, NF_GLOBAL, 'Conventions', 
     &              len(TRIM(CF_CONVENTIONS)), TRIM(CF_CONVENTIONS))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, NF_GLOBAL, 'institution', 
     &              len(TRIM(INSTITUTION)), TRIM(INSTITUTION))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, NF_GLOBAL, 'source', 
     &              len(TRIM(SOURCE)), TRIM(SOURCE))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, NF_GLOBAL, 'comment', 
     &              len(TRIM(COMMENT)), TRIM(COMMENT))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, NF_GLOBAL, 'references', 
     &              len(TRIM(REFERENCES)), TRIM(REFERENCES))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     Define the dimensions. The record dimension could also 
C     be defined to have unlimited length - it could grow as 
C     needed. 
         RETVAL = NF_DEF_DIM(NCID, LVL_NAME, NLVLS, LVL_DIMID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_DEF_DIM(NCID, LAT_NAME, NLATS, LAT_DIMID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_DEF_DIM(NCID, LON_NAME, NLONS, LON_DIMID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_DEF_DIM(NCID, TIME_NAME, NF_UNLIMITED, TIME_DIMID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     Define the coordinate  and time variables.
         RETVAL = NF_DEF_VAR(NCID, LAT_NAME, NF_REAL, 1,
     &            LAT_DIMID, LAT_VARID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_DEF_VAR(NCID, LON_NAME, NF_REAL, 1, 
     &            LON_DIMID, LON_VARID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_DEF_VAR(NCID, TIME_NAME, NF_INT, 1,
     &            TIME_DIMID, TIME_VARID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     Assign units attributes to coordinate and variables.
         RETVAL = NF_PUT_ATT_TEXT(NCID, LAT_VARID, 
     &            'units', 
     &            len(TRIM(CF_LATITUDE_UNITS)), 
     &            TRIM(CF_LATITUDE_UNITS))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, LAT_VARID, 
     &            'axis',1,'Y')
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, LON_VARID,
     &            'units', len(TRIM(CF_LONGITUDE_UNITS)), 
     6            TRIM(CF_LONGITUDE_UNITS))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, LON_VARID, 
     &            'axis',1,'X')
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, TIME_VARID,
     &            'units', len(TRIM(CF_TIME_UNITS(ICODE,IV,IPOST))), 
     &             TRIM(CF_TIME_UNITS(ICODE,IV,IPOST)))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, TIME_VARID, 
     &            'axis',1,'T')
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, TIME_VARID,
     &            'standard_name', len(TRIM('time')),
     &            'time')
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, TIME_VARID,
     &            'long_name', len(TRIM(TIME_LONG_NAME)),
     &            TRIM(TIME_LONG_NAME))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, TIME_VARID,
     &            'calendar', len(TRIM('gregorian')),
     &            'gregorian')
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)


C     The DIMIDS array is used to pass the DIMIDS of the dimensions of
C     the netCDF variables. In Fortran, the unlimited
C     dimension must come last on the list of DIMIDS.
         DIMIDS(1) = LON_DIMID
         DIMIDS(2) = LAT_DIMID
         DIMIDS(3) = LVL_DIMID
         DIMIDS(4) = TIME_DIMID

C     Define the netCDF variable 
!        RETVAL = NF_DEF_VAR(NCID, CF_VAR_NAME(IV), 
         RETVAL = NF_DEF_VAR(NCID, USER_VAR,
     &                       NF_REAL, NDIMS, DIMIDS, 
     &                       VARID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     Turn on chunking.
!     CHUNKS(1) = NLATS
!     CHUNKS(2) = NLONS
!     CHUNKS(3) = 1
!     CHUNKS(4) = 1
!     RETVAL = nf_def_var_chunking(NCID, VARID, NF_CHUNKED, CHUNKS)
!     if (RETVAL .ne. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     Define compression level in case of NETCDF4 - chunks are disabled w/ compression

C     Define compression level in case of NETCDF4
         IF (DATASET_FORMAT .EQ. 'NETCDF4') THEN
             RETVAL = NF_DEF_VAR_DEFLATE(NCID, VARID, SHUFFLE, 
     &                 DEFLATE, DEFLATE_LEVEL)
             IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         ENDIF

C     Assign attributes to the netCDF variable.
         RETVAL = NF_PUT_ATT_TEXT(NCID, VARID, 
     &            'units', len(TRIM(CF_VAR_ITEMS_UNITS(ICODE,IV))),
     &            TRIM(CF_VAR_ITEMS_UNITS(ICODE,IV)))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, VARID, 
     &            'long_name', 
     &            len(TRIM(CF_VAR_ITEMS_LONG_NAME(ICODE,IV))), 
     &            TRIM(CF_VAR_ITEMS_LONG_NAME(ICODE,IV)))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, VARID, 
     &            'standard_name', 
     &            len(TRIM(CF_VAR_ITEMS_STANDARD_NAME(ICODE,IV))), 
     &            TRIM(CF_VAR_ITEMS_STANDARD_NAME(ICODE,IV))) 
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_ATT_TEXT(NCID, VARID, 
     &            'canonical_units', 
     &            len(TRIM(CF_VAR_ITEMS_CANONICAL_UNITS(ICODE,IV))), 
     &            TRIM(CF_VAR_ITEMS_CANONICAL_UNITS(ICODE,IV)))
         RETVAL = NF_PUT_ATT_TEXT(NCID, VARID, 
     &            'description', 
     &            len(TRIM(CF_VAR_ITEMS_DESCRIPTION(ICODE,IV))),
     &            TRIM(CF_VAR_ITEMS_DESCRIPTION(ICODE,IV)))
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     Assign other attributes
C   http://www.unidata.ucar.edu/software/netcdf/docs/netcdf/Attribute-Conventions.html#Attribute-Conventions
C   http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.6/cf-conventions.html
        ! time units
C     Assign missing value to variable
         RETVAL = NF_PUT_ATT_REAL(NCID, VARID, 'missing_value',
     &                NF_FLOAT, 1, VAL_MISSING)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     End define mode.
         RETVAL = NF_ENDDEF(NCID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

C     Write the coordinate variable data. This will put the latitudes
C     and longitudes of our data grid into the netCDF file.
 
         RETVAL = NF_PUT_VAR_REAL(NCID, LAT_VARID, LATS)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
         RETVAL = NF_PUT_VAR_REAL(NCID, LON_VARID, LONS)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
C     These settings tell netcdf to write one timestep of data. (The
C     setting of START(4) inside the loop below tells netCDF which
C     timestep to write.)

C CF-1.6 2.4. Dimensions - we write variables with T,Z,Y,X order

         COUNT(1) = NLONS
         COUNT(2) = NLATS
         COUNT(3) = NLVLS
         COUNT(4) = 1
         START(1) = 1
         START(2) = 1
         START(3) = 1
 
         
         DO IT = 1, NTOUT

            DO J = NY, 1, -1
               READ(LUN,'(A)') STRING
               IREC = IREC + 1
               CALL DECODEG(IREC,STRING,VAL_MISSING,PREC,NX,VALUES)
               IF (J .EQ. NY) THEN
                   VAL_MAX(IT) = VALUES(1)
               ENDIF
               DO IX = 1, NX
                  IF (VALUES(IX).GT.VAL_MAX(IT) ) THEN
                      VAL_MAX(IT) = VALUES(IX)
                  ENDIF 
                  ALLVAL(IX,J,1) = VALUES(IX)
               ENDDO 
            ENDDO

            IF (LDEBUG) THEN
                WRITE(6,*) 'IT, VAL_MAX:',IT, VAL_MAX(IT)
            ENDIF 

C           Write the data. 
            START(4) = IT
          RETVAL = NF_PUT_VARA_INT(NCID, TIME_VARID, START(4), COUNT(4),
     &        IT)
            IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
            RETVAL = NF_PUT_VARA_REAL(NCID, VARID, START, COUNT, 
     &        ALLVAL)
            IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)

         ENDDO
C     Close the file. This causes netCDF to flush all buffers and make
C     sure your data are really written to disk.
         RETVAL = NF_CLOSE(NCID)
         IF (RETVAL .NE. NF_NOERR) CALL HANDLE_ERR(RETVAL)
   
      ENDDO

999   CLOSE (LUN)

      IF (LDEBUG) THEN
          WRITE(6,*) "Error (no message is OK):",ERROR_MESSAGE
      ENDIF

C
      stop
500   WRITE (6,*) 'ERROR (ENS2NC_AQ)'
      WRITE (6,*) 'ENS2NC_AQ version: ', VERSION
      WRITE (6,*) 'Minimum required version: ', SOURCE_VERSION
      STOP
530   WRITE (6,*) 'ERROR (ENS2NC_AQ)'
      WRITE (6,*) 'ENS2NC_AQ version: ', VERSION
      WRITE (6,*) 'MODEL NUMBER NOT VALID'
      STOP
905   WRITE(6,*) ' ERROR (ENS2NC_AQ) - '//
     &           'The source file (',
     &        SOURCE_FILE(1:LSTRIM(SOURCE_FILE)),') cannot be opened.'
      WRITE (6,*) 'ENS2NC_AQ version: ', VERSION
      STOP
906   WRITE(6,*) ' ERROR (ENS2NC_AQ) - '//
     &           'The CF file (',
     &        CF_FILE(1:LSTRIM(CF_FILE)),') cannot be opened.'
      WRITE (6,*) 'ENS2NC_AQ version: ', VERSION
      STOP
960   WRITE(6,*) ' ERROR (ENS2NC_AQ) - '//
     &           'Cannot open input_file', 
     &           TMP_FILE
      WRITE (6,*) 'ENS2NC_AQ version: ', VERSION
      stop
C
      END
C
      INCLUDE '../nclib/handle_err.for'
      INCLUDE '../nclib/parse_cf_file.for'
      INCLUDE '../nclib/parse_cf_averaging.for'

      INCLUDE '../lib/fixname.for'
      INCLUDE '../lib/lstrim.for'
      INCLUDE '../lib/lstriml.for'
      INCLUDE '../lib/parse_averaging.for'
      INCLUDE '../lib/parse_metafile.for'
      INCLUDE '../lib/parse_item.for'
      INCLUDE '../lib/readrec.for'

      INCLUDE '../lib/decodeg.for'
