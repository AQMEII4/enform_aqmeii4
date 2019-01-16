C--------------------------------------------------------------ENFORM_AQ---
C
      PROGRAM ENFORM_AQ
C
C --- PURPOSE      : This program encodes the ASCII model output for the 
C                    Ensemble system (AIR QUALITY ON 2D/3D GRIDS)
C                    Model output can be optionally be processed before
C                    encoding in order to extract some statistics of interest.
C
C                    Please refer to the accompanying file ENFORM_AQ.README
C                    for details.
C
C --- COPYRIGHT    : EC JRC - http://ensemble.jrc.ec.europa.eu 2000
C                    All rights reserved. This software cannot be modified
C                    without agreement with the European Commission.
C
C --- CONTACT:     : Stefano Galmarini - stefano.galmarini@jrc.ec.europa.eu
C
C --- AUTHOR       : Roberto Bianconi - http://www.enviroware.com
C
C --- INPUT        : command line
C
C --- OUTPUT       : standard output
C
C --- CALLS        : FIND_NTO
C                    FIX24
C                    GETARG
C                    GENERATE_DATES
C                    ENCODEG
C                    FIX24
C                    PARSE_METAFILE
C
C --- INPUT SINTAX : ./enform_aq /path_to/metafile_file /path_to/model_output_file
C
C --- NOTES        : In case this program is compiled under Lahey Fortran for
C                    windows, uncomment the include of getarg.for at the bottom
C                    of this file.
C
C --- VERSION      : 
C                  :     (20130101-RBI) Fixed missing value assignment to averages,
C                                       including missing values
C                                       Added summary
C                  :     (20100616-RBI) Fixed check on array sizes (NXM,NYM,NTMAX)
C                  :     (20130620-RBI) Fixed some warning messages
C                  :     (20131004-RBI) Fixed FORMAT line 238
C                  :     (20190101-CHO) Allocatable arrays
C                                       MD (monthly diurnal) statisics added
C
C---------------------------------------------------------------ENFORM_AQ---
C
      IMPLICIT NONE
C
CC    INCLUDE '../inc/enform_aq.prm'
CC    INCLUDE '../inc/version.prm'
CC    INCLUDE '../inc/enform_aq.cmn'
      INCLUDE '../inc/enform_aq.prm'
      INCLUDE '../inc/version.prm'
      INCLUDE '../inc/enform_aq.cmn'
C
      INTEGER   READ_DATE(5)
C      REAL      VALUES_GR(NXM,NYM,NTMAX)
C      REAL      VALUES_OUT(NXM,NTMAX)
      REAL, ALLOCATABLE ::  VALUES_GR(:,:,:)
      REAL, ALLOCATABLE ::  VALUES_OUT(:,:)
      REAL      VALUES(NXM), ARR(NTMAX), WARR(NTMAX), SOMMA(NXM)
      CHARACTER STRING*8128, OUTFILE*256
      CHARACTER SOURCE_FILE*256, USER_FILE*256
      INTEGER   USER_METEO_DATE(5), OUTPUT_DATES_24(5)
      INTEGER   USER_MODEL_NUMBER, NTO
      CHARACTER USER_VAR*6, USER_RANDOM_KEY*7, AVGTYPE*3
      CHARACTER MDATE*12
      INTEGER   I, J, K, KK, LUN, LUS, LOUT, IT, LSTRIM, MVAL, IPOST
      INTEGER   ICOUNT, ITOUT, IAVGH, NTOUT, ITT, IFIRST, IVAL, ICURR
      INTEGER   ITBEG, ITEND, ITO, ITLOOP
      INTEGER   N, ICURRMD(NTMAX),ITBEGMD(NTMAX), ITENDMD(NTMAX)
      LOGICAL   LLAST, LOOP, LMATCHMD

      REAL VMAX_INFO

      LOGICAL LMISS

      REAL VAL_MISSING, PREC, VMAX
      INTEGER UI
      INTEGER ALLOCSTAT

      INTEGER IUNIT
      PARAMETER (IUNIT = 44)

      DATA LUN, LUS, LOUT /1,2,3/

      INTEGER IQUARTER(12)
      DATA IQUARTER /1,1,1,2,2,2,3,3,3,4,4,4/

C --- Try to allocate memory

      ALLOCATE ( VALUES_GR( NXM,NYM,NTMAX ),
     &           VALUES_OUT(NXM,NTMAX), STAT = ALLOCSTAT )
     
      IF ( ALLOCSTAT .NE. 0 ) THEN
          WRITE(6,*) 'Memory allocation failure'
	  STOP
      ELSE
        IF (LDEBUG) THEN
          WRITE(6,*)
          WRITE(6,*) 'Succesfully allocated memory...'
          WRITE(6,*)
        ENDIF
      ENDIF
      
      CALL GETARG(1,SOURCE_FILE)
      CALL GETARG(2,USER_FILE)

!     source_file = '9999-001.src'
!     user_file = 'original-9999-001-tsd.3.txt'

C --- Parse source file

      OPEN (LUS, FILE=SOURCE_FILE(1:LSTRIM(SOURCE_FILE)), STATUS='OLD',
     &             ERR=905)
      CALL PARSE_METAFILE(LUS)
      CLOSE(LUS)

C --- Check Enform version consistency, with backward compatibility
      IF (SOURCE_VERSION .EQ. VERSION ) THEN
                ! version is ok
      ELSE
          GOTO 500
      ENDIF


C --- Open user file

      IF (LDEBUG) THEN
          WRITE(6,*)
          WRITE(6,*) 'Reading user file (model output)...'
          WRITE(6,*)
      ENDIF

      OPEN (LUN, FILE=USER_FILE, STATUS='OLD',ERR=910)

      READ(LUN,'(I5)') USER_MODEL_NUMBER
      IF (LDEBUG) THEN
          WRITE(6,*) 'User model number ---> ',  USER_MODEL_NUMBER
      ENDIF

      MODEL_NUMBER = USER_MODEL_NUMBER

      READ(LUN,'(A7)') USER_RANDOM_KEY
      IF (LDEBUG) THEN
          WRITE(6,*) 'User random key ---> ',  USER_RANDOM_KEY
      ENDIF

C --- Check random_key

      IF (USER_RANDOM_KEY .NE. SOURCE_RANDOM_KEY) GOTO 510

      READ(LUN,'(A6)') USER_VAR
C
      CALL FIXNAME(USER_VAR)
      IF (LDEBUG) THEN
          WRITE(6,*) 'User variable name ---> ',  USER_VAR
      ENDIF

C --- Find variable index

      ICODE = 0
      DO I = 1, NUC_MAX
         IF (USER_VAR .EQ. SOURCE_VAR_NAME(I)) THEN
             ICODE = I
             IF (LDEBUG) THEN
!                WRITE(6,*) 'User variable index ---> ',  USER_VAR
                 WRITE(6,*) 'Variable index in metafile ---> ',  ICODE
             ENDIF
         ENDIF
      ENDDO
      IF (ICODE .EQ. 0) GOTO 610

      IF (SOURCE_VAR_NITEMS(ICODE) .EQ. 0) GOTO 620

C --- Read item index

      READ(LUN,*) UI
      IF (LDEBUG) THEN
          WRITE(6,*) 'User item index ---> ',  UI
      ENDIF

C --- Read missing value

      READ(LUN,*) VAL_MISSING
      IF (LDEBUG) THEN
          WRITE(6,*) 'User missing value ---> ',  VAL_MISSING
      ENDIF

C --- Read number of output times in user's file

C     READ(LUN,'(I4)') NT
      READ(LUN,*) NT
      IF (LDEBUG) THEN
          WRITE(6,*) 'Number of time outputs in model output ---> ',  NT
      ENDIF
      IF (NT .GT. NTMAX) GOTO 710
      IF (NT .GT. NOUTPUTS) GOTO 715
      IF (NT .LE. 0) GOTO 717

C --- Read meteo date in user's file

      READ(LUN,'(A12)') MDATE
      IF (LDEBUG) THEN
          WRITE(6,*) 'User meteo date ---> ',  MDATE
      ENDIF
      READ(MDATE,'(I4.4,4I2.2)') (USER_METEO_DATE(K),K=1,5)

C --- Compute output dates

      CALL GENERATE_DATES(SOURCE_FIRST_OUTPUT,OUTPUT_DATES,
     &                    DELTA_MINUTES,NT)

C --- Read and store values
      IF (LDEBUG) THEN
          WRITE(6,*) ' '
          WRITE(6,*) 'Reading model output data....'
          WRITE(6,*) ' '
      ENDIF
 
      DO IT = 1, NT

         !READ(LUN,'(I4.4,4I2.2))',ERR=920,END=930) (READ_DATE(K),K=1,5) LGI 20131004
         READ(LUN,'(I4.4,4I2.2)',ERR=920,END=930) (READ_DATE(K),K=1,5)
         IF (LDEBUG_VERBOSE) THEN
             WRITE(6,*) 'Reading date ',(READ_DATE(K),K=1,5)
         ENDIF

         DO K = 1, 5
            
            IF (READ_DATE(K) .NE. OUTPUT_DATES(K,IT)) GOTO 760
         ENDDO

         DO J = NY, 1, -1
            READ(LUN,*,ERR=940) (VALUES_GR(I,J,IT),I=1,NX)
         ENDDO

       ENDDO
      CLOSE (LUN)
      
C--------------------------------------------------------------------------------------
C Include aq_processing_block
      INCLUDE '../inc/aq_processing_block.for'
C--------------------------------------------------------------------------------------

      STOP

C --- Error messages

500   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'Installed ENFORM_AQ version  : ', VERSION
      WRITE (6,*) 'Required ENFORM_AQ version (or higher)  : ', 
     &             SOURCE_VERSION
      STOP
510   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'The user-provided random key does not match with the
     &one provided in the source file.' 
      WRITE (6,*) 'RANDOM KEY (source file):', SOURCE_RANDOM_KEY 
      WRITE (6,*) 'RANDOM KEY (user file)  :', USER_RANDOM_KEY 
      STOP
610   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'The user-provided var is not listed in the metafile.'
      WRITE (6,*) 'USER VAR :', USER_VAR
      STOP
620   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'The user-provided item number for current var ',
     &            'is not listed in the source file.'
      WRITE (6,*) 'USER VAR  :', USER_VAR
      WRITE (6,*) 'VAR CODE  :', ICODE
      WRITE (6,*) 'USER ITEM :', UI
      STOP
C
710   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'Time horizon exceeds parameter NTMAX (',NTMAX,'):',
     &NT
      STOP

715   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'Time horizon exceeds value in source term file ',
     &            ' NOUTPUTS (',NOUTPUTS,'):'
      STOP

717   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'Time horizon must be in the future ',
     &            ' NOUTPUTS (',NOUTPUTS,'):'
      STOP

720   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'Check your file headers'
      WRITE (6,*) 'Variable name or item index could be wrong.'
      STOP

760   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'Read date and'
     &                          //' expected date do not match.'
      WRITE (6,*)  'READ:     ',(READ_DATE(KK),KK=1,5)
      WRITE (6,*)  'EXPECTED: ',(OUTPUT_DATES(KK,IT),KK=1,5)
      STOP

905   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'The source file '//
     & SOURCE_FILE(1:LSTRIM(SOURCE_FILE))//' does not exist.'
      STOP

910   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'The user file',USER_FILE,' does not exist.'
      STOP

920   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'Data format is not correct.'
      STOP

930   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'File is uncomplete. Error reading date record'
      STOP

940   WRITE (6,*) 'ERROR (ENFORM_AQ)'
      WRITE (6,*) 'File is uncomplete. Error reading data record'
      STOP



      END

      INCLUDE '../lib/day_number.for'
      INCLUDE '../lib/encodeg.for'
      INCLUDE '../lib/find_nto.for'
      INCLUDE '../lib/fixname.for'
      INCLUDE '../lib/fix24.for'
      INCLUDE '../lib/generate_dates.for'
      INCLUDE '../lib/get_date.for'
      INCLUDE '../lib/leap_year.for'
      INCLUDE '../lib/lstrim.for'
      INCLUDE '../lib/lstriml.for'
      INCLUDE '../lib/parse_averaging.for'
      INCLUDE '../lib/parse_metafile.for'
      INCLUDE '../lib/parse_item.for'
      INCLUDE '../lib/readrec.for'
      INCLUDE '../lib/sort.for'
C     INCLUDE '../lib/getarg.for'
