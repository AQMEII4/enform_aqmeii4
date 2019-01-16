C
C-------------------------------------------------WRITE_STATUS_C_XML---
C
      SUBROUTINE WRITE_STATUS_C_XML(LXML)
C
C --- PURPOSE: This routine writes the status XML associated to the 
C              whole dataset
C
C --- AUTHOR:  R.Bianconi - ENVIROWARE SRL   
C
C --- INPUT:   LXML XML file unit
C
C --- OUTPUT:  XML file in status_c directory
C
C               <opt>
C                 <SequenceId>010</SequenceId>
C                 <CaseId>001</CaseId>
C                 <ModelNumber>01</ModelNumber>
C                 <ModelName>UK1</ModelName>
C                 <MeteoDate>200306110600</MeteoDate>
C                 <MeteoDelta>-6h0m</MeteoDelta>
C                 <LastUploadDate>200404270756</LastUploadDate>
C                 <LastUploadDelta>+7699h56m</LastUploadDelta>
C               </opt>
C
C --- CALLS:   none
C
C --- VERSION: October 17, 2005
C              April 22, 2008 - Updated for:
C                                   - models with 5 digits
C                                   - sequences with 4 digits
C
C-------------------------------------------------WRITE_STATUS_C_XML---
C
      INCLUDE '../inc/enform_aq.prm'
      INCLUDE '../inc/enform_aq.cmn'
C
      CHARACTER*12 M_DATE
      CHARACTER*20 ST
      WRITE (M_DATE( 1: 4),'(I4.4)') METEO_DATE(1)
      WRITE (M_DATE( 5: 6),'(I2.2)') METEO_DATE(2)
      WRITE (M_DATE( 7: 8),'(I2.2)') METEO_DATE(3)
      WRITE (M_DATE( 9:10),'(I2.2)') METEO_DATE(4)
      WRITE (M_DATE(11:12),'(I2.2)') METEO_DATE(5)
C
      WRITE(LXML,'(A)') '<opt>'

      ST = ''
      WRITE(ST,'(I4.4)') SOURCE_SEQUENCE_NUMBER
      WRITE(LXML,'(A)') '   <SequenceId>'//
     &                   ST(1:4)
     &                  // '</SequenceId>'

      ST = ''
      WRITE(ST,'(I3.3)') SOURCE_CASE_NUMBER
      WRITE(LXML,'(A)') '   <CaseId>'//
     &                   ST(1:3)
     &                  // '</CaseId>'

      ST = '' 
      WRITE (ST(1:5),'(I5.5)') MODEL_NUMBER
      WRITE(LXML,'(A)') '   <ModelNumber>'//
     &                   ST(1:5)
     &                  // '</ModelNumber>'

      ST = MODEL_NAME
      WRITE(LXML,'(A)') '   <ModelName>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</ModelName>'

      ST = M_DATE
      WRITE(LXML,'(A)') '   <MeteoDate>'//
     &                   ST(1:LSTRIM(ST))
     &                  // '</MeteoDate>'

      WRITE(LXML,'(A)') '   <MeteoDelta>'//
     &                   DELTA_M(1:LSTRIM(DELTA_M))
     &                  // '</MeteoDelta>'

      WRITE(LXML,'(A)') '   <LastUploadDate>'//
     &                   UPL_DATE
     &                  // '</LastUploadDate>'

      WRITE(LXML,'(A)') '   <LastUploadDelta>'//
     &                   DELTA_U(1:LSTRIM(DELTA_U))
     &                  // '</LastUploadDelta>'

      WRITE(LXML,'(A)') '</opt>'
C
      RETURN
C
      END
