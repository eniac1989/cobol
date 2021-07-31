      ***--------------------------------------------------------****
      *-- PROGRAM TITLE     : WARNING ACCOUNTS REPORT .(FTP FORMAT) *
      *-- WRITED BY         : D-RAHMANI .                        ---*
      *-- WRITED ON         : 1393/04/16.                        ---*
      *****-------------------------------------------------------***
       IDENTIFICATION         DIVISION.
       PROGRAM-ID.            RPTFT090.
       AUTHOR.                D-RAHMANI.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       IBM-ES9000.
       OBJECT-COMPUTER.       IBM-ES9000.
       INPUT-OUTPUT           SECTION.
       FILE-CONTROL.
           SELECT  OUT-FILE       ASSIGN  TO OTFILE.
      ****--------------------------------------------------------***
      ****---          D A T A     D I V I S I O N             ---***
      ****--------------------------------------------------------***
       DATA        DIVISION.
       FILE        SECTION.
       FD          OUT-FILE
                   LABEL  RECORD   STANDARD
                   RECORD CONTAINS 221 CHARACTERS
                   BLOCK  CONTAINS 0     CHARACTERS
                   DATA RECORD IS OUT-REC.
       01  OUT-REC                       PIC X(221).
      ****--------------------------------------------------------***
      ****---       S T O R A G E  S E C T I O N               ---***
      ****--------------------------------------------------------***
       WORKING-STORAGE SECTION.
           EXEC  SQL   INCLUDE SQLCA     END-EXEC.
      ****--------------------------------------------------------***
      ****---       F I R S T     F O R M A T                  ---***
      ****--------------------------------------------------------***
       01  WS-OUT-REC.
           02  TA-BSNS-UNT-CD           PIC X(04)       VALUE SPACE.
           02  FILLER                   PIC X           VALUE '|'.
           02  TA-B-U-NM                PIC X(30)       VALUE SPACE.
           02  FILLER                   PIC X           VALUE '|'.
           02  TA-ACNT-NO               PIC X(13)       VALUE SPACE.
           02  FILLER                   PIC X           VALUE '|'.
           02  TA-SHRT-DSC              PIC X(30)       VALUE SPACE.
           02  FILLER                   PIC X           VALUE '|'.
           02  TA-CSTMR-CD              PIC X(10)       VALUE SPACE.
           02  FILLER                   PIC X           VALUE '|'.
           02  TA-CSTMR-DSC             PIC X(66)       VALUE SPACE.    00231636
           02  FILLER                   PIC X           VALUE '|'.      00230915
           02  WS-GRNTNG-DT             PIC 9(08)       VALUE ZERO.
           02  FILLER                   PIC X           VALUE '|'.
           02  WS-OVRDU-INST-NO         PIC 9(04)       VALUE ZERO.
           02  FILLER                   PIC X           VALUE '|'.
           02  WS-OVRDU-INST-DT         PIC 9(08)       VALUE ZERO.
           02  FILLER                   PIC X           VALUE '|'.
           02  WS-OVRDU-AMNT            PIC 9(16)       VALUE ZERO.
           02  FILLER                   PIC X           VALUE '|'.
           02  WS-RMNDR-DT              PIC X(08)       VALUE SPACE.
           02  FILLER                   PIC X           VALUE '|'.
           02  WS-RMNDR-NO              PIC 9(04)       VALUE ZERO.
           02  FILLER                   PIC X           VALUE '|'.
           02  WS-OPRTNL-DT             PIC X(08)       VALUE SPACE.
      ****--------------------------------------------------------***
       77  TA-OVR-GL-CD             PIC X(08)        VALUE SPACE.
       77  TA-DBT-FCLTY-AMNT        PIC S9(16)V99   COMP-3 VALUE +0.
       77  TA-DBT-INSTL-NO          PIC S9(04) USAGE COMP.
      ****--------------------------------------------------------***
       77  TA-WARNING-DT            PIC X(10).
       77  TA-FCLTY-TYP-CD          PIC X(04)        VALUE SPACE.
       77  TA-CRNT-INSTL-NO         PIC S9(04) USAGE COMP.
      ***------------------------------------------------***
       77  TA-OPRTNL-DT             PIC X(10)        VALUE SPACE.
       77  TA-OVRDU-INST-DT         PIC X(10)        VALUE SPACE.
       77  TA-RMNDR-DT              PIC X(10)        VALUE SPACE.
       77  TA-OVRDU-AMNT            PIC S9(16)V99   COMP-3 VALUE +0.
       77  TA-RMNDR-NO              PIC S9(4) USAGE COMP.
       77  WS-SQLCODE               PIC -(09)9      VALUE ZERO.
       77  TA-STS                   PIC X(2)        VALUE SPACE.
       77  TA-GRNTNG-DT             PIC X(10)       VALUE SPACE.
       77  TA-OVRDU-INST-NO         PIC S9(9) USAGE COMP.
       77  WS-NUM-FIELD             PIC 9(3) VALUE 120.
       77  WS-ZERO-FIELD            PIC 9(3) VALUE 0.
       77  WS-GIV-FIELD             PIC 9(3) VALUE ZERO.
       77  WS-REMN-FIELD            PIC 9(3) VALUE ZERO.
       01  WS-COUNTER-IN            PIC 9(9) VALUE ZERO.
       01  WS-COUNTER-OUT           PIC 9(7) VALUE ZERO.
       01  WS-REMAINDER             PIC 9(7) VALUE ZERO.
       01  WS-OUTREM                PIC 9(7) VALUE ZERO.
      ***------------------------------------------------***
       77  TA-COUNT                 PIC S9(4)     COMP   VALUE ZERO.
      ***------------------------------------------------***
      ***      COMMON   VARIABLES                        ***
      ***------------------------------------------------***
       77  WS-NULL-01               PIC S9(4) USAGE COMP.
      ***------------------------------------------------------------**
       77  TA-ERR-DSC                 PIC X(30).
       77  WS-SQLCODE0                PIC -9(10).
       77  WS-PRGRM-CD                PIC X(08).
       01  LK-ARPTCNTL-FIELDS.
           02  LK-PRGRM-CD            PIC X(08).
           02  LK-OPRTNL-DT           PIC X(10).
      ***------------------------------------------------------------***
       01  LK-AGEDTEDT-FIELDS.
           02  LK-AGEDTEDT-JOB        PIC X(01).
           02  LK-AGEDTEDT-DATE8      PIC X(08).
           02  LK-AGEDTEDT-DATE10     PIC X(10).
      ***-----------------------------------------------------------****
       01  LK-AGEDTCNV-FIELDS.                                          01550000
          02  WS-CODE-DT                  PIC X(1).                     01550000
          02  WS-GREG-DT                  PIC X(8).                     01550000
          02  WS-SHMS-DT                  PIC X(8).                     01550000
      ***------------------------------------------------***
      ***         COMMON VARIABLES                       ***
      ***------------------------------------------------***
       01  WS-INPUT-DT         PIC  X(10) .
       01  WS-STRT-DT          PIC  X(08) .
      ****------------------------------********
      ****  COMMON FUNCTION BGEDTEDT        ****
      ****------------------------------********
       01  LK-BGEDTEDT-FIELDS.
          02  WS-JOB                      PIC X     VALUE SPACE.
          02  WS-DATE8                    PIC X(8)  VALUE SPACE.
          02  WS-DATE10                   PIC X(10) VALUE SPACE.
      ***------------------------------------------------------------***
       01  WS-AGEFCAQ0-FIELDS.
           02  LS-ACCOUNT-NO       PIC X(13).
           02  LS-CUSTOMER-CODE    PIC X(10).
           02  LS-CUSTOMER-DESC    PIC X(66).
           02  LS-ERRMSGS-DESC     PIC X(50).
      ****--------------------------------------------------------***
      ****---          DECLARE CURSOR CS-TRMNDPST               ---***
      ****--------------------------------------------------------***
           EXEC SQL DECLARE CS-TRMNDPST    CURSOR FOR
                SELECT   BSNS_UNT_CD
                       , VALUE(B_U_NM,'     ')
                       , D.ACNT_NO
                       , SHRT_DSC
                       , CSTMR_CD
                       , GRNTNG_DT
                       , OVRDU_INST_NO
                       , OVRDU_INST_DT
                       , OVRDU_AMNT
                       , RMNDR_DT
                       , RMNDR_NO
                FROM TSMFACTP A, TBUBUNIT B,TRMNDPST C,TGFFACAC D
                                                      ,TCFFCTTL E
                WHERE (C.ACNT_NO BETWEEN '6100000000000'
                                 AND     '6899999999999'
                       OR
 DR                    C.ACNT_NO BETWEEN '7000000000000'
 DR                              AND     '7099999999999')
                AND   RMNDR_DT      >= :TA-WARNING-DT
                AND   BSNS_UNT_CD    = B_U_CD
                AND   A.FCLTY_TYP_CD = D.FCLTY_TYP_CD
                AND   C.ACNT_NO      = D.ACNT_NO
                AND   C.ACNT_NO      = E.ACNT_NO
                AND   E.TL_CD        = '001'
           END-EXEC.
      ****--------------------------------------------------------***
      ****---     P R O C E D U R E   D I V I S O N            ---***
      ****--------------------------------------------------------***
       PROCEDURE DIVISION.
      *    READY TRACE.
           EXEC SQL WHENEVER SQLERROR GO TO H0000-SQL-ERROR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING     CONTINUE        END-EXEC.
           EXEC SQL WHENEVER NOT FOUND      CONTINUE        END-EXEC.
      ****--------------------------------------------------------***
      ****---     M A I N     P R O G R A M                    ---***
      ****--------------------------------------------------------***
       A0000-MAIN-PROGRAM.
           DISPLAY ' ***-------------------------------*** '.
           DISPLAY ' ***   S  T   A   R   T            *** '.
           DISPLAY ' ***-------------------------------*** '.
           ACCEPT WS-STRT-DT.
           PERFORM   A0500-INPUT-DATE-CONTROL.
           OPEN    OUTPUT OUT-FILE .
           DISPLAY ' ***-------------------------------*** '.           02940000
           DISPLAY ' ***  OPRTNL_DT  = ' WS-OPRTNL-DT '        *** '.   02950000
           DISPLAY ' ***-------------------------------*** '.           02960000
           PERFORM C1000-INITIALIZE.
           EXEC    SQL OPEN CS-TRMNDPST END-EXEC.
           DISPLAY ' ***-------------------------------*** '.
           DISPLAY ' ***  A F T E R  O P E N  CURSOR   *** '.
           DISPLAY ' ***-------------------------------*** '.
           PERFORM A2000-FETCH-TRMNDPST.
           PERFORM A3000-GENERATE-DETAIL UNTIL SQLCODE = 100.
           DISPLAY   ' *** NUMBER OF RECORDS =' WS-COUNTER-IN
                     '  ***'
           EXEC    SQL CLOSE CS-TRMNDPST END-EXEC.
           CLOSE   OUT-FILE.
           DISPLAY ' ***-------------------------------*** '.
           DISPLAY ' ***  N O R M A L   E N D          *** '.
           DISPLAY ' ***-------------------------------*** '.
           STOP    RUN.
       A0000-MAIN-PROGRAM-END. EXIT.
      ***--------------------------------------------------------***
      ***    INPUT DATE CONTROL                                  --*
      ***--------------------------------------------------------***
       A0500-INPUT-DATE-CONTROL.
              PERFORM A0600-MAKE-OPRATION-DT
           IF WS-STRT-DT = SPACE
              MOVE   TA-WARNING-DT TO    WS-DATE10                      00410001
              PERFORM  E0003-CHANG-10TO8-DATE
              MOVE   WS-DATE8        TO WS-GREG-DT                      00410001
              PERFORM  E0005-MILADY-TO-SHAMSY
              MOVE   WS-SHMS-DT      TO WS-DATE8
              PERFORM E0002-CHANGE-8TO10-DATE                           03820053
              MOVE WS-DATE10         TO WS-INPUT-DT                     00410001
           ELSE IF WS-STRT-DT IS NUMERIC
              MOVE   WS-STRT-DT      TO WS-SHMS-DT                      00410001
                                        WS-OPRTNL-DT
                                        WS-RMNDR-DT
              PERFORM  E0004-SHAMSY-TO-MILADY
              MOVE   WS-GREG-DT      TO WS-DATE8
              PERFORM E0002-CHANGE-8TO10-DATE                           03820053
              MOVE WS-DATE10         TO TA-WARNING-DT                   00410001
           ELSE IF WS-STRT-DT IS NOT NUMERIC
              DISPLAY ' ***-------------------------------*** '
              DISPLAY ' ***  INPUT   DATE     IS   WRONG  *** '
              DISPLAY ' ***  PROGRAM ABNORMAL TERMINATED  *** '
              DISPLAY ' ***-------------------------------*** '
              STOP    RUN.
       A0500-INPUT-DATE-CONTROL-EN.
      ***--------------------------------------------------------***
       A0600-MAKE-OPRATION-DT.
              EXEC SQL
                SELECT MAX(OPRTNL_DT) INTO :TA-WARNING-DT
                FROM   TSMSYSTM
                WHERE  STS = '03'
              END-EXEC.
              MOVE   TA-WARNING-DT TO    WS-DATE10                      00410001
              PERFORM  E0003-CHANG-10TO8-DATE
              MOVE   WS-DATE8        TO WS-GREG-DT                      00410001
              PERFORM  E0005-MILADY-TO-SHAMSY
              MOVE   WS-SHMS-DT      TO WS-OPRTNL-DT WS-RMNDR-DT.
       A0600-MAKE-OPRATION-DT-END.
      ***--------------------------------------------------------***
       C1000-INITIALIZE.
           MOVE LOW-VALUE TO    OUT-REC
           MOVE SPACE TO
                TA-BSNS-UNT-CD
                TA-ACNT-NO
                TA-CSTMR-CD
                TA-CSTMR-DSC
                TA-B-U-NM
                TA-SHRT-DSC
                TA-OVRDU-INST-DT
                TA-RMNDR-DT.
           MOVE ZERO TO
                TA-GRNTNG-DT
                TA-OVRDU-INST-NO
                TA-OVRDU-AMNT
                TA-RMNDR-NO.
       C1000-INITIALIZE-END.
      ***--------------------------------------------------------***
      ***    GRAPH FOR FETCH A RECORD INTO CS-TRMNDPST CURSOR    --*
      ***--------------------------------------------------------***
       A2000-FETCH-TRMNDPST.
           EXEC SQL FETCH CS-TRMNDPST
                INTO     :TA-BSNS-UNT-CD
                       , :TA-B-U-NM
                       , :TA-ACNT-NO
                       , :TA-SHRT-DSC
                       , :TA-CSTMR-CD
                       , :TA-GRNTNG-DT
                       , :TA-OVRDU-INST-NO
                       , :TA-OVRDU-INST-DT
                       , :TA-OVRDU-AMNT
                       , :TA-RMNDR-DT
                       , :TA-RMNDR-NO
           END-EXEC.
           IF SQLCODE = 0
             MOVE  TA-GRNTNG-DT       TO  WS-DATE10                     00410001
             PERFORM  E0003-CHANG-10TO8-DATE
             MOVE   WS-DATE8        TO WS-GREG-DT                       00410001
             PERFORM  E0005-MILADY-TO-SHAMSY
             MOVE   WS-SHMS-DT      TO WS-GRNTNG-DT

             MOVE  TA-OVRDU-INST-NO   TO  WS-OVRDU-INST-NO

             MOVE  TA-OVRDU-INST-DT   TO  WS-DATE10                     00410001
             PERFORM  E0003-CHANG-10TO8-DATE
             MOVE   WS-DATE8        TO WS-GREG-DT                       00410001
             PERFORM  E0005-MILADY-TO-SHAMSY
             MOVE   WS-SHMS-DT      TO WS-OVRDU-INST-DT                 00410001

             MOVE  TA-OVRDU-AMNT      TO  WS-OVRDU-AMNT

             PERFORM  A5000-CSTMR-NAME-FAMILY.

             MOVE  TA-RMNDR-NO        TO WS-RMNDR-NO.
       A2000-FETCH-TRMNDPST-END.
      ***------------------------------------------------***
      ***    GRAPH FOR GENERATE DETAIL FOR ANY ROW       ***
      ***------------------------------------------------***
       A5000-CSTMR-NAME-FAMILY.
           MOVE TA-ACNT-NO    TO  LS-ACCOUNT-NO.                        02520003
           MOVE SPACE         TO  LS-CUSTOMER-CODE.                     02520003
           MOVE SPACE         TO  LS-CUSTOMER-DESC.                     02520003
           CALL 'AGEFCAQ0' USING  WS-AGEFCAQ0-FIELDS.                       0079
           IF    LS-ERRMSGS-DESC  NOT =  SPACE
               MOVE SPACE               TO TA-CSTMR-DSC
           ELSE
               MOVE LS-CUSTOMER-DESC    TO TA-CSTMR-DSC.
       A5000-CSTMR-NAME-FAMILY-E.
      ***------------------------------------------------***
      ***    GRAPH FOR GENERATE DETAIL FOR ANY ROW       ***
      ***------------------------------------------------***
       A3000-GENERATE-DETAIL.
           ADD  1         TO WS-COUNTER-IN.
           DIVIDE  WS-COUNTER-IN BY 100000 GIVING WS-OUTREM
                           REMAINDER WS-REMAINDER .
           IF WS-REMAINDER = ZERO AND WS-COUNTER-IN > 0
              DISPLAY 'NUMBER OF RECORD FETCHED: '  WS-COUNTER-IN .

           MOVE  WS-OUT-REC       TO   OUT-REC.
           WRITE OUT-REC.
           PERFORM C1000-INITIALIZE.
           PERFORM A2000-FETCH-TRMNDPST.
       A3000-GENERATE-DETAIL-END.
      ****------------------------------------********                  09760000
      **      GRAPH CHANGED SHAMSY  TO MILADY DATE ***                  09770000
      ****------------------------------------********                  09780000
       E0004-SHAMSY-TO-MILADY.                                          09790000
           MOVE '0'         TO WS-CODE-DT.                              09800000
           CALL 'AGEDTCNV' USING  LK-AGEDTCNV-FIELDS.                   01550000
       E0004-SHAMSY-TO-MILADY-EXIT.                                     09830000
      ****------------------------------------********                  09840000
      **      GRAPH CHANGED MILADY  TO SHAMSY DATE ***                  09850000
      ****------------------------------------********                  09860000
       E0005-MILADY-TO-SHAMSY.                                          09870000
           MOVE '1'         TO WS-CODE-DT.                              09880000
           CALL 'AGEDTCNV' USING  LK-AGEDTCNV-FIELDS.                   01550000
       E0005-MILADY-TO-SHAMSY-EXIT.                                     09910000
      *-------------------------------------------------*               03830000
      *--    GRAPH FOR CHANGE DATE FROM 8 TO 10       --*               03830000
      *-------------------------------------------------*               03830000
       E0002-CHANGE-8TO10-DATE.                                         03820053
           MOVE  '-'    TO  WS-JOB.                                     02840052
           CALL 'AGEDTEDT' USING  WS-JOB WS-DATE8 WS-DATE10.            03470000
       E0002-CHANGE-8TO10-DATE-END.                                     03820053
      *-------------------------------------------------*               03830000
       E0003-CHANG-10TO8-DATE.                                          03820053
           MOVE  ' '    TO  WS-JOB.                                     02840052
           CALL 'AGEDTEDT' USING  WS-JOB WS-DATE8 WS-DATE10.            03470000
       E0003-CHANG-10TO8-DATE-END.                                      03820053
      ***------------------------------------------------------------***
      ***         S Q L E R R O R      G R A P H                     ***
      ***------------------------------------------------------------***
       H0000-SQL-ERROR.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           MOVE  SQLCODE   TO  WS-SQLCODE0 .
           DISPLAY ' ***-------------------------------*** '.           05920000
           DISPLAY ' ***  SQLERROR CODE = ' WS-SQLCODE0 ' *** '.        05930000
           DISPLAY ' ***  ACNT_NO = ' TA-ACNT-NO        ' *** '.        05940000
           DISPLAY ' SQLERRMC CODE = ' SQLERRMC         ' *** '.
           DISPLAY ' ***-------------------------------*** '.           05950000
           STRING  TA-ACNT-NO    WS-SQLCODE0  DELIMITED BY SIZE
                   INTO  TA-ERR-DSC.
           CLOSE     OUT-FILE.                                          05970000
           EXEC  SQL CLOSE CS-TRMNDPST END-EXEC.                        00739048
           DIVIDE   WS-NUM-FIELD BY        WS-ZERO-FIELD
           GIVING   WS-GIV-FIELD REMAINDER WS-REMN-FIELD.
           STOP RUN.
       H0000-RET-ERROR-END.
