00001  IDENTIFICATION DIVISION.                                         04/24/95
00002  PROGRAM-ID.     APKRP240.                                        APKRP240
00003  AUTHOR.         MARY KING.                                          LV001
00004  DATE-WRITTEN.   03/16/1995.                                      APKRP240
00005  DATE-COMPILED.                                                   APKRP240
00006 *************************************************************     APKRP240
00007 *    COBOL 2 WITH SQL                                       *     APKRP240
00008 *         --- REPORT ON VENDOR SPECIFICATION ENTRIES ---    *     APKRP240
00009 *                                                           *     APKRP240
00010 *    THIS PROGRAM REPORTS THE CONTENTS OF THE AP VENDOR     *     APKRP240
00011 *    SPECIFICATION TABLE.                                   *     APKRP240
00012 *                                                           *     APKRP240
00013 *                                                           *     APKRP240
00014 * INPUT:                                                    *     APKRP240
00015 *  1. AP VENDOR SPECIFICATIONS TABLE   (TAPVNDS)            *     APKRP240
00016 *  2. EXTERNAL ENTERPRISE NAME         (TENTNME)            *     APKRP240
00017 *  3. EXTERNAL ENTITY ID TABLE         (TEXTENT)            *     APKRP240
00018 *                                                           *     APKRP240
00019 * OUTPUT:                                                   *     APKRP240
00020 *  1. AP VENDOR SPECIFICATIONS REPORT                       *     APKRP240
00021 *                                                           *     APKRP240
00022 *************************************************************     APKRP240
00023 *---------------------------------------------------------------* APKRP240
00024 *                                                               * APKRP240
00025 * CHANGE LOG:                                                   * APKRP240
00026 *                                                               * APKRP240
00027 *---------------------------------------------------------------* APKRP240
00028    EJECT                                                          APKRP240
00029  ENVIRONMENT DIVISION.                                            APKRP240
00030  CONFIGURATION SECTION.                                           APKRP240
00031  SOURCE-COMPUTER.    IBM-3090.                                    APKRP240
00032  OBJECT-COMPUTER.    IBM-3090.                                    APKRP240
00033                                                                   APKRP240
00034  INPUT-OUTPUT SECTION.                                            APKRP240
00035  FILE-CONTROL.                                                    APKRP240
00036      SELECT VENDOR-SPEC-RPT-FILE ASSIGN TO UT-S-RPT01.            APKRP240
00037  EJECT                                                            APKRP240
00038  DATA DIVISION.                                                   APKRP240
00039  FILE SECTION.                                                    APKRP240
00040                                                                   APKRP240
00041  FD  VENDOR-SPEC-RPT-FILE                                         APKRP240
00042      RECORDING MODE IS F                                          APKRP240
00043      LABEL RECORDS ARE STANDARD                                   APKRP240
00044      BLOCK CONTAINS 0 RECORDS                                     APKRP240
00045      DATA RECORD IS VENDOR-SPEC-RPT-REC.                          APKRP240
00046  01  VENDOR-SPEC-RPT-REC.                                         APKRP240
00047      05  FILLER                       PIC  X(132).                APKRP240
00048                                                                   APKRP240
00049  EJECT                                                            APKRP240
00050  WORKING-STORAGE SECTION.                                         APKRP240
00051                                                                   APKRP240
00052  01  PV-PROGRAM-VARIABLES.                                        APKRP240
00053      05  FILLER                  PIC  X(30)  VALUE                APKRP240
00054          '** BEGINING OF APKRP240 W/S **'.                        APKRP240
00055      05  PV-CURRENT-PARAGRAPH    PIC  X(35).                      APKRP240
00056      05  PV-PROGRAM-NAME         PIC  X(08)  VALUE 'APKRP240'.    APKRP240
00057      05  PV-CURRENT-DATE.                                         APKRP240
00058          10  PV-CURRENT-YEAR     PIC  9(02)  VALUE ZEROS.         APKRP240
00059          10  PV-CURRENT-MONTH    PIC  9(02)  VALUE ZEROS.         APKRP240
00060          10  PV-CURRENT-DAY      PIC  9(02)  VALUE ZEROS.         APKRP240
00061      05  PV-CURRENT-TIME.                                         APKRP240
00062          10  PV-CURRENT-HOUR     PIC  9(02)  VALUE ZEROS.         APKRP240
00063          10  PV-CURRENT-MINUTE   PIC  9(02)  VALUE ZEROS.         APKRP240
00064          10  FILLER              PIC  9(02)  VALUE ZEROS.         APKRP240
00065                                                                   APKRP240
00066                                                                   APKRP240
00067  01  PS-PROGRAM-SWITCHES.                                         APKRP240
00068      05  PS-OUT-OF-VENDORS-SW        PIC X(01) VALUE 'M'.         APKRP240
00069          88  OUT-OF-VENDORS                    VALUE 'O'.         APKRP240
00070          88  MORE-VENDORS                      VALUE 'M'.         APKRP240
00071                                                                   APKRP240
00072                                                                   APKRP240
00073  01  PC-PROGRAM-COUNTERS.                                         APKRP240
00074      05  PC-RETRY-MAX            PIC S9(04)  VALUE 3              APKRP240
00075                                              COMP SYNC.           APKRP240
00076      05  PC-RETRY-COUNT          PIC S9(04)  VALUE ZEROS          APKRP240
00077                                              COMP SYNC.           APKRP240
00078      05  PC-MAX-LINES            PIC S9(03)  VALUE +60            APKRP240
00079                                              COMP-3.              APKRP240
00080      05  PC-LINE-COUNT           PIC S9(03)  VALUE +99            APKRP240
00081                                              COMP-3.              APKRP240
00082      05  PC-PAGE-COUNT           PIC S9(05)  VALUE ZEROS          APKRP240
00083                                              COMP-3.              APKRP240
00084      05  PC-VENDOR-SPEC-COUNT    PIC S9(05)  VALUE ZERO           APKRP240
00085                                              COMP-3.              APKRP240
00086  EJECT                                                            APKRP240
00087 *----------------------------------------------------------------*APKRP240
00088 *  STANDARD HEADER LAYOUT                                        *APKRP240
00089 *----------------------------------------------------------------*APKRP240
00090      COPY DPWS132O.                                               APKRP240
00091                                                                   APKRP240
00092                                                                   APKRP240
00093  01  H2-HEADER2.                                                  APKRP240
00094      05  FILLER                  PIC  X(50) VALUE SPACES.         APKRP240
00095      05  FILLER                  PIC  X(82) VALUE                 APKRP240
00096          'VENDOR TOLERANCE SPECIFICATIONS'.                       APKRP240
00097                                                                   APKRP240
00098  01  DH-DETAIL-HEADER1.                                           APKRP240
00099      05  FILLER                  PIC   X(66) VALUE SPACES.        APKRP240
00100      05  FILLER                  PIC   X(08) VALUE 'VARIANCE'.    APKRP240
00101      05  FILLER                  PIC   X(04) VALUE SPACES.        APKRP240
00102      05  FILLER                  PIC   X(08) VALUE 'VARIANCE'.    APKRP240
00103      05  FILLER                  PIC   X(46) VALUE SPACES.        APKRP240
00104                                                                   APKRP240
00105  01  DH-DETAIL-HEADER2.                                           APKRP240
00106      05  FILLER                  PIC   X(21) VALUE SPACES.        APKRP240
00107      05  FILLER                  PIC   X(11) VALUE 'VENDOR NAME'. APKRP240
00108      05  FILLER                  PIC   X(25) VALUE SPACES.        APKRP240
00109      05  FILLER                  PIC   X(06) VALUE 'DUNSNO'.      APKRP240
00110      05  FILLER                  PIC   X(03) VALUE SPACES.        APKRP240
00111      05  FILLER                  PIC   X(07) VALUE 'PERCENT'.     APKRP240
00112      05  FILLER                  PIC   X(06) VALUE SPACES.        APKRP240
00113      05  FILLER                  PIC   X(06) VALUE 'AMOUNT'.      APKRP240
00114      05  FILLER                  PIC   X(04) VALUE SPACES.        APKRP240
00115      05  FILLER                  PIC   X(03) VALUE 'IND'.         APKRP240
00116      05  FILLER                  PIC   X(40) VALUE SPACES.        APKRP240
00117                                                                   APKRP240
00118  01  DL-DETAIL-LINE.                                              APKRP240
00119      05  FILLER                  PIC   X(21)   VALUE SPACES.      APKRP240
00120      05  DL-VENDOR-NAME          PIC   X(30)   VALUE SPACES.      APKRP240
00121      05  FILLER                  PIC   X(03)   VALUE SPACES.      APKRP240
00122      05  DL-DUN-NBR              PIC   Z(09)   VALUE SPACES.      APKRP240
00123      05  FILLER                  PIC   X(03)   VALUE SPACES.      APKRP240
00124      05  DL-INVC-RCV-VAR-PCT     PIC   ZZZ.99  VALUE SPACES.      APKRP240
00125      05  DL-PCT-IND              PIC   X(01)   VALUE SPACES.      APKRP240
00126      05  FILLER                  PIC   X(05)   VALUE SPACES.      APKRP240
00127      05  DL-INVC-RCV-VAR-AMT     PIC   $$$$.99 VALUE SPACES.      APKRP240
00128      05  FILLER                  PIC   X(05)   VALUE SPACES.      APKRP240
00129      05  DL-INVC-RCV-VAR-IND     PIC   X(01)   VALUE SPACES.      APKRP240
00130      05  FILLER                  PIC   X(41)   VALUE SPACES.      APKRP240
00131  EJECT                                                            APKRP240
00132 *----------------------------------------------------------------*APKRP240
00133 *  COMMUNICATIONS AREA2 FOR DB2                                  *APKRP240
00134 *----------------------------------------------------------------*APKRP240
00135      COPY SQLCA2.                                                 APKRP240
00136                                                                   APKRP240
00137                                                                   APKRP240
00138      EXEC SQL                                                     APKRP240
00139           INCLUDE SQLCA                                           APKRP240
00140      END-EXEC.                                                    APKRP240
00141  EJECT                                                            APKRP240
00142 *----------------------------------------------------------------*APKRP240
00143 *  ACCOUNTS PAYABLE VENDOR SPECIFICATIONS TABLE                  *APKRP240
00144 *----------------------------------------------------------------*APKRP240
00145      EXEC SQL                                                     APKRP240
00146           INCLUDE TAPVNDS                                         APKRP240
00147      END-EXEC.                                                    APKRP240
00148  EJECT                                                            APKRP240
00149 *----------------------------------------------------------------*APKRP240
00150 *  EXTERNAL ENTERPRISE NAME TABLE                                *APKRP240
00151 *----------------------------------------------------------------*APKRP240
00152      EXEC SQL                                                     APKRP240
00153           INCLUDE TENTNME                                         APKRP240
00154      END-EXEC.                                                    APKRP240
00155  EJECT                                                            APKRP240
00156 *----------------------------------------------------------------*APKRP240
00157 *  EXTERNAL ENTITY ID TABLE                                      *APKRP240
00158 *----------------------------------------------------------------*APKRP240
00159      EXEC SQL                                                     APKRP240
00160           INCLUDE TEXTENT                                         APKRP240
00161      END-EXEC.                                                    APKRP240
00162  EJECT                                                            APKRP240
00163 *----------------------------------------------------------------*APKRP240
00164 *   VENDOR_SPEC_CSR GETS ALL ROWS ON THE APVENDS TABLE.          *APKRP240
00165 *----------------------------------------------------------------*APKRP240
00166      EXEC SQL                                                     APKRP240
00167        DECLARE VENDOR_SPEC_CSR CURSOR FOR                         APKRP240
00168          SELECT                                                   APKRP240
00169              EN.ENT_NAME_DESC                                     APKRP240
00170             ,EE.DUN_NBR                                           APKRP240
00171             ,VS.INVC_RCV_VAR_PCT                                  APKRP240
00172             ,VS.INVC_RCV_VAR_AMT                                  APKRP240
00173             ,VS.INVC_RCV_VAR_IND                                  APKRP240
00174          FROM                                                     APKRP240
00175              TENTNME EN                                           APKRP240
00176             ,TEXTENT EE                                           APKRP240
00177             ,TAPVNDS VS                                           APKRP240
00178          WHERE EN.ENT_ID         = VS.ENT_ID                      APKRP240
00179            AND EN.ENT_ID         = EE.ENT_ID                      APKRP240
00180            AND EN.TYPE_CDE       = '01'                           APKRP240
00181          ORDER BY EN.ENT_NAME_DESC                                APKRP240
00182                  ,INVC_RCV_VAR_IND                                APKRP240
00183      END-EXEC.                                                    APKRP240
00184  EJECT                                                            APKRP240
00185 *----------------------------------------------------------------*APKRP240
00186 *  ABEND DATA AREAS                                              *APKRP240
00187 *----------------------------------------------------------------*APKRP240
00188  01  ABEND-CODE                  PIC S9(04)  VALUE ZEROS          APKRP240
00189                                              COMP SYNC.           APKRP240
00190      88  AC-BAD-DATA                         VALUE +4003.         APKRP240
00191      88  AC-DB2-ERROR                        VALUE +4013.         APKRP240
00192                                                                   APKRP240
00193                                                                   APKRP240
00194  01  ABEND-AREAS.                                                 APKRP240
00195      05  AA-ABEND-LIT            PIC  X(40)  VALUE                APKRP240
00196              '*****       ABEND'.                                 APKRP240
00197      05  AA-PROGRAM-LIT          PIC  X(40)  VALUE                APKRP240
00198              '*****   PROGRAM: APKRP240'.                         APKRP240
00199      05  AA-PARAGRAPH-LIT.                                        APKRP240
00200          10  FILLER              PIC  X(17)  VALUE                APKRP240
00201              '***** PARAGRAPH: '.                                 APKRP240
00202          10  AA-PARAGRAPH-NAME   PIC  X(35)  VALUE SPACES.        APKRP240
00203      05  AA-MESSAGE-LINE.                                         APKRP240
00204          10  FILLER              PIC  X(06)  VALUE '*****'.       APKRP240
00205          10  AA-MESSAGE          PIC  X(127) VALUE SPACES.        APKRP240
00206      05  AA-DB2-ERROR-LIT        PIC  X(40)  VALUE                APKRP240
00207              '*****    DB2 ERROR'.                                APKRP240
00208      05  AA-DB2-OPERATION-LIT.                                    APKRP240
00209          10  FILLER              PIC  X(17)  VALUE                APKRP240
00210              '***** OPERATION: '.                                 APKRP240
00211          10  AA-DB2-OPERATION.                                    APKRP240
00212              15  AA-DB2-OP-1     PIC  X(25)  VALUE SPACES.        APKRP240
00213              15  AA-DB2-OP-2     PIC  X(25)  VALUE SPACES.        APKRP240
00214      05  AA-DB2-TABLE-1          PIC  X(08)  VALUE SPACES.        APKRP240
00215      05  AA-DB2-TABLE-2          PIC  X(08)  VALUE SPACES.        APKRP240
00216      05  AA-DB2-TABLE-3          PIC  X(08)  VALUE SPACES.        APKRP240
00217      05  AA-DB2-TABLE-4          PIC  X(08)  VALUE SPACES.        APKRP240
00218      05  AA-DB2-TABLE-5          PIC  X(08)  VALUE SPACES.        APKRP240
00219                                                                   APKRP240
00220                                                                   APKRP240
00221      COPY DPWS004.                                                APKRP240
00222  01  FILLER                      PIC  X(25)  VALUE                APKRP240
00223      '** END OF APKRP240 W/S **'.                                 APKRP240
00224  EJECT                                                            APKRP240
00225  PROCEDURE DIVISION.                                              APKRP240
00226  A100-MAIN.                                                       APKRP240
00227                                                                   APKRP240
00228      MOVE 'A100-MAIN' TO PV-CURRENT-PARAGRAPH.                    APKRP240
00229                                                                   APKRP240
00230      PERFORM B100-INITIALIZE.                                     APKRP240
00231                                                                   APKRP240
00232      PERFORM B200-PREPARE-REPORT                                  APKRP240
00233          UNTIL OUT-OF-VENDORS.                                    APKRP240
00234                                                                   APKRP240
00235      PERFORM B300-END-PROGRAM.                                    APKRP240
00236                                                                   APKRP240
00237      GOBACK.                                                      APKRP240
00238  EJECT                                                            APKRP240
00239 *----------------------------------------------------------------*APKRP240
00240 *    INITIALIZATION PROCESSING                                   *APKRP240
00241 *    CALLED FROM A100-MAIN                                       *APKRP240
00242 *----------------------------------------------------------------*APKRP240
00243  B100-INITIALIZE.                                                 APKRP240
00244                                                                   APKRP240
00245      MOVE 'B100-INITIALIZE' TO PV-CURRENT-PARAGRAPH.              APKRP240
00246                                                                   APKRP240
00247      OPEN OUTPUT VENDOR-SPEC-RPT-FILE.                            APKRP240
00248                                                                   APKRP240
00249      INITIALIZE DCLTAPVNDS                                        APKRP240
00250                 DCLTEXTENT                                        APKRP240
00251                 DCLTENTNME.                                       APKRP240
00252                                                                   APKRP240
00253      ACCEPT PV-CURRENT-DATE FROM DATE.                            APKRP240
00254      MOVE PV-CURRENT-MONTH            TO DP132O-RUN-MONTH.        APKRP240
00255      MOVE PV-CURRENT-DAY              TO DP132O-RUN-DAY.          APKRP240
00256      MOVE PV-CURRENT-YEAR             TO DP132O-RUN-YEAR.         APKRP240
00257                                                                   APKRP240
00258      ACCEPT PV-CURRENT-TIME FROM TIME.                            APKRP240
00259      MOVE PV-CURRENT-HOUR             TO DP132O-RUN-HOUR.         APKRP240
00260      MOVE PV-CURRENT-MINUTE           TO DP132O-RUN-MINUTE.       APKRP240
00261      MOVE PV-PROGRAM-NAME             TO DP132O-PROGRAM-NAME.     APKRP240
00262      MOVE 1                           TO DP132O-REPORT-NUMBER.    APKRP240
00263                                                                   APKRP240
00264      PERFORM Y100-OPEN-VENDOR-SPEC-CSR.                           APKRP240
00265      PERFORM R100-FETCH-VENDOR-SPEC.                              APKRP240
00266  EJECT                                                            APKRP240
00267 *----------------------------------------------------------------*APKRP240
00268 *    MOVES DATA FROM THE DB2 TABLE AREAS TO THE REPORT.          *APKRP240
00269 *    CALLED FROM A100-MAIN                                       *APKRP240
00270 *----------------------------------------------------------------*APKRP240
00271  B200-PREPARE-REPORT.                                             APKRP240
00272                                                                   APKRP240
00273      MOVE 'B200-PREPARE-REPORT' TO PV-CURRENT-PARAGRAPH.          APKRP240
00274                                                                   APKRP240
00275      MOVE ENTNME-ENT-NAME-DESC    TO DL-VENDOR-NAME.              APKRP240
00276      MOVE EXTENT-DUN-NBR          TO DL-DUN-NBR.                  APKRP240
00277      MOVE APVNDS-INVC-RCV-VAR-PCT TO DL-INVC-RCV-VAR-PCT.         APKRP240
00278      MOVE '%'                     TO DL-PCT-IND.                  APKRP240
00279      MOVE APVNDS-INVC-RCV-VAR-AMT TO DL-INVC-RCV-VAR-AMT.         APKRP240
00280      MOVE APVNDS-INVC-RCV-VAR-IND TO DL-INVC-RCV-VAR-IND.         APKRP240
00281                                                                   APKRP240
00282      PERFORM W100-WRITE-VENDOR-SPEC-RPT.                          APKRP240
00283      INITIALIZE DL-DETAIL-LINE.                                   APKRP240
00284                                                                   APKRP240
00285      PERFORM R100-FETCH-VENDOR-SPEC.                              APKRP240
00286  EJECT                                                            APKRP240
00287 *----------------------------------------------------------------*APKRP240
00288 *    ENDING PROCESSING: DISPLAYS COUNT OF VENDORS AND CLOSES     *APKRP240
00289 *    THE CURSOR AND FILES.                                       *APKRP240
00290 *    CALLED FROM A100-MAIN                                       *APKRP240
00291 *----------------------------------------------------------------*APKRP240
00292  B300-END-PROGRAM.                                                APKRP240
00293                                                                   APKRP240
00294      MOVE 'B300-END-PROGRAM' TO PV-CURRENT-PARAGRAPH.             APKRP240
00295                                                                   APKRP240
00296      DISPLAY 'VENDORS PROCESSED = ' PC-VENDOR-SPEC-COUNT.         APKRP240
00297                                                                   APKRP240
00298      PERFORM Y110-CLOSE-VENDOR-SPEC-CSR.                          APKRP240
00299                                                                   APKRP240
00300      CLOSE VENDOR-SPEC-RPT-FILE.                                  APKRP240
00301  EJECT                                                            APKRP240
00302 *----------------------------------------------------------------*APKRP240
00303 *    RETRIEVES THE VENDORS ON THE VENDOR SPECIFICATION TABLE.    *APKRP240
00304 *    NOTE:  A NOT FOUND CONDITION IN THIS INSTANCE SIMPLY        *APKRP240
00305 *           RESULTS IN THE OUT OF VENDORS SWITCH BEING SET.      *APKRP240
00306 *----------------------------------------------------------------*APKRP240
00307  R100-FETCH-VENDOR-SPEC.                                          APKRP240
00308                                                                   APKRP240
00309      MOVE 'R100-FETCH-VENDOR-SPEC' TO PV-CURRENT-PARAGRAPH.       APKRP240
00310                                                                   APKRP240
00311      PERFORM WITH TEST AFTER                                      APKRP240
00312              VARYING PC-RETRY-COUNT FROM 1 BY 1                   APKRP240
00313              UNTIL   PC-RETRY-COUNT > PC-RETRY-MAX OR             APKRP240
00314                      SQLCODE = ZERO OR                            APKRP240
00315                      SQLCODE = +100                               APKRP240
00316                                                                   APKRP240
00317              EXEC SQL                                             APKRP240
00318                  FETCH VENDOR_SPEC_CSR                            APKRP240
00319                  INTO                                             APKRP240
00320                      :ENTNME-ENT-NAME-DESC                        APKRP240
00321                     ,:EXTENT-DUN-NBR                              APKRP240
00322                     ,:APVNDS-INVC-RCV-VAR-PCT                     APKRP240
00323                     ,:APVNDS-INVC-RCV-VAR-AMT                     APKRP240
00324                     ,:APVNDS-INVC-RCV-VAR-IND                     APKRP240
00325              END-EXEC                                             APKRP240
00326                                                                   APKRP240
00327              EVALUATE TRUE                                        APKRP240
00328                  WHEN SQLCODE = ZERO                              APKRP240
00329                  WHEN SQLCODE = +100                              APKRP240
00330                  WHEN SQLCODE = -904                              APKRP240
00331                  WHEN SQLCODE = -913                              APKRP240
00332                       CONTINUE                                    APKRP240
00333                                                                   APKRP240
00334                  WHEN SQLWARN0 NOT = SPACES                       APKRP240
00335                  WHEN SQLCODE  NOT = ZERO                         APKRP240
00336                      MOVE PV-CURRENT-PARAGRAPH                    APKRP240
00337                                          TO AA-PARAGRAPH-NAME     APKRP240
00338                      MOVE                                         APKRP240
00339                       'UNSUCCESSFUL FETCH WITH VENDOR_SPEC_CSR'   APKRP240
00340                                          TO AA-DB2-OPERATION      APKRP240
00341                      MOVE 'TAPVNDS'      TO AA-DB2-TABLE-1        APKRP240
00342                      MOVE SPACES         TO AA-DB2-TABLE-2        APKRP240
00343                                             AA-DB2-TABLE-3        APKRP240
00344                                             AA-DB2-TABLE-4        APKRP240
00345                                             AA-DB2-TABLE-5        APKRP240
00346                      PERFORM Z998-DB2-ABEND                       APKRP240
00347              END-EVALUATE                                         APKRP240
00348      END-PERFORM.                                                 APKRP240
00349                                                                   APKRP240
00350      IF PC-RETRY-COUNT > PC-RETRY-MAX                             APKRP240
00351          MOVE PV-CURRENT-PARAGRAPH                                APKRP240
00352                              TO AA-PARAGRAPH-NAME                 APKRP240
00353          MOVE 'MAX RETRIES EXCEEDED ON FETCH FOR VENDOR_SPEC_CSR' APKRP240
00354                              TO AA-DB2-OPERATION                  APKRP240
00355          MOVE 'TAPVNDS'      TO AA-DB2-TABLE-1                    APKRP240
00356          MOVE SPACES         TO AA-DB2-TABLE-2                    APKRP240
00357                                 AA-DB2-TABLE-3                    APKRP240
00358                                 AA-DB2-TABLE-4                    APKRP240
00359                                 AA-DB2-TABLE-5                    APKRP240
00360          PERFORM Z998-DB2-ABEND                                   APKRP240
00361      END-IF.                                                      APKRP240
00362                                                                   APKRP240
00363      EVALUATE TRUE                                                APKRP240
00364          WHEN SQLCODE = ZEROS                                     APKRP240
00365               ADD 1 TO PC-VENDOR-SPEC-COUNT                       APKRP240
00366          WHEN SQLCODE = +100                                      APKRP240
00367               SET OUT-OF-VENDORS TO TRUE                          APKRP240
00368      END-EVALUATE.                                                APKRP240
00369  EJECT                                                            APKRP240
00370 *----------------------------------------------------------------*APKRP240
00371 *    WRITES THE VENDOR SPECIFICATION REPORT                      *APKRP240
00372 *----------------------------------------------------------------*APKRP240
00373  W100-WRITE-VENDOR-SPEC-RPT.                                      APKRP240
00374                                                                   APKRP240
00375      MOVE 'W100-WRITE-VENDOR-SPEC-RPT' TO PV-CURRENT-PARAGRAPH.   APKRP240
00376                                                                   APKRP240
00377      IF PC-LINE-COUNT > PC-MAX-LINES                              APKRP240
00378          PERFORM W200-PRINT-HEADERS                               APKRP240
00379      END-IF.                                                      APKRP240
00380                                                                   APKRP240
00381      WRITE VENDOR-SPEC-RPT-REC FROM DL-DETAIL-LINE AFTER 1.       APKRP240
00382      ADD 1  TO PC-LINE-COUNT.                                     APKRP240
00383                                                                   APKRP240
00384 *----------------------------------------------------------------*APKRP240
00385 *    WRITES THE VENDOR SPECIFICATION HEADERS.                    *APKRP240
00386 *----------------------------------------------------------------*APKRP240
00387  W200-PRINT-HEADERS.                                              APKRP240
00388                                                                   APKRP240
00389      MOVE 'W200-PRINT-HEADERS' TO PV-CURRENT-PARAGRAPH.           APKRP240
00390                                                                   APKRP240
00391      ADD 1                            TO PC-PAGE-COUNT.           APKRP240
00392      MOVE PC-PAGE-COUNT               TO DP132O-PAGE-NUMBER.      APKRP240
00393                                                                   APKRP240
00394      WRITE VENDOR-SPEC-RPT-REC                                    APKRP240
00395            FROM DP132O-STANDRD-HEADER-132-COLS AFTER PAGE.        APKRP240
00396      WRITE VENDOR-SPEC-RPT-REC FROM H2-HEADER2 AFTER 1.           APKRP240
00397                                                                   APKRP240
00398      WRITE VENDOR-SPEC-RPT-REC FROM DH-DETAIL-HEADER1 AFTER 2.    APKRP240
00399      WRITE VENDOR-SPEC-RPT-REC FROM DH-DETAIL-HEADER2 AFTER 1.    APKRP240
00400                                                                   APKRP240
00401      MOVE SPACES TO VENDOR-SPEC-RPT-REC.                          APKRP240
00402      WRITE VENDOR-SPEC-RPT-REC AFTER 1.                           APKRP240
00403      MOVE 6  TO PC-LINE-COUNT.                                    APKRP240
00404 *----------------------------------------------------------------*APKRP240
00405 *    OPENS THE VENDOR SPECIFICATION CURSOR.                      *APKRP240
00406 *    CALLED FROM B100-INITIALIZE                                 *APKRP240
00407 *----------------------------------------------------------------*APKRP240
00408  Y100-OPEN-VENDOR-SPEC-CSR.                                       APKRP240
00409                                                                   APKRP240
00410      MOVE 'Y100-OPEN-VENDOR-SPEC-CSR' TO PV-CURRENT-PARAGRAPH.    APKRP240
00411                                                                   APKRP240
00412      PERFORM WITH TEST AFTER                                      APKRP240
00413              VARYING PC-RETRY-COUNT FROM 1 BY 1                   APKRP240
00414              UNTIL   PC-RETRY-COUNT > PC-RETRY-MAX OR             APKRP240
00415                      SQLCODE = ZERO OR                            APKRP240
00416                      SQLCODE = +100                               APKRP240
00417                                                                   APKRP240
00418              EXEC SQL                                             APKRP240
00419                  OPEN VENDOR_SPEC_CSR                             APKRP240
00420              END-EXEC                                             APKRP240
00421                                                                   APKRP240
00422              EVALUATE TRUE                                        APKRP240
00423                  WHEN SQLCODE = ZERO                              APKRP240
00424                  WHEN SQLCODE = -904                              APKRP240
00425                  WHEN SQLCODE = -913                              APKRP240
00426                       CONTINUE                                    APKRP240
00427                                                                   APKRP240
00428                  WHEN SQLWARN0 NOT = SPACES                       APKRP240
00429                  WHEN SQLCODE  NOT = ZERO                         APKRP240
00430                      MOVE PV-CURRENT-PARAGRAPH                    APKRP240
00431                                          TO AA-PARAGRAPH-NAME     APKRP240
00432                      MOVE 'UNSUCCESSFUL OPEN ON VENDOR_SPEC_CSR'  APKRP240
00433                                          TO AA-DB2-OPERATION      APKRP240
00434                      MOVE 'TPAYREQ'      TO AA-DB2-TABLE-1        APKRP240
00435                      MOVE 'TINVOIC'      TO AA-DB2-TABLE-2        APKRP240
00436                      MOVE SPACES         TO AA-DB2-TABLE-3        APKRP240
00437                                             AA-DB2-TABLE-4        APKRP240
00438                                             AA-DB2-TABLE-5        APKRP240
00439                      PERFORM Z998-DB2-ABEND                       APKRP240
00440              END-EVALUATE                                         APKRP240
00441      END-PERFORM.                                                 APKRP240
00442                                                                   APKRP240
00443      IF PC-RETRY-COUNT > PC-RETRY-MAX                             APKRP240
00444          MOVE PV-CURRENT-PARAGRAPH                                APKRP240
00445                              TO AA-PARAGRAPH-NAME                 APKRP240
00446          MOVE 'MAX RETRIES EXCEEDED ON OPEN FOR VENDOR_SPEC_CSR'  APKRP240
00447                              TO AA-DB2-OPERATION                  APKRP240
00448          MOVE 'TPAYREQ'      TO AA-DB2-TABLE-1                    APKRP240
00449          MOVE 'TINVOIC'      TO AA-DB2-TABLE-2                    APKRP240
00450          MOVE SPACES         TO AA-DB2-TABLE-3                    APKRP240
00451                                 AA-DB2-TABLE-4                    APKRP240
00452                                 AA-DB2-TABLE-5                    APKRP240
00453          PERFORM Z998-DB2-ABEND                                   APKRP240
00454      END-IF.                                                      APKRP240
00455                                                                   APKRP240
00456                                                                   APKRP240
00457 *----------------------------------------------------------------*APKRP240
00458 *    CLOSES THE VENDOR SPECIFICATION CURSOR.                     *APKRP240
00459 *    CALLED FROM B300-END-PROGRAM                                *APKRP240
00460 *----------------------------------------------------------------*APKRP240
00461  Y110-CLOSE-VENDOR-SPEC-CSR.                                      APKRP240
00462                                                                   APKRP240
00463      MOVE 'Y110-CLOSE-VENDOR-SPEC-CSR' TO PV-CURRENT-PARAGRAPH.   APKRP240
00464                                                                   APKRP240
00465      PERFORM WITH TEST AFTER                                      APKRP240
00466              VARYING PC-RETRY-COUNT FROM 1 BY 1                   APKRP240
00467              UNTIL   PC-RETRY-COUNT > PC-RETRY-MAX OR             APKRP240
00468                      SQLCODE = ZERO OR                            APKRP240
00469                      SQLCODE = +100                               APKRP240
00470                                                                   APKRP240
00471              EXEC SQL                                             APKRP240
00472                  CLOSE VENDOR_SPEC_CSR                            APKRP240
00473              END-EXEC                                             APKRP240
00474                                                                   APKRP240
00475              EVALUATE TRUE                                        APKRP240
00476                  WHEN SQLCODE = ZERO                              APKRP240
00477                  WHEN SQLCODE = -904                              APKRP240
00478                  WHEN SQLCODE = -913                              APKRP240
00479                       CONTINUE                                    APKRP240
00480                                                                   APKRP240
00481                  WHEN SQLWARN0 NOT = SPACES                       APKRP240
00482                  WHEN SQLCODE  NOT = ZERO                         APKRP240
00483                      MOVE PV-CURRENT-PARAGRAPH                    APKRP240
00484                                          TO AA-PARAGRAPH-NAME     APKRP240
00485                      MOVE 'UNSUCCESSFUL CLOSE ON VENDOR_SPEC_CSR' APKRP240
00486                                          TO AA-DB2-OPERATION      APKRP240
00487                      MOVE 'TPAYREQ'      TO AA-DB2-TABLE-1        APKRP240
00488                      MOVE 'TINVOIC'      TO AA-DB2-TABLE-2        APKRP240
00489                      MOVE SPACES         TO AA-DB2-TABLE-3        APKRP240
00490                                             AA-DB2-TABLE-4        APKRP240
00491                                             AA-DB2-TABLE-5        APKRP240
00492                      PERFORM Z998-DB2-ABEND                       APKRP240
00493              END-EVALUATE                                         APKRP240
00494      END-PERFORM.                                                 APKRP240
00495                                                                   APKRP240
00496      IF PC-RETRY-COUNT > PC-RETRY-MAX                             APKRP240
00497          MOVE PV-CURRENT-PARAGRAPH                                APKRP240
00498                              TO AA-PARAGRAPH-NAME                 APKRP240
00499          MOVE 'MAX RETRIES EXCEEDED ON CLOSE FOR VENDOR_SPEC_CSR' APKRP240
00500                              TO AA-DB2-OPERATION                  APKRP240
00501          MOVE 'TPAYREQ'      TO AA-DB2-TABLE-1                    APKRP240
00502          MOVE 'TINVOIC'      TO AA-DB2-TABLE-2                    APKRP240
00503          MOVE SPACES         TO AA-DB2-TABLE-3                    APKRP240
00504                                 AA-DB2-TABLE-4                    APKRP240
00505                                 AA-DB2-TABLE-5                    APKRP240
00506          PERFORM Z998-DB2-ABEND                                   APKRP240
00507      END-IF.                                                      APKRP240
00508  EJECT                                                            APKRP240
00509 *----------------------------------------------------------------*APKRP240
00510 *    ABEND ROUTINE FOR DB2 ERRORS                                *APKRP240
00511 *----------------------------------------------------------------*APKRP240
00512  Z998-DB2-ABEND.                                                  APKRP240
00513                                                                   APKRP240
00514      CLOSE VENDOR-SPEC-RPT-FILE.                                  APKRP240
00515      DISPLAY AA-ABEND-LIT.                                        APKRP240
00516      DISPLAY AA-DB2-ERROR-LIT.                                    APKRP240
00517      DISPLAY AA-PROGRAM-LIT.                                      APKRP240
00518      DISPLAY AA-PARAGRAPH-LIT.                                    APKRP240
00519      DISPLAY AA-DB2-OPERATION-LIT.                                APKRP240
00520      DISPLAY AA-DB2-TABLE-1.                                      APKRP240
00521      DISPLAY AA-DB2-TABLE-2.                                      APKRP240
00522      DISPLAY AA-DB2-TABLE-3.                                      APKRP240
00523      DISPLAY AA-DB2-TABLE-4.                                      APKRP240
00524      SET AC-DB2-ERROR TO TRUE.                                    APKRP240
00525                                                                   APKRP240
00526      COPY DPPD004.                                                APKRP240
00527                                                                   APKRP240
00528      CALL 'ILBOABN0' USING ABEND-CODE.                            APKRP240
00529  EJECT                                                            APKRP240
00530 *----------------------------------------------------------------*APKRP240
00531 *    ABEND ROUTINE                                               *APKRP240
00532 *----------------------------------------------------------------*APKRP240
00533  Z999-ABEND.                                                      APKRP240
00534                                                                   APKRP240
00535      CLOSE VENDOR-SPEC-RPT-FILE.                                  APKRP240
00536      DISPLAY AA-ABEND-LIT.                                        APKRP240
00537      DISPLAY AA-PROGRAM-LIT.                                      APKRP240
00538      DISPLAY AA-PARAGRAPH-LIT.                                    APKRP240
00539      DISPLAY AA-MESSAGE-LINE.                                     APKRP240
00540      CALL 'ILBOABN0' USING ABEND-CODE.                            APKRP240