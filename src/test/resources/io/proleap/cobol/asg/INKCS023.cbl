00010 ******************************************************************04/27/90
00020 IDENTIFICATION DIVISION.                                          INKCS023
00030 ******************************************************************   LV001
00004  PROGRAM-ID.    INKCS023.                                         INKCS023
00005  AUTHOR.        TESS BESTE.                                       INKCS023
00006  INSTALLATION.  KOHLS DEPARTMENT STORES.                          INKCS023
00007  DATE-WRITTEN.  08-13-93.                                         INKCS023
00008  DATE-COMPILED.                                                   INKCS023
00009 *----------------------------------------------------------------*INKCS023
00010 *    I023 - ADD STOCKROOM WITHDRAWAL SHEET                       *INKCS023
00011 *                                                                *INKCS023
00012 *    THIS PROGRAM ALLOWS THE USER TO ADD A STOCKROOM WITHDRAWAL  *INKCS023
00013 *    SHEET.  FOR EACH LINE ON THE SHEET, A LINE IS ENTERED ON    *INKCS023
00014 *    THE SCREEN.  EACH LINE CORRESPONDS TO A ROW ON THE          *INKCS023
00015 *    STOCKROOM WITHDRAWAL TABLE, TSTKRMW.                        *INKCS023
00016 *                                                                *INKCS023
00017 *    SHEETS MAY ONLY BE ADDED DURING THE PERIOD BETWEEN THE      *INKCS023
00018 *    ACTUAL INVENTORY DATE AND A SPECIFIED NUMBER OF DAYS AFTER  *INKCS023
00019 *    THE ACTUAL INVENTORY DATE.  THE NUMBER OF DAYS IS SPECIFIED *INKCS023
00020 *    ON THE INVENTORY PARAMETERS TABLE, TINVPAR. THE CRITERIA    *INKCS023
00021 *    SELECTION PROGRAM, INKCS020, DETERMINES WHETHER THE         *INKCS023
00022 *    CURRENT DATE IS WITHIN THE STOCKROOM WITHDRAWAL PERIOD      *INKCS023
00023 *    AND INDICATES THE RESULTS VIA A SWITCH IN THE INTER-APPL    *INKCS023
00024 *    COMMAREA.                                                   *INKCS023
00025 *                                                                *INKCS023
00026 *    FOR EACH LINE, A SKU OR UPC, AND QUANTITY ARE ENTERED.      *INKCS023
00027 *    IF A UPC IS ENTERED, IT IS TRANSLATED TO A SKU, AND BOTH    *INKCS023
00028 *    THE UPC AND SKU ARE STORED ON THE TABLE. CLEARANCE SKUS     *INKCS023
00029 *    ARE INVALID.                                                *INKCS023
00030 *                                                                *INKCS023
00031 *    EACH LINE IS ASSIGNED A SEQUENTIAL LINE NUMBER, BEGINNING   *INKCS023
00032 *    WITH 1, AS IT IS STORED IN THE TABLE.                       *INKCS023
00033 *                                                                *INKCS023
00034 *    IF A SKU WAS ENTERED AND IT IS A CLEARANCE SKU OR THE       *INKCS023
00035 *    SEQUENCE NUMBER IS '000' OR '999', OR IF A UPC WAS          *INKCS023
00036 *    ENTERED AND IT IS AN INTERNAL UPC WITH AN EMBEDDED SEQUENCE *INKCS023
00037 *    NUMBER OF '000' OR '999', THE CUTOFF PRICE MUST BE ENTERED  *INKCS023
00038 *    ON THE SCREEN. OTHERWISE, IF A UPC WAS ENTERED, THE         *INKCS023
00039 *    ASSOCIATED SKU IS USED TO DETERMINE THE CUTOFF PRICE. IF A  *INKCS023
00040 *    SKU WAS ENTERED, THE ENTERED SKU IS USED TO DETERMINE THE   *INKCS023
00041 *    CUTOFF PRICE. IF THE SKU ENTERED WAS RENUMBERED BEFORE      *INKCS023
00042 *    INVENTORY TOOK PLACE, THE RENUMBERED SKU IS USED TO         *INKCS023
00043 *    DETERMINE THE CUTOFF PRICE. PRICES ARE DISPLYAED  ONLY      *INKCS023
00044 *    AFTER ALL EDITS HAVE BEEN PASSED.                           *INKCS023
00045 *----------------------------------------------------------------*INKCS023
00046 *  KIM WRIGHT                                   9/16/94         * INKCS023
00047 *  CHANGED THE SELECTION OF CUTOFF PRICE ON THE TSKST TABLE TO  * INKCS023
00048 *  SELECT FROM STORE '000' INSTEAD OF THE SPECIFIC STORE        * INKCS023
00049 *---------------------------------------------------------------* INKCS023
00046 *  JULIE SEIDLER                                8/09/99         * INKCS023
00047 *  REMOVED REFERENCE TO DPG51-SYSTEM-DATE                       * INKCS023
00049 *---------------------------------------------------------------* INKCS023
00046 *  SCOTT JACKSON       CHANGE ID : IN20000      9/29/99         * INKCS023
00047 *  RCP CHANGES - RETAIL/COST PRICING WILL BE SOURCED FROM       * INKCS023
00047 *  TUPCPLS INSTEAD OF TSKST. TSK.REGN_PRIC_IND WILL DETERMINE   * INKCS023
00047 *  WHETHER SKU/UPC IS AN RCP ITEM (Y/N). A 'Y' WILL FORCE THE   * INKCS023
00047 *  SYSTEM TO LOOK FOR STORE-SPECIFIC ROW ON TUPCPLS FOR PRICING.* INKCS023
00047 *  IF NO STORE-SPECIFIC ROW IS FOUND, THEN USE STORE 000 ROW.   * INKCS023
00049 *---------------------------------------------------------------* INKCS023
SMJ   * 03/04/2000  SCOTT JACKSON      CHG ID:SMJ                     *
SMJ   *    ONLINE ABENDS (-913) OCCUR UPON ENTRY TO SHEET MAINTENANCE *
SMJ   *    IF TINVPAR IS WAS BEING UPDATED BY BATCH PROCESSES. 'SOFT' *
SMJ   *    ABEND TO USER WITH 'RESOURCE UNAVAILABLE' ON ADD ATTEMPT . *
SMJ   *---------------------------------------------------------------* INKCS023
IN2001* 09/06/2000  SCOTT JACKSON      CHG ID:IN2001                  *
IN2001*  AN ITEM IN RCP STATUS MAY BE RETURNED TO NON-RCP STATUS      *
IN2001*  PRIOR TO PROCESSING ALL OF THE STORE'S DATA. THEREFORE USING *
IN2001*  TSK.REGN_PRICE_IND AS THE IND TO OBTAINING CURRENT PRICE MAY *
IN2001*  BE INCORRECT AND UNRELIABLE BASED ON WHEN DATA IS PROCESSED. *
IN2001*  TUPCPLS WILL BE USED SOLELY IN DETERMINING AN ITEM'S STATUS. *
IN2001*  CHECK STORE 000 ROW 1ST (IF STATUS = '25' OR '30' CHECK FOR  *
IN2001*  STORE-SPECIFIC ROW). USE STORE 000 PRICING IF NO STORE ROW.  *
IN2001*  IF STATUS <> '25' THEN STORE-SPECIFIC ROW WILL NOT BE CHECKED*
      *****************************************************************
      *                                                               *
      * WR/PROJ  DATE        DESCRIPTION OF CHANGES                   *
      * -------  ----------  ---------------------------------------- *
      * 22000    02-28-2001  CHANGE TO SET STOCKROOM WITHDRAWAL IND   *
      *                      TO 'Y' WHEN THERE IS A SUCCESSFUL ADD    *
      *                      TO THE STOCKROOM WITHDRAWAL.  -LEE YANG  *
      *****************************************************************
RENUM * XXXXX    09-26-2001  SCOTT JACKSON      CHG ID:RENUM
      * ENTERED SKU'S MAY BE IN A RENUMBERED STATUS. THIS PROGRAM GETS
      * THE MOST RECENT SKU FOR PRICING PURPOSES, BUT STILL PASSES THE
      * OUT-DATED SKU TO TSTKRMW. CONVERT 'RENUMBERED' SKU'S TO CURRENT
      * SKU NUMBER AND LOAD THE NEW SKU TO DB2.
      *---------------------------------------------------------------*
23511C* 05/21/2002 CAROL WOJCIECHOWSKI CHG ID:23511C                  *
23511C*  DISPLAY THE PRICE FOR A YELLOW TICKET SKU WHEN THE           *
23511C*  UPC_STAT_CHG_DTE IS EQUAL TO THE UNT_RTL_CHG_DTE             *
W21732*---------------------------------------------------------------*
W21732* 10/25/2002 IGSI                CHG ID:W21732                  *
W21732* ACCEPT NON-INTELLIGENT SKU IN PLACE OF THE CURRENT DEPT/CLASS/*
W21732* SEQ NUMBER.                                                   *
W21732*---------------------------------------------------------------*
24029 * 24029    01-28-2003  DEPT/CLASS 254/48 IS INVALID; HOWEVER, THE
24029 *                      MODULE IS ALLOWING ENTRY WITHOUT AN ERROR.
24029 *                      WHY?     - ROD JANSSEN
C21732*---------------------------------------------------------------*
C21732*    CHANGE BY S JACKSON  3/17/03                               *
C21732*    CHANGE DPKUT040 TO USE DPKUT041 FOR CHECK DIGIT COMPUTATION*
C21732*    REMOVE DPKUT040 MODULE (FOR INTERNAL UPC GENERATOR) WITH   *
C21732*    A DB2 CALL USING TUPC/TSKXREF FOR RENUMBERS (DCLGENS ADDED)*
C21732*----------------------------------------------------------------*
W25428*    CHANGE BY S JACKSON 10/17/03                               *
W25428*    PREVENT STORES FORM ADDING NEW SHEET ENTRIES AFTER THE     *
W25428*    HAS ALREADY BEEN SELECTED FOR UNIT BOOKING                 *
      *                                                               *
      * 26600    04-20-2004  STORE NUMBER EXPANSION FOR INV. LEDGER   *
      *                      TEMPORARILY ADD LOC-NBR TO INSERT STMT   *
      *                      -V. LEE YANG                             *
      * 26682    06-27-2004  REMOVE CHECK DIGIT LOGIC AND RELATED     *
      *                      CALLED ROUTINES FROM PROGRAM             *
      *                      -SCOTT JACKSON                           *
W26600* 26600    06-28-2004  INV STORE NUMBER EXPANSION
W26600*                      -ROD JANSSEN
W29908* 29908    01-11-2006  SQL -803 ABEND ON INSERT TO STOCK WITHDRWL
W29908*                      -ROD JANSSEN
W28545* 28545    04-18-2006  ADD COMMON PRICING INWS400 AND INPD400 TO
W28545*                      REPLACE SKU AND UPC LOOK UPS.
W28545*                      -JOHN SELWITSCHKA
J28545* 28545    06-16-2006  TINVPAR/TINCNTL ARE USED TO GET
J28545*                      THE PROPER INV-ID AND STORE
J28545*                      -JAN BLAZICH
974311*P974311 01-23-2007 S JACKSON   INVALID HANDLING OF MIXED STATUS
974311*                               SKUS-RETURNING 0.00 RETAIL
W28582* W28582 07-31-2007 J SELWITSCHKA
W28582*                     AFTER LOOKING UP RETAIL WITH UPC, IF DUMMY
W28582*                     UPC GET DATA FROM ATTRIBUTES.
      * W33304 05-01-2008 JILL LIN    HOLDING INVENTORY LEDGER OPEN
      *                               ACROSS PERIODS INVENTORY SHORTAGE
      *                               REPORTING
SR3403* SR3403 03-27-2012 ANUJ GUPTA  LARGE RETAIL EXPANSION
      *----------------------------------------------------------------*
263126* CHANGED 10/25/2021    BY JIM HUNTER      WR: CHG0263126
263126* CHANGE CALL OF DPKUT100 TO CALL DP010I-NUMERIC-EDIT-ROUTINE
263126* MAKING THE CALL DYNAMIC VS STATIC.
263126*----------------------------------------------------------------*
1122RM* CHANGED 11/03/2022    BY BOB MCASEY      WR: CHG0277188
1122RM* ADDED THE DPKCS930 COPYBOOK. CHANGED THE PARAMETERS FOR CALLING
1122RM* THE CICS ARCHITECTURE API.
1122RM*---------------------------------------------------------------*
00050 * CHANGED 02/14/2023    BY JEAN KURK       WR: CHG0281369
00050 * REMOVE COMMENTED CODE THAT WAS CALLING THE UPC CHECK DIGIT RTN
00050 *---------------------------------------------------------------*
00051                                                                   INKCS023
00053  ENVIRONMENT DIVISION.                                            INKCS023
00057  DATA DIVISION.                                                   INKCS023
00059                                                                   INKCS023
00060  WORKING-STORAGE SECTION.                                         INKCS023
00061                                                                   INKCS023
00062  01  PC-PROGRAM-CONSTANTS.                                        INKCS023
00063      05  PC-CURRENT-MAP-NAME     PIC  X(08) VALUE 'IN023A  '.     INKCS023
00064      05  PC-CURRENT-MAPSET-NAME  PIC  X(08) VALUE 'INKM023 '.     INKCS023
00065      05  PC-CURRENT-PROGRAM-NAME PIC  X(08) VALUE 'INKCS023'.     INKCS023
00066      05  PC-CALENDAR-ROUTINE     PIC  X(08) VALUE 'DPKUT500'.     INKCS023
00067      05  PC-LINES-PER-PANEL      PIC S9(04) VALUE +16   COMP SYNC.INKCS023
IN2001     05  PC-MIXED-STATUS         PIC  X(02) VALUE '25'.
IN2000     05  PC-CLEARANCE-SKU-STATUS PIC  X(02) VALUE '30'.
JUNE99     05  PC-RENUMBERED-SKU-STAT  PIC  X(02) VALUE '98'.           INKCS023
00071      05  PC-MDSELV-CDE           PIC  X(03) VALUE 'SCL'.          INKCS023
00072      05  PC-OPERCO-NBR           PIC  X(02) VALUE '03'.           INKCS023
00073      05  PC-APLSYS-CDE           PIC  X(02) VALUE 'IN'.           INKCS023
00074      05  PC-ITM-UNIT-PR-MAX-DIGITS                                INKCS023
00075                                  PIC S9(04) VALUE +5.             INKCS023
00076      05  PC-MAX-QTY              PIC  9(07) VALUE 99.             INKCS023
00077      05  PC-MIN-PRICE            PIC  9(05)V99                    INKCS023
00078                                             VALUE .2.             INKCS023
00079      05  PC-MAX-PRICE            PIC  9(05)V99                    INKCS023
00080                                             VALUE 99999.99.       INKCS023
IN2000     05  PC-STORE-000            PIC S9(04) COMP        VALUE +0.
00082      05  PC-TSYMSG-NUMBERS.                                       INKCS023
00083          10  PC-TSYMSG-00005     PIC  9(05) VALUE 00005.          INKCS023
00084          10  PC-TSYMSG-00007     PIC  9(05) VALUE 00007.          INKCS023
00085          10  PC-TSYMSG-00008     PIC  9(05) VALUE 00008.          INKCS023
00086          10  PC-TSYMSG-00010     PIC  9(05) VALUE 00010.          INKCS023
00087          10  PC-TSYMSG-00137     PIC  9(05) VALUE 00137.          INKCS023
00088          10  PC-TSYMSG-00343     PIC  9(05) VALUE 00343.          INKCS023
00089          10  PC-TSYMSG-00413     PIC  9(05) VALUE 00413.          INKCS023
RENUM          10  PC-TSYMSG-00453     PIC  9(05) VALUE 00453.          00310000
00090          10  PC-TSYMSG-00474     PIC  9(05) VALUE 00474.          INKCS023
00091          10  PC-TSYMSG-00547     PIC  9(05) VALUE 00547.          INKCS023
00092          10  PC-TSYMSG-00549     PIC  9(05) VALUE 00549.          INKCS023
00093          10  PC-TSYMSG-00550     PIC  9(05) VALUE 00550.          INKCS023
00094          10  PC-TSYMSG-00551     PIC  9(05) VALUE 00551.          INKCS023
00095          10  PC-TSYMSG-00552     PIC  9(05) VALUE 00552.          INKCS023
00096          10  PC-TSYMSG-00560     PIC  9(05) VALUE 00560.          INKCS023
00097          10  PC-TSYMSG-00684     PIC  9(05) VALUE 00684.          INKCS023
00098          10  PC-TSYMSG-00685     PIC  9(05) VALUE 00685.          INKCS023
SMJ            10  PC-TSYMSG-01063     PIC  9(05) VALUE 01063.
W25428         10  PC-TSYMSG-02895     PIC  9(05) VALUE 02895.
00099                                                                   INKCS023
00100                                                                   INKCS023
00101  01  PS-PROGRAM-SUBSCRIPTS.                                       INKCS023
00102      05  PS-SUB                  PIC S9(04) VALUE +0    COMP SYNC.INKCS023
00103      05  PS-SUB2                 PIC S9(04) VALUE +0    COMP SYNC.INKCS023
00104 *                                                                 INKCS023
00105  01  PS-PROGRAM-SWITCHES.                                         INKCS023
00106      05  PS-ERROR-SW             PIC  X     VALUE  'N'.           INKCS023
00107          88  PS-ERROR                       VALUE  'Y'.           INKCS023
00108          88  PS-NO-ERROR                    VALUE  'N'.           INKCS023
00109      05  PS-ERRORS-IN-LINE-SW    PIC  X     VALUE  'N'.           INKCS023
00110          88  PS-ERRORS-IN-LINE              VALUE  'Y'.           INKCS023
00111          88  PS-NO-ERRORS-IN-LINE           VALUE  'N'.           INKCS023
00112      05  PS-DATA-ENTERED-SW      PIC  X     VALUE  'N'.           INKCS023
00113          88  PS-DATA-ENTERED                VALUE  'Y'.           INKCS023
00114          88  PS-NO-DATA-ENTERED             VALUE  'N'.           INKCS023
00115      05  PS-INVALID-SKU-SW       PIC X      VALUE  'N'.           INKCS023
00116          88  PS-VALID-SKU                   VALUE  'Y'.           INKCS023
00117          88  PS-INVALID-SKU                 VALUE  'N'.           INKCS023
W26682     05  PS-INVALID-UPC-SW       PIC X      VALUE  'N'.           INKCS023
W26682         88  PS-VALID-UPC                   VALUE  'Y'.           INKCS023
W26682         88  PS-INVALID-UPC                 VALUE  'N'.           INKCS023
00118      05  PS-DONE-RENUMBERING-SW  PIC X      VALUE  'N'.           INKCS023
00119          88  PS-DONE-RENUMBERING            VALUE  'Y'.           INKCS023
00120          88  PS-NOT-DONE-RENUMBERING        VALUE  'N'.           INKCS023
00121      05  PS-ADD-SUCCESSFUL-SW    PIC X      VALUE  'Y'.           INKCS023
00122          88 PS-ADD-SUCCESSFUL               VALUE  'Y'.           INKCS023
00123          88 PS-ADD-NOT-SUCCESSFUL           VALUE  'N'.           INKCS023
00124      05  PS-GOT-LAST-PRICE-SW    PIC X.                           INKCS023
00125          88  PS-GOT-LAST-PRICE-FROM-SCREEN                        INKCS023
00126                                             VALUE 'S'.            INKCS023
00127          88  PS-GOT-LAST-PRICE-FROM-TSKST                         INKCS023
00128                                             VALUE 'T'.            INKCS023
00129          88  PS-DIDNT-GET-LAST-PRICE        VALUE 'N'.            INKCS023
SMJ        05  PS-INVPAR-SW            PIC  X     VALUE  'N'.           INKCS023
SMJ            88  PS-UNAVAILABLE-RESOURCE        VALUE  'Y'.           INKCS023
SMJ            88  PS-INVPAR-PROCESSED            VALUE  'N'.           INKCS023
00130 *                                                                 INKCS023
00131  01  PV-PROGRAM-VARIABLES.                                        INKCS023
W26600     05  PV-STORE-NBR-X          PIC X(04)   VALUE ZEROES.
W26600     05  PV-STORE-NBR-9 REDEFINES PV-STORE-NBR-X
W26600                                 PIC 9(04).
W21732     05  PV-DUMMY-SKU            PIC X(08).
00132      05  PV-CICS-RESPONSE        PIC S9(04) VALUE +0    COMP SYNC.INKCS023
00133      05  PV-UPC-CODE             PIC S9(15) VALUE +0    COMP-3.   INKCS023
IN2001     05  PV-STR000-ROW.
IN2001         10  PV-STR000-UPC-STAT-CDE  PIC X(02)    VALUE SPACES.
IN2001         10  PV-STR000-UNT-RTL-AMT   PIC S9(7)V99 VALUE +0 COMP-3.
IN2001         10  PV-STR000-MEITGP-NBR    PIC S9(9)    VALUE +0 COMP-3.
00134      05  PV-STAT-CHNGD-DATE-YYYYMMDD                              INKCS023
00135                                  PIC  X(08).                      INKCS023
00136      05  PV-MAJ-CL-NBR.                                           INKCS023
00137          10 PV-MAJ-CL-NBR-1      PIC  X     VALUE SPACES.         INKCS023
00138          10 PV-MAJ-CL-NBR-2      PIC  X     VALUE SPACES.         INKCS023
00139      05  PV-SUB-CL-NBR.                                           INKCS023
00140          10 PV-SUB-CL-NBR-1      PIC  X     VALUE SPACES.         INKCS023
00141          10 PV-SUB-CL-NBR-2-3    PIC  X(02) VALUE SPACES.         INKCS023
W21732     05  PV-DATE                 PIC X(10).
W21732     05  PV-YYMMDD-DATE          PIC  X(6).
W21732     05  FILLER                  REDEFINES
W21732         PV-YYMMDD-DATE.
W21732         10  PV-YYMMDD-YY        PIC  X(02).
W21732         10  PV-YYMMDD-MMDD.
W21732             15 PV-YYMMDD-MM     PIC  X(02).
W21732             15 PV-YYMMDD-DD     PIC  X(02).
W21732     05  PV-MMDDYY-DATE.                                          INKCS023
W21732         10  PV-MMDDYY-MMDD      PIC  X(04).                      INKCS023
W21732         10  PV-MMDDYY-YY        PIC  X(02).                      INKCS023
00150                                                                   INKCS023
00151  01  PV-DB2-KEY-AREA.                                             INKCS023
W26600     05  PV-DB2-STORE-NBR        PIC  X(04).                      INKCS023
00153      05  PV-DB2-SHEET-NBR        PIC S9(09)             COMP SYNC.INKCS023
00154      05  PV-DB2-UPC-NBR          PIC S9(15)             COMP-3.   INKCS023
00155      05  PV-DB2-ITEM-NMBR        PIC S9(15)             COMP-3.   INKCS023
W21732     05  PV-DB2-SKU-NBR          PIC  X(08).                      INKCS023
W21732     05  PV-DB2-SKU-NBR-N REDEFINES                               INKCS023
W21732         PV-DB2-SKU-NBR          PIC  9(08).
IN2000     05  PV-STR-NBR              PIC S9(04) COMP.
SR3403     05  PV-GROUP-UNIT-RTL       PIC  9(05)V99 VALUE ZEROES.
IN2000
IN2000 01  PV-PLND-INV-TMST            PIC  X(26) VALUE SPACES.
IN2000
IN2000 01  PV-PLND-INV-TIMESTAMP.
IN2000     10  PV-PLND-INV-DTE         PIC  X(10) VALUE SPACES.
IN2000     10  PV-DASH                 PIC  X(01) VALUE '-'.
IN2000     10  PV-PLND-DTE-HOUR        PIC  X(02) VALUE '23'.
IN2000     10  PV-SEPARATOR            PIC  X(01) VALUE '.'.
IN2000     10  PV-PLND-DTE-MINUTE      PIC  X(02) VALUE '59'.
IN2000     10  PV-SEPARATOR            PIC  X(01) VALUE '.'.
IN2000     10  PV-PLND-DTE-SECOND      PIC  X(02) VALUE '59'.
IN2000     10  PV-SEPARATOR            PIC  X(01) VALUE '.'.
IN2000     10  PV-PLND-DTE-MSEC        PIC  X(06) VALUE '999999'.
IN2000     05  PV-RENUMBERED-SKU.
W21732         10  PV-RSKU-NBR          PIC X(08).
      *
IN2001*01  PV-ACTL-INV-DTE             PIC  X(10) VALUE SPACES.
00161                                                                   INKCS023
00162  EJECT                                                            INKCS023
00163 *----------------------------------------------------------------*INKCS023
00164 *    MAP LAYOUT                                                  *INKCS023
00165 *----------------------------------------------------------------*INKCS023
00166                                                                   INKCS023
W26600     COPY INKM023.                                                INKCS023
00168                                                                   INKCS023
00169  01  MR-OCCURS-LAYOUT          REDEFINES  IN023AO.                INKCS023
W26600     05  FILLER                PIC X(62).                         INKCS023
00171      05  MR-DETAIL-LINE OCCURS 16 TIMES.                          INKCS023
W21732         10  MR-SKUL           PIC S9(04) COMP.                   INKCS023
W21732         10  MR-SKUA           PIC  X(01).                        INKCS023
W21732         10  MR-SKUF           REDEFINES                          INKCS023
W21732             MR-SKUA           PIC  X(01).                        INKCS023
W21732         10  MR-SKUC           PIC  X(01).                        INKCS023
W21732         10  MR-SKUH           PIC  X(01).                        INKCS023
W21732         10  MR-SKU            PIC  X(08).                        INKCS023
00193          10  MR-UPCL           PIC S9(04) COMP.                   INKCS023
00194          10  MR-UPCA           PIC  X(01).                        INKCS023
00195          10  MR-UPCF           REDEFINES                          INKCS023
00196              MR-UPCA           PIC  X(01).                        INKCS023
00197          10  MR-UPCC           PIC  X(01).                        INKCS023
00198          10  MR-UPCH           PIC  X(01).                        INKCS023
00199          10  MR-UPC-X.                                            INKCS023
00200              15  MR-UPC        PIC  Z(14)9.                       INKCS023
00201          10  MR-QTYL           PIC S9(04) COMP.                   INKCS023
00202          10  MR-QTYA           PIC  X(01).                        INKCS023
00203          10  MR-QTYF           REDEFINES                          INKCS023
00204              MR-QTYA           PIC  X(01).                        INKCS023
00205          10  MR-QTYC           PIC  X(01).                        INKCS023
00206          10  MR-QTYH           PIC  X(01).                        INKCS023
00207          10  MR-QTY-X.                                            INKCS023
00208              15  MR-QTY        PIC  Z(6)9.                        INKCS023
00209          10  MR-PRCEL          PIC S9(04) COMP.                   INKCS023
00210          10  MR-PRCEA          PIC  X(01).                        INKCS023
00211          10  MR-PRCEF          REDEFINES                          INKCS023
00212              MR-PRCEA          PIC  X(01).                        INKCS023
00213          10  MR-PRCEC          PIC  X(01).                        INKCS023
00214          10  MR-PRCEH          PIC  X(01).                        INKCS023
00215          10  MR-PRCE-X.                                           INKCS023
00216              15  MR-PRCE       PIC  Z(05).99.                     INKCS023
00217      EJECT                                                        INKCS023
00218 *----------------------------------------------------------------*INKCS023
00219 *    COPY BOOK FOR NUMERIC EDIT SUBROUTINE (DPKUT100) PARAMETER  *INKCS023
00220 *    LIST.                                                       *INKCS023
00221 *----------------------------------------------------------------*INKCS023
00222                                                                   INKCS023
00223      COPY DPWS010I.                                               INKCS023
00224                                                                   INKCS023
00225 *----------------------------------------------------------------*INKCS023
00226 *    ATTRIBUTE SETTINGS COPYBOOK.                                *INKCS023
00227 *----------------------------------------------------------------*INKCS023
00228                                                                   INKCS023
00229      COPY DPWS015.                                                INKCS023
00230                                                                   INKCS023
00231 *----------------------------------------------------------------*INKCS023
00232 *    FUNCTION KEYS COPYBOOK                                      *INKCS023
00233 *----------------------------------------------------------------*INKCS023
00234                                                                   INKCS023
00235      COPY DPWS016.                                                INKCS023
00236                                                                   INKCS023
00237 *----------------------------------------------------------------*INKCS023
00238 *    COPY BOOK FOR CURRENT SYSTEM DATE/TIME PROCEDURE DIVISION   *INKCS023
00239 *    ROUTINE                                                      INKCS023
00240 *----------------------------------------------------------------*INKCS023
00241                                                                   INKCS023
00242      COPY DPWS017.                                                INKCS023
00243                                                                   INKCS023
00251 *----------------------------------------------------------------*INKCS023
00252 *    COPY BOOK FOR CALENDAR SUBROUTINE (DPKUT500)                 INKCS023
00253 *----------------------------------------------------------------*INKCS023
00254                                                                   INKCS023
00255      COPY DPWS500.                                                INKCS023
00256                                                                   INKCS023
00257  EJECT                                                            INKCS023
00258 ***************************************************************** INKCS023
00259 * PARAMETERS FOR CALLING CICS ARCHITECTURE API (DPKCS030).        INKCS023
00260 ***************************************************************** INKCS023
00261      COPY DPWS030.                                                INKCS023
1122RM     COPY DPWS930.                                                INKCS023
00262  EJECT                                                            INKCS023
00263 ***************************************************************** INKCS023
00264 *    STANDARD COMMAREA.                                           INKCS023
00265 ***************************************************************** INKCS023
00266                                                                   INKCS023
00267      COPY DPWS020.                                                INKCS023
00268      05  FILLER REDEFINES DP020-VARIABLE-COMMAREA.                INKCS023
00269                                                                   INKCS023
00270 ***************************************************************** INKCS023
00271 *    INTER-APPLICATION COMMAREA.                                  INKCS023
00272 ***************************************************************** INKCS023
00273                                                                   INKCS023
00274          10  INTER-APPL-COMM-AREA PIC X(200).                     INKCS023
W26600     COPY INWS001.                                                INKCS023
00276 ***************************************************************** INKCS023
00277 *    SPECIFIC COMMMAREA FOR DPKCS037.                             INKCS023
00278 ***************************************************************** INKCS023
00279          10 ASC-SPECIFIC-COMMAREA.                                INKCS023
00280            12  ASC-FIELDS-FROM-INTERAPPL-COMM.                    INKCS023
00281              15  ASC-IN-WITHDRAWAL-PERIOD-SW PIC  X.              INKCS023
00282                  88  ASC-IN-WITHDRAWAL-PERIOD       VALUE 'Y'.    INKCS023
00283                  88  ASC-NOT-IN-WITHDRAWAL-PERIOD   VALUE 'N'.    INKCS023
00284              15  ASC-ITM-CTOFF-IND           PIC  X.              INKCS023
00285                  88  ASC-ITM-CTOFF-HAS-OCCURRED     VALUE 'Y'.    INKCS023
00286                  88  ASC-ITM-CTOFF-HASNT-OCCURRED   VALUE 'N'.    INKCS023
W26600             15  ASC-KEY-STORE-NBR-X         PIC  X(04).          INKCS023
W26600             15  ASC-KEY-STORE-NBR REDEFINES                      INKCS023
W26600                 ASC-KEY-STORE-NBR-X         PIC  9(04).          INKCS023
00288              15  ASC-KEY-SHEET-NBR-X.                             INKCS023
00289                  20  ASC-KEY-SHEET-NBR       PIC  9(06).          INKCS023
00290              15  ASC-STORE-NAME              PIC  X(25).          INKCS023
00291              15  ASC-ACTL-INV-DTE-YYYYMMDD   PIC  X(08).          INKCS023
00292                                                                   INKCS023
00293            12  ASC-ITEM-ARRAY.                                    INKCS023
00294              15  ASC-MAP-DETAIL-LINE         OCCURS 16 TIMES.     INKCS023
W21732                 20  ASC-SKU-NBR             PIC  X(08).          INKCS023
W21732                 20  ASC-SKU-NBR-N           REDEFINES            INKCS023
W21732                     ASC-SKU-NBR             PIC  9(08).          INKCS023
00306                  20  ASC-UPC-NBR-X           PIC  X(15).          INKCS023
00307                  20  ASC-UPC-NBR             REDEFINES            INKCS023
00308                      ASC-UPC-NBR-X           PIC  9(15).          INKCS023
00309                  20  FILLER                  REDEFINES            INKCS023
00310                      ASC-UPC-NBR-X.                               INKCS023
00311                      25  FILLER              PIC  X(02).          INKCS023
00312                      25  ASC-UPC-1-2         PIC  X(02).          INKCS023
00313                          88  ASC-UPC-INTERNAL       VALUE '04'.   INKCS023
00314                      25  ASC-UPC-3-4         PIC  X(02).          INKCS023
W21732                     25  ASC-UPC-SKU         PIC  X(08).          INKCS023
00320                      25  ASC-UPC-CHECK-DIGIT PIC  9.              INKCS023
00321                  20  ASC-INV-QTY-X           PIC  X(07).          INKCS023
00322                  20  ASC-INV-QTY             REDEFINES            INKCS023
00323                      ASC-INV-QTY-X           PIC  9(07).          INKCS023
00324                  20  ASC-ITM-UNIT-PR-AMT-X   PIC  X(08).          INKCS023
00325                  20  ASC-ITM-UNIT-PR-AMT-X-FMT                    INKCS023
00326                                              REDEFINES            INKCS023
00327                      ASC-ITM-UNIT-PR-AMT-X   PIC  Z(5).99.        INKCS023
00328                  20  ASC-ITM-UNIT-PR-AMT     PIC  9(5)V99.        INKCS023
00329                  20  ASC-SKU-ENTERED-SW      PIC  X.              INKCS023
00330                      88 ASC-SKU-ENTERED             VALUE 'Y'.    INKCS023
00331                      88 ASC-SKU-NOT-ENTERED         VALUE 'N'.    INKCS023
00332                  20  ASC-GET-PRICE-SW        PIC X.               INKCS023
00333                      88  ASC-GET-PRICE-FROM-SCREEN  VALUE 'S'.    INKCS023
W28545                     88  ASC-GET-PRICE-FROM-INPD400 VALUE 'T'.    INKCS023
IN2001                     88  ASC-GOT-STR000-PRICE       VALUE 'O'.    INKCS023
00335                      88  ASC-DONT-GET-PRICE         VALUE 'N'.    INKCS023
00336                                                                   INKCS023
00337          10  FILLER                          PIC  X(2068).        INKCS023
00338      EJECT                                                        INKCS023
00339 *----------------------------------------------------------------*INKCS023
00340 *    ABEND PROCESSING COPYBOOK.                                  *INKCS023
00341 *----------------------------------------------------------------*INKCS023
00342                                                                   INKCS023
00343      COPY DPWS013.                                                INKCS023
00344                                                                   INKCS023
W28545*    DB2 AREA AND WS AREA FOR COMMON PRICING LOOK-UP
W28545
W28545     COPY INWS400.
W28545
IN2000*    DB2 AREA FOR TINVPAR (INVENTORY PARAMETERS TABLE)
IN2000
IN2000     EXEC SQL
IN2000          INCLUDE TINVPAR
IN2000     END-EXEC.
IN2000
J28545*    DB2 AREA FOR TINCNTL (INVENTORY CONTROL TABLE)
J28545
J28545     EXEC SQL
J28545          INCLUDE TINCNTL
J28545     END-EXEC.
J28545
00345 *    DB2 AREA FOR TSTKRMW (STOCKROOM WITHDRAWAL)                  INKCS023
00346                                                                   INKCS023
00347      EXEC SQL                                                     INKCS023
W26600          INCLUDE TSTKRMW                                         INKCS023
00349      END-EXEC.                                                    INKCS023
00350                                                                   INKCS023
00363 *    DB2 AREA FOR TSKST (SKU STORE)                               INKCS023
00364                                                                   INKCS023
00365      EXEC SQL                                                     INKCS023
00366           INCLUDE TSKST                                           INKCS023
00367      END-EXEC.                                                    INKCS023
00368                                                                   INKCS023
      *    DB2 AREA FOR TSKXREF/TUPC VIEW TABLE (IMV_SKU)
      ***
      * *  EXEC SQL
C21732* *       INCLUDE IMV00001
      * *  END-EXEC.
      ***
00381 *---------------------------------------------------------------* INKCS023
00382 * CURSOR USED TO CHECK FOR THE EXISTENCE OF A SHEET BEING ADDED.* INKCS023
00383 *---------------------------------------------------------------* INKCS023
00384                                                                   INKCS023
00385      EXEC SQL                                                     INKCS023
00386           DECLARE LINE-NBR-CSR CURSOR FOR                         INKCS023
00387           SELECT LINE_NBR                                         INKCS023
00388           FROM   TSTKRMW                                          INKCS023
W26600          WHERE  LOC_NBR = :STKRMW-LOC-NBR     AND                INKCS023
00390                  SHEET_NBR = :PV-DB2-SHEET-NBR                    INKCS023
00391      END-EXEC.                                                    INKCS023
00392                                                                   INKCS023
00393 *    DB2 AREA FOR COMMUNICATIONS                                  INKCS023
00394                                                                   INKCS023
00395      EXEC SQL                                                     INKCS023
00396           INCLUDE SQLCA                                           INKCS023
00397      END-EXEC.                                                    INKCS023
00398                                                                   INKCS023
00399  EJECT                                                            INKCS023
00400  LINKAGE SECTION.                                                 INKCS023
00401                                                                   INKCS023
00402  01  DFHCOMMAREA.                                                 INKCS023
00403      05  FILLER                         OCCURS  1 TO 4072 TIMES   INKCS023
00404                                         DEPENDING ON EIBCALEN.    INKCS023
00405          10  FILLER                     PIC  X(01).               INKCS023
00406                                                                   INKCS023
00407                                                                   INKCS023
00408  PROCEDURE DIVISION.                                              INKCS023
00409                                                                   INKCS023
00410 *---------------------------------------------------------------* INKCS023
00411 *  THIS MODULE CONTROLS THE OVERALL PROCESSING IN THE PROGRAM.  * INKCS023
00412 *  THE SETUP AND PERFORM OF PARAGRAPH 0001-CALL-CICS-ARCH-API   * INKCS023
00413 *  MUST BE THE FIRST CODE EXECUTED IN THIS PROGRAM.             * INKCS023
00414 *                                                               * INKCS023
00415 *  THE SECOND OR 'EXIT' PERFORM OF THIS PARAGRAPH MUST BE THE   * INKCS023
00416 *  LAST CODE EXECUTED ON EACH ITERATION OF THIS PROGRAM.        * INKCS023
00417 *---------------------------------------------------------------* INKCS023
00418                                                                   INKCS023
00419  0000-MAIN-MODULE.                                                INKCS023
00420      INITIALIZE DP030-CICS-API-FIELDS.                            INKCS023
00421      MOVE +1                       TO DP030-NUMBER-OF-MAPS.       INKCS023
00422      MOVE PC-CURRENT-MAPSET-NAME   TO DP030-MAPSET-NAME.          INKCS023
00423      MOVE PC-CURRENT-MAP-NAME      TO DP030-MAP-NAME (1).         INKCS023
00424      SET DP030-RECEIVE-APPL-MAP    TO TRUE.                       INKCS023
00425      MOVE LENGTH OF IN023AI        TO DP030-MAP-LENGTH (1).       INKCS023
00426      MOVE 'PROGRAM ENTRY CALL'     TO DP013-MESSAGE-TEXT (1).     INKCS023
00427      PERFORM 0001-CALL-CICS-ARCH-API.                             INKCS023
00428 *                                                                 INKCS023
00429      PERFORM 1000-CONTROL-PROCESSING                              INKCS023
00430 *                                                                 INKCS023
00431      MOVE 'PROGRAM EXIT CALL'      TO DP013-MESSAGE-TEXT (1).     INKCS023
00432      PERFORM 0001-CALL-CICS-ARCH-API.                             INKCS023
00433                                                                   INKCS023
00434                                                                   INKCS023
00435  0001-CALL-CICS-ARCH-API.                                         INKCS023
00436                                                                   INKCS023
1122RM*    CALL 'DPKCS030' USING DFHEIBLK                               INKCS023
1122RM     CALL DP930-CICS-ARCH-API
1122RM                     USING DFHEIBLK
00438                            DFHCOMMAREA                            INKCS023
00439                            DP030-CICS-API-FIELDS                  INKCS023
00440                            DP020-STANDARD-COMMAREA                INKCS023
00441                            IN023AI.                               INKCS023
00442 *                                                                 INKCS023
00443      IF  DP030-RC-CALL-SUCCESSFUL                                 INKCS023
00444          CONTINUE                                                 INKCS023
00445      ELSE                                                         INKCS023
00446          SET DP013-NO-ROLLBACK                                    INKCS023
00447              DP013-XCTL-DISPLAY-RESTART                           INKCS023
00448              DP013-CICS-ABEND      TO TRUE                        INKCS023
00449          MOVE 'BEFORE 0000-MAIN-MODULE'                           INKCS023
00450                                    TO DP013-PARAGRAPH             INKCS023
1122RM*        MOVE 'CALL TO DPKCS030 NOT SUCCESSFUL, RETURN-CODE ON NEXINKCS023
1122RM*             'T LINE'             TO DP013-MESSAGE-TEXT (2)      INKCS023
1122RM         MOVE 'CALL TO CICS ARCH API NOT SUCCESSFUL, RETURN-CODE O
1122RM-             'N NEXT LINE'        TO DP013-MESSAGE-TEXT (2)
00453          MOVE DP030-RETURN-CODE                                   INKCS023
00454                                    TO DP013-MESSAGE-TEXT (3)      INKCS023
00455          PERFORM DP013-0000-PROCESS-ABEND                         INKCS023
00456      END-IF.                                                      INKCS023
00457  EJECT                                                            INKCS023
00458 *----------------------------------------------------------------*INKCS023
00459 *    PROCESS THE APPROPRIATE PARAGRAPHS BASED ON WHAT THE NEXT   *INKCS023
00460 *    COURSE OF ACTION IS FOR THIS TRANSACTION.                   *INKCS023
00461 *----------------------------------------------------------------*INKCS023
00462                                                                   INKCS023
00463  1000-CONTROL-PROCESSING.                                         INKCS023
00464                                                                   INKCS023
00465      EVALUATE TRUE                                                INKCS023
00466          WHEN DP020-NEXT-ACT-INITIAL                              INKCS023
00467              INITIALIZE ASC-SPECIFIC-COMMAREA                     INKCS023
00468              PERFORM 1100-PROCESS-INTER-APPL-COMM                 INKCS023
00469              IF DP020-NEXT-ACT-APPL-ERROR                         INKCS023
00470                  CONTINUE                                         INKCS023
00471              ELSE                                                 INKCS023
00472                  PERFORM 4000-BUILD-INITIAL-PANEL                 INKCS023
IN2000                 PERFORM 3480-READ-TINVPAR
00473              END-IF                                               INKCS023
00474                                                                   INKCS023
00475          WHEN DP020-NEXT-ACT-READ-MAP                             INKCS023
IN2000             PERFORM 3480-READ-TINVPAR
SMJ                IF  PS-INVPAR-PROCESSED
00476                  PERFORM 2000-PROCESS-PANEL                       INKCS023
SMJ                END-IF                                               INKCS023
00477                                                                   INKCS023
00478          WHEN DP020-NEXT-ACT-RETURN                               INKCS023
IN2000             PERFORM 3480-READ-TINVPAR
SMJ                IF  PS-INVPAR-PROCESSED
00479                  PERFORM 3000-EDIT-DATA-IN-COMMAREA               INKCS023
00480                  PERFORM 4400-MOVE-COMMAREA-TO-SCREEN             INKCS023
00481                      VARYING PS-SUB FROM 1 BY 1                   INKCS023
00482                        UNTIL PS-SUB > PC-LINES-PER-PANEL          INKCS023
SMJ                END-IF                                               INKCS023
00483                                                                   INKCS023
00484          WHEN OTHER                                               INKCS023
00485              SET DP013-LOGIC-ABEND                                INKCS023
00486                  DP013-NO-ROLLBACK TO TRUE                        INKCS023
00487              MOVE '1000-CONTROL-PROCESSING'                       INKCS023
00488                                    TO DP013-PARAGRAPH             INKCS023
00489              MOVE 'INVALID NEXT ACTIVITY RETURNED TO APPL PGM:'   INKCS023
00490                                    TO DP013-MESSAGE-TEXT(1)       INKCS023
00491              MOVE DP020-NEXT-APPL-ACTIVITY                        INKCS023
00492                                    TO DP013-MESSAGE-TEXT(2)       INKCS023
00493              PERFORM DP013-0000-PROCESS-ABEND                     INKCS023
00494      END-EVALUATE.                                                INKCS023
00495                                                                   INKCS023
SMJ        IF  PS-UNAVAILABLE-RESOURCE
SMJ           SET DP030-OVERRIDE-APPL-ERROR
SMJ               DP020-MSG-WARNING    TO TRUE
SMJ   *      ---- SYSTEM UNAVAILABLE, TRY AGAIN LATER ---
SMJ           MOVE PC-TSYMSG-01063  TO DP020-MSG-NUMBER
SMJ           SET DP030-SET-CURSOR-APPL-1    TO TRUE
SMJ        END-IF.
00496                                                                   INKCS023
00497  1100-PROCESS-INTER-APPL-COMM.                                    INKCS023
00498                                                                   INKCS023
00499      IF IN001-NOT-IN-WITHDRAWAL-PERIOD                            INKCS023
00500      OR IN001-ITM-CTOFF-HASNT-OCCURRED                            INKCS023
00501          SET DP020-NEXT-ACT-APPL-ERROR                            INKCS023
00502              DP020-MSG-FATAL           TO TRUE                    INKCS023
00503 *        ---- STORE NOT YET AVAILABLE FOR STKRM WITHDRAWAL ----   INKCS023
00504          MOVE PC-TSYMSG-00560 TO DP020-MSG-NUMBER                 INKCS023
00505      ELSE                                                         INKCS023
W25428          IF IN001-UNIT-BOOK-HAS-OCCURRED                         INKCS023
W25428              SET DP020-NEXT-ACT-APPL-ERROR                       INKCS023
W25428                  DP020-MSG-FATAL           TO TRUE               INKCS023
W25428*           ---- LOCATION HAS ALREADY BEEN SELECTED FOR BOOKING---INKCS023
W25428              MOVE PC-TSYMSG-02895 TO DP020-MSG-NUMBER            INKCS023
00505          ELSE                                                     INKCS023
00506              IF IN001-SHEET-NBR-X NOT NUMERIC                     INKCS023
00507                  SET DP020-NEXT-ACT-APPL-ERROR                    INKCS023
00508                      DP020-MSG-FATAL           TO TRUE            INKCS023
00509 *            ---- ASTERISK NOT PERMITTED FOR SELECTED FUNCTION ---INKCS023
00510                  MOVE PC-TSYMSG-00137 TO DP020-MSG-NUMBER         INKCS023
00511              ELSE                                                 INKCS023
00512                  MOVE IN001-STORE-NBR-X TO PV-DB2-STORE-NBR       INKCS023
W29908                 MOVE PV-DB2-STORE-NBR  TO PV-STORE-NBR-X         INKCS023
W29908                 MOVE PV-STORE-NBR-9    TO STKRMW-LOC-NBR
00513                  MOVE IN001-SHEET-NBR   TO PV-DB2-SHEET-NBR       INKCS023
00514                  PERFORM 4100-CHECK-IF-SHEET-EXISTS               INKCS023
00515              END-IF                                               INKCS023
W25428         END-IF                                                   INKCS023
00516      END-IF.                                                      INKCS023
00517                                                                   INKCS023
00518                                                                   INKCS023
00519 *----------------------------------------------------------------*INKCS023
00520 * FURTHER DETERMINE PROCESSING PATH BASED ON FUNCTION KEY ACTIONS*INKCS023
00521 * SPECIFIC FUNCTION KEYS LISTED HERE ARE ACTED UPON REGARDLESS OF*INKCS023
00522 * EDITS.                                                         *INKCS023
00523 *----------------------------------------------------------------*INKCS023
00524                                                                   INKCS023
00525  2000-PROCESS-PANEL.                                              INKCS023
00526                                                                   INKCS023
00527      EVALUATE TRUE                                                INKCS023
00528          WHEN DP020-SRC-AID = DP016-CLEAR                         INKCS023
00529              PERFORM 3000-EDIT-DATA-IN-COMMAREA                   INKCS023
00530              PERFORM 4400-MOVE-COMMAREA-TO-SCREEN                 INKCS023
00531                  VARYING PS-SUB FROM 1 BY 1                       INKCS023
00532                    UNTIL PS-SUB > PC-LINES-PER-PANEL              INKCS023
00533                                                                   INKCS023
00534          WHEN DP020-FK-REFRESH(DP020-SRC-AID)                     INKCS023
00535              INITIALIZE ASC-ITEM-ARRAY                            INKCS023
00536              PERFORM 4000-BUILD-INITIAL-PANEL                     INKCS023
00537                                                                   INKCS023
00538          WHEN OTHER                                               INKCS023
00539              PERFORM 2200-MOVE-SCREEN-TO-COMMAREA                 INKCS023
00540                  VARYING PS-SUB FROM 1 BY 1                       INKCS023
00541                    UNTIL PS-SUB > PC-LINES-PER-PANEL              INKCS023
00542              PERFORM 3000-EDIT-DATA-IN-COMMAREA                   INKCS023
00543              IF  PS-NO-ERROR                                      INKCS023
00544                  PERFORM 2100-CHECK-FUNCTION-KEY                  INKCS023
00545              END-IF                                               INKCS023
00546              PERFORM 4400-MOVE-COMMAREA-TO-SCREEN                 INKCS023
00547                  VARYING PS-SUB FROM 1 BY 1                       INKCS023
00548                    UNTIL PS-SUB > PC-LINES-PER-PANEL              INKCS023
00549      END-EVALUATE.                                                INKCS023
00550  EJECT                                                            INKCS023
00551 *----------------------------------------------------------------*INKCS023
00552 *  ACT ON ANY FUNCTION KEYS THAT REQUIRE EDITS TO BE PASSED      *INKCS023
00553 *  FIRST.  NOTE THAT INVALID FUNCTION KEYS WILL NOT BE RETURNED  *INKCS023
00554 *  FROM THE CICS ARCHITECTURE API.                               *INKCS023
00555 *----------------------------------------------------------------*INKCS023
00556                                                                   INKCS023
00557  2100-CHECK-FUNCTION-KEY.                                         INKCS023
00558                                                                   INKCS023
00559      EVALUATE TRUE                                                INKCS023
00560                                                                   INKCS023
00561          WHEN DP020-FK-LOCAL-FUNC-01 (DP020-SRC-AID)              INKCS023
00562              PERFORM 5000-ADD-SHEET                               INKCS023
00563              IF PS-ADD-SUCCESSFUL                                 INKCS023
22000                  PERFORM 6000-SET-STOCKROOM-IND
00564 *                *------------------------------------------------INKCS023
00565 *                * IF A SHEET HAS BEEN SUCCESSFULLY ADDED, THE    INKCS023
00566 *                * SPECIFICATION SCREEN IS RETURNED WITH THE      INKCS023
00567 *                * APPROPRIATE MESSAGE. THIS IS ACCOMPLISHED BY   INKCS023
00568 *                * EMULATING THE SETTING OF PF3.                  INKCS023
00569 *                *------------------------------------------------INKCS023
00570                  SET DP020-FK-RETURN (DP020-SRC-AID) TO TRUE      INKCS023
00571 *                ---- STKRMW SHEET SUCCESSFULLY ADDED ----        INKCS023
00572                  MOVE PC-TSYMSG-00550         TO DP020-MSG-NUMBER INKCS023
00573                  MOVE ASC-KEY-SHEET-NBR-X     TO DP020-MSG-TEXT   INKCS023
00574                  SET  DP020-MSG-INFORMATIONAL TO TRUE             INKCS023
00575              END-IF                                               INKCS023
00576                                                                   INKCS023
00577          WHEN DP020-SRC-AID = DP016-ENTER                         INKCS023
00578              CONTINUE                                             INKCS023
00579                                                                   INKCS023
00580          WHEN OTHER                                               INKCS023
00581              SET DP013-NO-ROLLBACK                                INKCS023
00582                  DP013-XCTL-DISPLAY-RESTART                       INKCS023
00583                  DP013-CICS-ABEND  TO TRUE                        INKCS023
00584              MOVE '2100-CHECK-FUNCTION-KEY'                       INKCS023
00585                                    TO DP013-PARAGRAPH             INKCS023
00586              MOVE 'INVALID FUNCTION KEY NOT CAPTURED BY API'      INKCS023
00587                                    TO DP013-MESSAGE-TEXT (1)      INKCS023
00588              PERFORM DP013-0000-PROCESS-ABEND                     INKCS023
00589      END-EVALUATE.                                                INKCS023
00590  EJECT                                                            INKCS023
00591 *----------------------------------------------------------------*INKCS023
00592 * MOVE DATA ENTERED ON THE SCREEN INTO THEIR RESPECTIVE FIELDS IN*INKCS023
00593 * THE APPLICATION-SPECIFIC COMMAREA.  ALL EDITS ARE DONE IN THE  *INKCS023
00594 * APPLICATION-SPECIFIC COMMAREA, NOT ON THE SCREEN.              *INKCS023
00595 *----------------------------------------------------------------*INKCS023
00596                                                                   INKCS023
00597  2200-MOVE-SCREEN-TO-COMMAREA.                                    INKCS023
W21732     IF  MR-SKUL (PS-SUB) > ZERO                                  INKCS023
W21732         MOVE MR-SKU (PS-SUB)     TO ASC-SKU-NBR (PS-SUB)         INKCS023
W21732         IF ASC-SKU-NBR (PS-SUB) > SPACE                          INKCS023
W21732             SET ASC-SKU-ENTERED     (PS-SUB) TO TRUE             INKCS023
W21732         ELSE                                                     INKCS023
W21732             SET ASC-SKU-NOT-ENTERED (PS-SUB) TO TRUE             INKCS023
W21732         END-IF                                                   INKCS023
W21732     ELSE                                                         INKCS023
W21732         IF  MR-SKUF (PS-SUB) = DP015-ERASE-EOF                   INKCS023
W21732             MOVE SPACE           TO ASC-SKU-NBR (PS-SUB)         INKCS023
W21732             SET ASC-SKU-NOT-ENTERED (PS-SUB)                     INKCS023
W21732                                  TO TRUE                         INKCS023
W21732         END-IF                                                   INKCS023
W21732     END-IF.                                                      INKCS023
00628                                                                   INKCS023
00629      IF  MR-UPCL (PS-SUB) > ZERO                                  INKCS023
00630          MOVE MR-UPC-X (PS-SUB)   TO ASC-UPC-NBR-X (PS-SUB)       INKCS023
00631      ELSE                                                         INKCS023
00632          IF  MR-UPCF (PS-SUB) = DP015-ERASE-EOF                   INKCS023
00633              MOVE SPACE           TO ASC-UPC-NBR-X (PS-SUB)       INKCS023
00634          END-IF                                                   INKCS023
00635      END-IF.                                                      INKCS023
00636                                                                   INKCS023
00637      IF  MR-QTYL (PS-SUB) > ZERO                                  INKCS023
00638          MOVE MR-QTY-X (PS-SUB)   TO ASC-INV-QTY-X (PS-SUB)       INKCS023
00639      ELSE                                                         INKCS023
00640          IF  MR-QTYF (PS-SUB) = DP015-ERASE-EOF                   INKCS023
00641              MOVE SPACE           TO ASC-INV-QTY-X (PS-SUB)       INKCS023
00642          END-IF                                                   INKCS023
00643      END-IF.                                                      INKCS023
00644                                                                   INKCS023
00645      IF  MR-PRCEL (PS-SUB) > ZERO                                 INKCS023
00646          MOVE MR-PRCE-X (PS-SUB)                                  INKCS023
00647                          TO ASC-ITM-UNIT-PR-AMT-X (PS-SUB)        INKCS023
00648      ELSE                                                         INKCS023
00649          IF  MR-PRCEF (PS-SUB) = DP015-ERASE-EOF                  INKCS023
00650              MOVE SPACE  TO ASC-ITM-UNIT-PR-AMT-X (PS-SUB)        INKCS023
00651          END-IF                                                   INKCS023
00652      END-IF.                                                      INKCS023
00653                                                                   INKCS023
00654                                                                   INKCS023
00655 *----------------------------------------------------------------*INKCS023
00656 * PERFORM EDITS ON EACH FIELD ENTERED BY THE USER.  IF AN ERROR  *INKCS023
00657 * IS FOUND THE CURSOR IS POSITIONED AT THAT FIELD AND AN         *INKCS023
00658 * APPROPRIATE ERROR MESSAGE IS DISPLAYED.                        *INKCS023
00659 *----------------------------------------------------------------*INKCS023
00660                                                                   INKCS023
00661  3000-EDIT-DATA-IN-COMMAREA.                                      INKCS023
00662                                                                   INKCS023
00663      PERFORM 3220-ELIMINATE-EMPTY-LINES.                          INKCS023
00664                                                                   INKCS023
00665      MOVE ASC-KEY-STORE-NBR-X TO PV-DB2-STORE-NBR.                INKCS023
00666      MOVE ASC-KEY-SHEET-NBR   TO PV-DB2-SHEET-NBR.                INKCS023
00667                                                                   INKCS023
00668      PERFORM VARYING PS-SUB FROM PC-LINES-PER-PANEL BY -1         INKCS023
00669                UNTIL PS-SUB < 1                                   INKCS023
00670          PERFORM 3225-RESET-ATTRIBUTES                            INKCS023
00671          IF ASC-SKU-ENTERED (PS-SUB) OR                           INKCS023
00672             ASC-UPC-NBR-X   (PS-SUB) > SPACE                      INKCS023
00673              PERFORM 3250-EDIT-MAP-DETAIL-FIELDS                  INKCS023
00674          END-IF                                                   INKCS023
00675      END-PERFORM.                                                 INKCS023
00676                                                                   INKCS023
00677                                                                   INKCS023
00678      IF PS-NO-DATA-ENTERED                                        INKCS023
00679          SET  PS-ERROR                                            INKCS023
00680               DP020-MSG-FATAL        TO TRUE                      INKCS023
00681 *        ---- PLEASE ENTER A VALID SKU OR UPC ----                INKCS023
00682          MOVE PC-TSYMSG-00413        TO DP020-MSG-NUMBER          INKCS023
W21732         MOVE -1                     TO MR-SKUL (1)               INKCS023
00684          SET DP030-SET-CURSOR-APPL-1 TO TRUE                      INKCS023
00685      ELSE                                                         INKCS023
00686          IF PS-NO-ERROR                                           INKCS023
W21732             MOVE -1 TO MR-SKUL (1)                               INKCS023
00688              SET DP030-SET-CURSOR-APPL-1 TO TRUE                  INKCS023
00689          END-IF                                                   INKCS023
00690      END-IF.                                                      INKCS023
00691                                                                   INKCS023
00692                                                                   INKCS023
00693 *----------------------------------------------------------------*INKCS023
00694 * IF THE UPC AND THE DEPARTMENT PORTION OF THE SKU ARE EMPTY,    *INKCS023
00695 * REMOVE THE ENTIRE LINE AND MOVE THE SUBSEQUENT LINES UP TO     *INKCS023
00696 * CLOSE THE GAP.                                                 *INKCS023
00697 *----------------------------------------------------------------*INKCS023
00698                                                                   INKCS023
00699  3220-ELIMINATE-EMPTY-LINES.                                      INKCS023
00700                                                                   INKCS023
00701      MOVE +1 TO PS-SUB2.                                          INKCS023
00702      PERFORM VARYING PS-SUB FROM 1 BY 1                           INKCS023
00703                UNTIL PS-SUB > PC-LINES-PER-PANEL                  INKCS023
00704          IF ASC-SKU-ENTERED (PS-SUB) OR                           INKCS023
00705             ASC-UPC-NBR-X   (PS-SUB) > SPACE                      INKCS023
00706              MOVE ASC-MAP-DETAIL-LINE (PS-SUB)                    INKCS023
00707                TO ASC-MAP-DETAIL-LINE (PS-SUB2)                   INKCS023
00708              IF ASC-SKU-NOT-ENTERED (PS-SUB2)                     INKCS023
W21732                 MOVE SPACE TO ASC-SKU-NBR         (PS-SUB2)      INKCS023
00712              END-IF                                               INKCS023
00713              ADD +1 TO PS-SUB2                                    INKCS023
00714              SET PS-DATA-ENTERED TO TRUE                          INKCS023
00715          END-IF                                                   INKCS023
00716      END-PERFORM.                                                 INKCS023
00717                                                                   INKCS023
00718      PERFORM VARYING PS-SUB2 FROM PS-SUB2 BY 1                    INKCS023
00719                UNTIL PS-SUB2 > PC-LINES-PER-PANEL                 INKCS023
00720          INITIALIZE ASC-MAP-DETAIL-LINE     (PS-SUB2)             INKCS023
00721      END-PERFORM.                                                 INKCS023
00722                                                                   INKCS023
00723                                                                   INKCS023
00724  3225-RESET-ATTRIBUTES.                                           INKCS023
00725                                                                   INKCS023
W21732     MOVE DP015-UNP-NUM-NOR-OFF    TO MR-SKUA  (PS-SUB)           INKCS023
00729                                       MR-UPCA  (PS-SUB)           INKCS023
00730                                       MR-QTYA  (PS-SUB).          INKCS023
W21732     MOVE DP015-GREEN              TO MR-SKUC  (PS-SUB)           INKCS023
00734                                       MR-UPCC  (PS-SUB)           INKCS023
00735                                       MR-QTYC  (PS-SUB).          INKCS023
W21732     MOVE DP015-UNDERLINE          TO MR-SKUH  (PS-SUB)           INKCS023
00739                                       MR-UPCH  (PS-SUB)           INKCS023
00740                                       MR-QTYH  (PS-SUB).          INKCS023
00741      MOVE DP015-PRO-NOR-OFF        TO MR-PRCEA (PS-SUB).          INKCS023
00742      MOVE DP015-BLUE               TO MR-PRCEC (PS-SUB).          INKCS023
00743      MOVE DP015-HL-OFF             TO MR-PRCEH (PS-SUB).          INKCS023
00744                                                                   INKCS023
00745                                                                   INKCS023
00746  3250-EDIT-MAP-DETAIL-FIELDS.                                     INKCS023
00747                                                                   INKCS023
00748 *    *------------------------------------------------------------INKCS023
00749 *    * EDIT THE QUANTITY                                          INKCS023
00750 *    *------------------------------------------------------------INKCS023
00751                                                                   INKCS023
00752      SET PS-NO-ERRORS-IN-LINE                                     INKCS023
00753          PS-VALID-SKU         TO TRUE.                            INKCS023
IN2001
IN2001     INITIALIZE PV-STR000-UPC-STAT-CDE
IN2001                PV-STR000-UNT-RTL-AMT
IN2001                PV-STR000-MEITGP-NBR.
00754                                                                   INKCS023
00755      IF ASC-INV-QTY-X (PS-SUB) = SPACE                            INKCS023
00756          SET PS-ERROR                                             INKCS023
00757              PS-ERRORS-IN-LINE                                    INKCS023
00758              DP020-MSG-FATAL         TO TRUE                      INKCS023
00759 *        ---- REQUIRED FIELD ----                                 INKCS023
00760          MOVE PC-TSYMSG-00007        TO DP020-MSG-NUMBER          INKCS023
00761          MOVE DP015-UNP-NUM-BRT-OFF  TO MR-QTYA (PS-SUB)          INKCS023
00762          MOVE DP015-RED              TO MR-QTYC (PS-SUB)          INKCS023
00763          MOVE DP015-REVERSE          TO MR-QTYH (PS-SUB)          INKCS023
00764          MOVE -1                     TO MR-QTYL (PS-SUB)          INKCS023
00765          SET DP030-SET-CURSOR-APPL-1 TO TRUE                      INKCS023
00766      ELSE                                                         INKCS023
00767          MOVE ASC-INV-QTY-X (PS-SUB) TO DP010I-UNEDITED-FIELD     INKCS023
00768          MOVE LENGTH OF ASC-INV-QTY (PS-SUB)                      INKCS023
00769                                      TO DP010I-MAXIMUM-DIGITS     INKCS023
00770          MOVE ZERO                   TO DP010I-MAXIMUM-DECIMALS   INKCS023
00771          SET  DP010I-NEGATIVE-NOT-ALLOWED                         INKCS023
00772                                      TO TRUE                      INKCS023
263126         CALL DP010I-NUMERIC-EDIT-ROUTINE
263126              USING DP010I-NUMERIC-EDIT-AREA
00774                                                                   INKCS023
00775          IF DP010I-ERROR-DETECTED                                 INKCS023
00776              SET  PS-ERROR                                        INKCS023
00777                   PS-ERRORS-IN-LINE                               INKCS023
00778                   DP020-MSG-FATAL        TO TRUE                  INKCS023
00779 *            ---- MUST BE NUMERIC ----                            INKCS023
00780              MOVE PC-TSYMSG-00008        TO DP020-MSG-NUMBER      INKCS023
00781              MOVE DP015-UNP-NUM-BRT-OFF  TO MR-QTYA (PS-SUB)      INKCS023
00782              MOVE DP015-RED              TO MR-QTYC (PS-SUB)      INKCS023
00783              MOVE DP015-REVERSE          TO MR-QTYH (PS-SUB)      INKCS023
00784              MOVE -1                     TO MR-QTYL (PS-SUB)      INKCS023
00785              SET DP030-SET-CURSOR-APPL-1 TO TRUE                  INKCS023
00786          ELSE                                                     INKCS023
00787              MOVE DP010I-NUMERIC-FIELD   TO ASC-INV-QTY (PS-SUB)  INKCS023
00788              IF ASC-INV-QTY (PS-SUB) = ZERO                       INKCS023
00789                  SET  PS-ERROR                                    INKCS023
00790                       PS-ERRORS-IN-LINE                           INKCS023
00791                       DP020-MSG-FATAL    TO TRUE                  INKCS023
00792 *                ---- CANNOT BE ZERO ----                         INKCS023
00793                  MOVE PC-TSYMSG-00010    TO DP020-MSG-NUMBER      INKCS023
00794                  MOVE DP015-UNP-NUM-BRT-OFF                       INKCS023
00795                                          TO MR-QTYA (PS-SUB)      INKCS023
00796                  MOVE DP015-RED          TO MR-QTYC (PS-SUB)      INKCS023
00797                  MOVE DP015-REVERSE      TO MR-QTYH (PS-SUB)      INKCS023
00798                  MOVE -1                 TO MR-QTYL (PS-SUB)      INKCS023
00799                  SET DP030-SET-CURSOR-APPL-1                      INKCS023
00800                                          TO TRUE                  INKCS023
00801              ELSE                                                 INKCS023
00802                  IF ASC-INV-QTY (PS-SUB) > PC-MAX-QTY             INKCS023
00803                      SET  PS-ERROR                                INKCS023
00804                           PS-ERRORS-IN-LINE                       INKCS023
00805                           DP020-MSG-FATAL    TO TRUE              INKCS023
00806 *                    -- CANNOT EXCEED 99 UNITS PER STKRMW LINE -- INKCS023
00807                      MOVE PC-TSYMSG-00684    TO DP020-MSG-NUMBER  INKCS023
00808                      MOVE DP015-UNP-NUM-BRT-OFF                   INKCS023
00809                                              TO MR-QTYA (PS-SUB)  INKCS023
00810                      MOVE DP015-RED          TO MR-QTYC (PS-SUB)  INKCS023
00811                      MOVE DP015-REVERSE      TO MR-QTYH (PS-SUB)  INKCS023
00812                      MOVE -1                 TO MR-QTYL (PS-SUB)  INKCS023
00813                      SET DP030-SET-CURSOR-APPL-1                  INKCS023
00814                                              TO TRUE              INKCS023
00815                  END-IF                                           INKCS023
00816              END-IF                                               INKCS023
00817          END-IF                                                   INKCS023
00818      END-IF.                                                      INKCS023
00819                                                                   INKCS023
00820 *    *------------------------------------------------------------INKCS023
00821 *    * EDIT THE SKU AND UPC                                       INKCS023
00822 *    *------------------------------------------------------------INKCS023
00823                                                                   INKCS023
00824      MOVE ASC-GET-PRICE-SW  (PS-SUB) TO PS-GOT-LAST-PRICE-SW.     INKCS023
00825      SET ASC-DONT-GET-PRICE (PS-SUB) TO TRUE.                     INKCS023
00826                                                                   INKCS023
00827      IF ASC-SKU-ENTERED (PS-SUB)                                  INKCS023
00828          IF ASC-UPC-NBR-X (PS-SUB) > SPACE                        INKCS023
00829              SET PS-ERROR                                         INKCS023
00830                  DP020-MSG-FATAL         TO TRUE                  INKCS023
00831 *            ---- SPECIFY UPC OR SKU; NOT BOTH ----               INKCS023
00832              MOVE PC-TSYMSG-00547        TO DP020-MSG-NUMBER      INKCS023
W21732             MOVE DP015-UNP-NUM-BRT-OFF  TO MR-SKUA  (PS-SUB)     INKCS023
00836                                             MR-UPCA  (PS-SUB)     INKCS023
W21732             MOVE DP015-RED              TO MR-SKUC  (PS-SUB)     INKCS023
00840                                             MR-UPCC  (PS-SUB)     INKCS023
W21732             MOVE DP015-REVERSE          TO MR-SKUH  (PS-SUB)     INKCS023
00844                                             MR-UPCH  (PS-SUB)     INKCS023
W21732             MOVE -1                     TO MR-SKUL  (PS-SUB)     INKCS023
00846              SET DP030-SET-CURSOR-APPL-1 TO TRUE                  INKCS023
00847          ELSE                                                     INKCS023
00848              PERFORM 3325-EDIT-SKU                                INKCS023
00849              IF PS-VALID-SKU                                      INKCS023
00850                  PERFORM 3350-VALIDATE-SKU                        INKCS023
00851              END-IF                                               INKCS023
00852          END-IF                                                   INKCS023
00853      ELSE                                                         INKCS023
00854          MOVE ASC-UPC-NBR-X (PS-SUB)                              INKCS023
00855                                     TO DP010I-UNEDITED-FIELD      INKCS023
00856          MOVE LENGTH OF ASC-UPC-NBR-X (PS-SUB)                    INKCS023
00857                                     TO DP010I-MAXIMUM-DIGITS      INKCS023
00858          MOVE ZERO                  TO DP010I-MAXIMUM-DECIMALS    INKCS023
00859          SET  DP010I-NEGATIVE-NOT-ALLOWED                         INKCS023
00860                                     TO TRUE                       INKCS023
263126         CALL DP010I-NUMERIC-EDIT-ROUTINE
263126              USING DP010I-NUMERIC-EDIT-AREA
00862                                                                   INKCS023
00863          IF DP010I-ERROR-DETECTED                                 INKCS023
00864              SET  PS-ERROR                                        INKCS023
00865                   DP020-MSG-FATAL        TO TRUE                  INKCS023
00866 *            ---- MUST BE NUMBERIC ----                           INKCS023
00867              MOVE PC-TSYMSG-00008        TO DP020-MSG-NUMBER      INKCS023
00868              MOVE DP015-UNP-NUM-BRT-OFF  TO MR-UPCA (PS-SUB)      INKCS023
00869              MOVE DP015-RED              TO MR-UPCC (PS-SUB)      INKCS023
00870              MOVE DP015-REVERSE          TO MR-UPCH (PS-SUB)      INKCS023
00871              MOVE -1                     TO MR-UPCL (PS-SUB)      INKCS023
00872              SET DP030-SET-CURSOR-APPL-1 TO TRUE                  INKCS023
00873          ELSE                                                     INKCS023
00874              MOVE DP010I-NUM-FIELD-15-DIG-2-DEC                   INKCS023
00875                                        TO ASC-UPC-NBR (PS-SUB)    INKCS023
00876              IF ASC-UPC-NBR (PS-SUB) = ZERO                       INKCS023
00877                  SET  PS-ERROR                                    INKCS023
00878                       DP020-MSG-FATAL    TO TRUE                  INKCS023
00879 *                ---- CANNOT BE ZERO ----                         INKCS023
00880                  MOVE PC-TSYMSG-00010    TO DP020-MSG-NUMBER      INKCS023
00881                  MOVE DP015-UNP-NUM-BRT-OFF                       INKCS023
00882                                          TO MR-UPCA (PS-SUB)      INKCS023
00883                  MOVE DP015-RED          TO MR-UPCC (PS-SUB)      INKCS023
00884                  MOVE DP015-REVERSE      TO MR-UPCH (PS-SUB)      INKCS023
00885                  MOVE -1                 TO MR-UPCL (PS-SUB)      INKCS023
00886                  SET DP030-SET-CURSOR-APPL-1                      INKCS023
00887                                          TO TRUE                  INKCS023
00888              ELSE                                                 INKCS023
00889                  PERFORM 3300-VALIDATE-UPC                        INKCS023
00890              END-IF                                               INKCS023
00891          END-IF                                                   INKCS023
00892      END-IF.                                                      INKCS023
00893                                                                   INKCS023
00894                                                                   INKCS023
00895 *    *------------------------------------------------------------INKCS023
00896 *    * EDIT THE CUTOFF PRICE. FOR CLEARANCE SKUS OR UPCS/SKUS     INKCS023
00897 *    * WITH A SEQUENCE NUMBER OF '000' OR '999', THE PRICE MUST   INKCS023
00898 *    * BE ENTERED ON THE SCREEN.  IN ALL OTHER CASES, THE PRICE   INKCS023
00899 *    * IS RETRIEVED FROM THE SKU STORE TABLE. A SWITCH WAS SET    INKCS023
00900 *    * WHILE EDITING THE UPC OR SKU, INDICATING WHETHER THE SOURCEINKCS023
00901 *    * OF THE CUTOFF PRICE SHOULD BE THE SCREEN OR THE TABLE.     INKCS023
00902 *    *                                                            INKCS023
00903 *    * BECAUSE THE EDITING OF THE PRICE FIELD IS DEPENDENT UPON   INKCS023
00904 *    * THE RESULTS OF EDITING THE UPC OR SKU, THE NORMAL          INKCS023
00905 *    * CONVENTION OF EDITING FIELDS IN REVERSE ORDER CANNOT BE    INKCS023
00906 *    * FOLLOWED WITHIN EACH LINE. THEREFORE, IF AN ERROR IS       INKCS023
00907 *    * DISCOVERED WHILE EDITING PRICE, THE ERROR MESSAGE IS       INKCS023
00908 *    * ONLY FILLED IN IF NO OTHER ERRORS OCCURRED ON THE LINE.    INKCS023
00909 *    * THIS WILL ENSURE THAT THE ERROR MESSAGE DISPLAYED          INKCS023
00910 *    * CORRESPONDS TO THE FIELD ON WHICH THE CURSOR IS SET.       INKCS023
00911 *    *------------------------------------------------------------INKCS023
00912                                                                   INKCS023
IN2001*    CLEARANCE                                                    INKCS023
00913      IF ASC-GET-PRICE-FROM-SCREEN (PS-SUB)                        INKCS023
IN2001         PERFORM 3275-PROCESS-CLEARANCE-ITEM                      INKCS023
01046      END-IF.                                                      INKCS023
01047  EJECT                                                            INKCS023
      *
23511C*------------------------------------------------------------
23511C* THIS PARAGRAPH WAS MOVED FROM WITHIN 3250-EDIT-MAP-DETAIL-FIELDS
23511C* TO CHECK THE DATES TO DECIDE IF THE PRICE SHOULD BE DISPLAYED
23511C* OR THE USER SHOULD ENTER THE PRICE
23511C*------------------------------------------------------------
      *
23511C  3265-PROCESS-CLEARANCE-DATES.
W28545      IF IN400-PRC-STAT-CHG-DTE-OUT
W28545                         EQUAL IN400-PRC-UPC-CHG-DTE-OUT
W28545          SET ASC-GET-PRICE-FROM-INPD400 (PS-SUB) TO TRUE
W28545          MOVE IN400-PRC-STATUS-OUT TO PV-STR000-UPC-STAT-CDE
W28545          MOVE IN400-PRC-UNIT-RTL-OUT
23511C              TO PV-STR000-UNT-RTL-AMT
23511C                 ASC-ITM-UNIT-PR-AMT-X-FMT (PS-SUB)
23511C                 ASC-ITM-UNIT-PR-AMT       (PS-SUB)
W28545          MOVE IN400-PRC-MEITGP-NBR TO PV-STR000-MEITGP-NBR
23511C      ELSE
23511C          SET ASC-GET-PRICE-FROM-SCREEN (PS-SUB) TO TRUE
23511C      END-IF.
      *
IN2001*                                                                 INKCS023
IN2001*------------------------------------------------------------     INKCS023
IN2001* THIS PARAGRAPH WAS MOVED FROM WITHIN 3250-EDIT-MAP-DETAIL-FIELDSINKCS023
IN2001* TO ACCOUNT FOR RE-ANALYSIS OF CLEARANCE ITEMS (IF STR000 ROW WASINKCS023
IN2001* FOUND TO BE A MIXED ('25') STATUS).                             INKCS023
IN2001* THIS WILL BE EXECUTED AT LEAST ONCE FROM THE INITIAL REVIEW OF  INKCS023
IN2001* SWITCH THAT WAS SET FROM THE STR000 ROW. IF THE STORE-SPECIFIC  INKCS023
IN2001* ROW IS CALLED AND FOUND TO BE A CLEARANCE STATUS, THIS MUST BE  INKCS023
IN2001* RE-EVALUATED.                                                   INKCS023
IN2001*------------------------------------------------------------     INKCS023
IN2001*                                                                 INKCS023
IN2001 3275-PROCESS-CLEARANCE-ITEM.                                     INKCS023
IN2001*                                                                 INKCS023
IN2001     IF PS-GOT-LAST-PRICE-FROM-TSKST                              S023
IN2001         MOVE SPACE TO ASC-ITM-UNIT-PR-AMT-X (PS-SUB)             S023
IN2001     END-IF                                                       S023
IN2001     IF ASC-ITM-UNIT-PR-AMT-X (PS-SUB) = SPACE                    S023
IN2001         IF PS-NO-ERRORS-IN-LINE                                  S023
IN2001             SET DP020-MSG-FATAL         TO TRUE                  S023
IN2001*            ---- REQUIRED FIELD ----                             S023
IN2001             MOVE PC-TSYMSG-00007        TO DP020-MSG-NUMBER      S023
IN2001             MOVE -1                     TO MR-PRCEL (PS-SUB)     S023
IN2001             SET DP030-SET-CURSOR-APPL-1 TO TRUE                  S023
IN2001         END-IF                                                   S023
IN2001         SET PS-ERROR                    TO TRUE                  S023
IN2001         MOVE DP015-UNP-NUM-BRT-OFF      TO MR-PRCEA (PS-SUB)     S023
IN2001         MOVE DP015-RED                  TO MR-PRCEC (PS-SUB)     S023
IN2001         MOVE DP015-REVERSE              TO MR-PRCEH (PS-SUB)     S023
IN2001     ELSE                                                         S023
IN2001         MOVE ASC-ITM-UNIT-PR-AMT-X (PS-SUB)                      S023
IN2001                                    TO DP010I-UNEDITED-FIELD      S023
IN2001         MOVE PC-ITM-UNIT-PR-MAX-DIGITS                           S023
IN2001                                    TO DP010I-MAXIMUM-DIGITS      S023
IN2001         MOVE 2                     TO DP010I-MAXIMUM-DECIMALS    S023
IN2001         SET  DP010I-NEGATIVE-NOT-ALLOWED                         S023
IN2001                                    TO TRUE                       S023
263126         CALL DP010I-NUMERIC-EDIT-ROUTINE
263126              USING DP010I-NUMERIC-EDIT-AREA
IN2001                                                                  S023
IN2001         IF DP010I-ERROR-DETECTED                                 S023
IN2001             IF PS-NO-ERRORS-IN-LINE                              S023
IN2001                 SET DP020-MSG-FATAL      TO TRUE                 S023
IN2001*                ---- MUST BE NUMBERIC, NON-NEGATIVE, ----        S023
IN2001*                ---- AND WITHIN SIZE CONSTRAINT      ----        S023
IN2001                 MOVE PC-TSYMSG-00343     TO DP020-MSG-NUMBER     S023
IN2001                 MOVE -1                  TO MR-PRCEL (PS-SUB)    S023
IN2001                 SET DP030-SET-CURSOR-APPL-1                      S023
IN2001                                          TO TRUE                 S023
IN2001             END-IF                                               S023
IN2001             SET  PS-ERROR                TO TRUE                 S023
IN2001             MOVE DP015-UNP-NUM-BRT-OFF   TO MR-PRCEA (PS-SUB)    S023
IN2001             MOVE DP015-RED               TO MR-PRCEC (PS-SUB)    S023
IN2001             MOVE DP015-REVERSE           TO MR-PRCEH (PS-SUB)    S023
IN2001         ELSE                                                     S023
IN2001             MOVE DP010I-NUMERIC-FIELD                            S023
IN2001                         TO ASC-ITM-UNIT-PR-AMT-X-FMT (PS-SUB)    S023
IN2001                            ASC-ITM-UNIT-PR-AMT       (PS-SUB)    S023
IN2001             IF DP010I-NUMERIC-FIELD  = ZERO                      S023
IN2001                 IF PS-NO-ERRORS-IN-LINE                          S023
IN2001                     SET  DP020-MSG-FATAL TO TRUE                 S023
IN2001*                    ---- CANNOT BE ZERO ----                     S023
IN2001                     MOVE PC-TSYMSG-00010 TO DP020-MSG-NUMBER     S023
IN2001                     MOVE -1              TO MR-PRCEL (PS-SUB)    S023
IN2001                     SET DP030-SET-CURSOR-APPL-1                  S023
IN2001                                          TO TRUE                 S023
IN2001                 END-IF                                           S023
IN2001                 SET  PS-ERROR            TO TRUE                 S023
IN2001                 MOVE DP015-UNP-NUM-BRT-OFF                       S023
IN2001                                          TO MR-PRCEA (PS-SUB)    S023
IN2001                 MOVE DP015-RED           TO MR-PRCEC (PS-SUB)    S023
IN2001                 MOVE DP015-REVERSE       TO MR-PRCEH (PS-SUB)    S023
IN2001             ELSE                                                 S023
IN2001                 IF ASC-ITM-UNIT-PR-AMT (PS-SUB)                  S023
IN2001                                                < PC-MIN-PRICE    S023
IN2001                 OR ASC-ITM-UNIT-PR-AMT (PS-SUB)                  S023
IN2001                                                > PC-MAX-PRICE    S023
IN2001                     IF PS-NO-ERRORS-IN-LINE                      S023
IN2001                         SET  DP020-MSG-FATAL TO TRUE             S023
IN2001*                        -- UNIT PRICE MUST BE BETWEEN .20 --     S023
IN2001*                        -- AND 999.99                     --     S023
IN2001                         MOVE PC-TSYMSG-00685                     S023
IN2001                                          TO DP020-MSG-NUMBER     S023
IN2001                         MOVE -1          TO MR-PRCEL (PS-SUB)    S023
IN2001                         SET DP030-SET-CURSOR-APPL-1              S023
IN2001                                          TO TRUE                 S023
IN2001                     END-IF                                       S023
IN2001                     SET  PS-ERROR        TO TRUE                 S023
IN2001                     MOVE DP015-UNP-NUM-BRT-OFF                   S023
IN2001                                          TO MR-PRCEA (PS-SUB)    S023
IN2001                     MOVE DP015-RED       TO MR-PRCEC (PS-SUB)    S023
IN2001                     MOVE DP015-REVERSE   TO MR-PRCEH (PS-SUB)    S023
IN2001                 ELSE                                             S023
IN2001                     MOVE DP015-UNP-NUM-NOR-OFF                   S023
IN2001                                          TO MR-PRCEA (PS-SUB)    S023
IN2001                     MOVE DP015-GREEN     TO MR-PRCEC (PS-SUB)    S023
IN2001                     MOVE DP015-UNDERLINE                         S023
IN2001                                          TO MR-PRCEH (PS-SUB)    S023
IN2001                 END-IF                                           S023
IN2001             END-IF                                               S023
IN2001         END-IF                                                   S023
IN2001     END-IF.                                                      S023
01048 *                                                                 INKCS023
01048 *----------------------------------------------------------------*INKCS023
W26682* FOR ALL UPC'S, NO LONGER VALIDATE THE CHECK DIGIT.              INKCS023
01050 * IF THE UPC IS AN INTERNAL UPC WITH A SEQUENCE NUMBER OF '000'  *INKCS023
01051 * OR '999' IN THE EMBEDDED SKU, VALIDATE THE DEPARTMENT AND       INKCS023
01052 * CLASS AGAINST TMDSEAR. IF NOT, CONVERT THE UPC TO A SKU.        INKCS023
01053 *----------------------------------------------------------------*INKCS023
01054                                                                   INKCS023
01055  3300-VALIDATE-UPC.                                               INKCS023
01056                                                                   INKCS023
W28545     INITIALIZE IN400-PV-INPUT-AREA.
W28545     MOVE ASC-UPC-NBR (PS-SUB) TO PV-DB2-UPC-NBR                  INKCS023
W28545                                  IN400-PV-UPC-NBR-IN.            18500100
W28545     SET IN400-PV-UPC-NBR-LOOKUP TO TRUE.                         18500200
W28545     MOVE PV-STR-NBR             TO IN400-PV-LOC-IN-NUM.          18500300
W28545     MOVE PV-PLND-INV-TMST       TO IN400-PV-EFF-DTE-IN.          18500400
W28545     PERFORM IN400-LOOKUP-RETAIL.                                 18500600
W28545     EVALUATE TRUE                                                18500700
W28545         WHEN IN400-PV-FOUND                                      18500800
W28582*            IF IN400-PV-DUMMY-SKU
W28582             IF IN400-PV-DUMMY-UPC
W28545                 SET ASC-GET-PRICE-FROM-SCREEN (PS-SUB)
W28545                                                      TO TRUE
W28582                 MOVE IN400-ATT-SKU-OUT TO ASC-SKU-NBR (PS-SUB)
W28582                 MOVE IN400-ATT-ITM-NBR-OUT TO PV-DB2-ITEM-NMBR
W28545             ELSE
W28545                 PERFORM 3352-PROCESS-PRICE                       18501100
W28545                 MOVE IN400-PRC-SKU-OUT TO ASC-SKU-NBR (PS-SUB)   18502000
W28545                 MOVE IN400-PRC-ITM-NBR-OUT TO PV-DB2-ITEM-NMBR   18502200
W28545             END-IF                                               18502200
W28545         WHEN IN400-PV-NOT-FOUND                                  18503000
W28545             SET  PS-ERROR                                        18504000
W28545                  PS-ERRORS-IN-LINE                               18505000
W28545                  DP020-MSG-FATAL  TO TRUE                        18506000
W28545*            ---- INVALID ENTRY ----                              18507000
W28545             MOVE PC-TSYMSG-00005  TO DP020-MSG-NUMBER            18508000
W28545             MOVE DP015-UNP-NUM-BRT-OFF                           18509100
W28545                                   TO MR-UPCA (PS-SUB)            18509200
W28545             MOVE DP015-RED  TO MR-UPCC (PS-SUB)                  18509300
W28545             MOVE DP015-REVERSE    TO MR-UPCH (PS-SUB)            18509400
W28545             MOVE -1         TO MR-UPCL (PS-SUB)                  18509600
W28545             SET DP030-SET-CURSOR-APPL-1 TO TRUE                  18509700
W28545*            END-IF                                               18509700
W28545         WHEN IN400-PV-SQL-ERROR                                  18509800
W28545             SET  PS-ERROR                                        18509900
W28545             DP020-MSG-WARNING TO TRUE                            18510000
W28545*            ---- WAIT 15 SECONDS, THEN RETRY ----                18510100
W28545             MOVE PC-TSYMSG-00551  TO DP020-MSG-NUMBER            18510200
W28545     END-EVALUATE.                                                18510400
01171                                                                   INKCS023
01172  3325-EDIT-SKU.                                                   INKCS023
01173                                                                   INKCS023
01174 *    *------------------------------------------------------------INKCS023
01175 *    * EDIT THE SKU                                               INKCS023
01176 *    *------------------------------------------------------------INKCS023
01177                                                                   INKCS023
W21732     IF ASC-SKU-NBR (PS-SUB) > SPACE                              INKCS023
W21732         MOVE ASC-SKU-NBR (PS-SUB) TO DP010I-UNEDITED-FIELD       INKCS023
W21732         MOVE LENGTH OF ASC-SKU-NBR (PS-SUB)                      INKCS023
W21732                                   TO DP010I-MAXIMUM-DIGITS       INKCS023
01182          MOVE ZERO                 TO DP010I-MAXIMUM-DECIMALS     INKCS023
01183          SET  DP010I-NEGATIVE-NOT-ALLOWED TO TRUE                 INKCS023
263126         CALL DP010I-NUMERIC-EDIT-ROUTINE
263126              USING DP010I-NUMERIC-EDIT-AREA
01185                                                                   INKCS023
01186          IF DP010I-ERROR-DETECTED                                 INKCS023
01187                  SET  PS-ERROR                                    INKCS023
01188                       PS-INVALID-SKU                              INKCS023
01189                       DP020-MSG-FATAL        TO TRUE              INKCS023
01190 *                ---- MUST BE NUMBERIC ----                       INKCS023
01191                  MOVE PC-TSYMSG-00008        TO DP020-MSG-NUMBER  INKCS023
W21732                 MOVE DP015-UNP-NUM-BRT-OFF  TO MR-SKUA  (PS-SUB) INKCS023
W21732                 MOVE DP015-RED              TO MR-SKUC  (PS-SUB) INKCS023
W21732                 MOVE DP015-REVERSE          TO MR-SKUH  (PS-SUB) INKCS023
W21732                 MOVE -1                     TO MR-SKUL  (PS-SUB) INKCS023
01196                  SET DP030-SET-CURSOR-APPL-1 TO TRUE              INKCS023
01197          ELSE                                                     INKCS023
W21732             MOVE DP010I-NUM-FIELD-11-DIG-6-DEC                   INKCS023
W21732                                     TO ASC-SKU-NBR-N (PS-SUB)    INKCS023
01200          END-IF                                                   INKCS023
01201      END-IF.                                                      INKCS023
01202                                                                   INKCS023
01262                                                                   INKCS023
01263 *-----------------------------------------------------------------INKCS023
01264 * IF THE SKU HAS A SEQUENCE NUMBER OF '000' OR '999', VALIDATE   *INKCS023
01265 * THE DEPARTMENT AND CLASS AGAINST TMDSEAR.  IF NOT, LOOK UP     *INKCS023
W21732* SKU ON TSKXREF. IF IT IS NOT FOUND, IT IS INVALID. IF THE SKU  *INKCS023
01267 * WAS RENUMBERED ON OR BEFORE THE ACTUAL INVENTORY DATE, THE     *INKCS023
01268 * RENUMBERED SKU IS OBTAINED SO THAT ITS ITEM NUMBER CAN BE USED *INKCS023
01269 * LATER TO RETRIEVE THE CUTOFF PRICE.                            *INKCS023
01270 *----------------------------------------------------------------*INKCS023
01271                                                                   INKCS023
01272  3350-VALIDATE-SKU.                                               INKCS023
01273                                                                   INKCS023
W28545     INITIALIZE IN400-PV-INPUT-AREA.
W28545     MOVE ASC-SKU-NBR (PS-SUB) TO PV-DB2-SKU-NBR                  INKCS023
W28545                                  IN400-PV-SKU-IN-NUM.            20201000
W28545     SET IN400-PV-SKU-LOOKUP   TO TRUE.                           20202000
W28545     MOVE PV-STR-NBR           TO IN400-PV-LOC-IN-NUM.            20203000
W28545     MOVE PV-PLND-INV-TMST     TO IN400-PV-EFF-DTE-IN.            20203100
W28545     PERFORM IN400-LOOKUP-RETAIL.                                 20206000
W28545     EVALUATE TRUE                                                20207000
W28545         WHEN IN400-PV-FOUND                                      20208000
W28545             IF IN400-PV-DUMMY-SKU
W28545                 SET ASC-GET-PRICE-FROM-SCREEN (PS-SUB)
W28545                                                      TO TRUE
W28545             ELSE
W28545                 PERFORM 3352-PROCESS-PRICE                       20209200
W28545             END-IF                                               20209200
W28545         WHEN IN400-PV-NOT-FOUND                                  20209300
W28545             SET PS-ERROR                                         20209400
W28545                 PS-ERRORS-IN-LINE                                20209500
W28545                 DP020-MSG-FATAL         TO TRUE                  20209600
W28545*            ---- INVALID ENTRY ----                              20209700
W28545             MOVE PC-TSYMSG-00005        TO DP020-MSG-NUMBER      20209800
W28545             MOVE DP015-UNP-NUM-BRT-OFF  TO MR-SKUA  (PS-SUB)     20209900
W28545             MOVE DP015-RED              TO MR-SKUC  (PS-SUB)     20210000
W28545             MOVE DP015-REVERSE          TO MR-SKUH  (PS-SUB)     20210100
W28545             MOVE -1                     TO MR-SKUL  (PS-SUB)     20210200
W28545             SET DP030-SET-CURSOR-APPL-1 TO TRUE                  20210300
W28545         WHEN IN400-PV-SQL-ERROR                                  20210400
W28545             SET  PS-ERROR                                        20210500
W28545                  DP020-MSG-WARNING TO TRUE                       20210600
W28545*            ---- WAIT 15 SECONDS, THEN RETRY ----                20210700
W28545             MOVE PC-TSYMSG-00551 TO DP020-MSG-NUMBER             20210800
W28545     END-EVALUATE.                                                20210900
01339                                                                   INKCS023
W28545 3352-PROCESS-PRICE.                                              20841000
W28545                                                                  20842000
W28545     IF IN400-PS-RENUMBER-SKU                                     20842000
W28545         PERFORM 3380-PROCESS-RENUMBERED-SKU                      20842000
W28545         SET DP020-MSG-INFORMATIONAL   TO TRUE
W28545         MOVE PC-TSYMSG-00453      TO DP020-MSG-NUMBER
W28545         MOVE IN400-PRC-SKU-OUT    TO PV-RSKU-NBR
W28545                                      ASC-SKU-NBR     (PS-SUB)
W28545         MOVE PV-RENUMBERED-SKU    TO DP020-MSG-TEXT
W28545     END-IF.
W28545
W28545     IF IN400-PRC-STATUS-OUT = PC-CLEARANCE-SKU-STATUS            20842000
W28545         PERFORM 3265-PROCESS-CLEARANCE-DATES                     20842000
W28545     ELSE                                                         20842000
W28545         IF IN400-PRC-STATUS-OUT = PC-MIXED-STATUS                20842000
W28545             SET ASC-GET-PRICE-FROM-INPD400 (PS-SUB) TO TRUE      20842000
W28545*            MOVE IN400-PRC-ITM-NBR-OUT TO PV-DB2-ITEM-NMBR       20842000
974311             MOVE IN400-PRC-UNIT-RTL-OUT TO                       20842000
974311                                ASC-ITM-UNIT-PR-AMT-X-FMT (PS-SUB)20842000
974311                                ASC-ITM-UNIT-PR-AMT       (PS-SUB)
W28545         ELSE                                                     20842000
W28545             SET ASC-GOT-STR000-PRICE (PS-SUB)  TO TRUE           20842000
W28545             PERFORM 3359-CHECK-GROUP-PRICING                     20842000
W28545         END-IF                                                   20842000
W28545     END-IF.                                                      20842000
W28545                                                                  20850100
W28545                                                                  20850200
IN2001******************************************************************
IN2001* USE GROUP ITEM INDICATOR FROM TUPCPLS TO DETERMINE IF RETAIL
IN2001* IS PRICED AS PART OF A GROUPING. IF SO - RETRIEVE GROUP PRICING
IN2001* FROM DB2 (TMITGPL). THEN POPULATES SCREEN ITEM UNIT RETAIL
IN2001******************************************************************
IN2001 3359-CHECK-GROUP-PRICING.
IN2001
W28545     IF IN400-PRC-MEITGP-NBR NOT EQUAL 0
IN2001      COMPUTE PV-GROUP-UNIT-RTL ROUNDED =
W28545            (IN400-PRC-GP-AMT /
W28545             IN400-PRC-MITGPL-QTY )
IN2001         MOVE PV-GROUP-UNIT-RTL      TO
IN2001          ASC-ITM-UNIT-PR-AMT-X-FMT (PS-SUB)
IN2001          ASC-ITM-UNIT-PR-AMT       (PS-SUB)
IN2001      ELSE
W28545          MOVE IN400-PRC-UNIT-RTL-OUT TO
IN2001          ASC-ITM-UNIT-PR-AMT-X-FMT (PS-SUB)
IN2001          ASC-ITM-UNIT-PR-AMT       (PS-SUB)
IN2001      END-IF.
IN2001
01449  3380-PROCESS-RENUMBERED-SKU.                                     INKCS023
01450                                                                   INKCS023
01451      PERFORM 3385-FMT-STCHNG-DTE-FOR-CMPARE.                      INKCS023
01452                                                                   INKCS023
01453      IF ASC-ACTL-INV-DTE-YYYYMMDD < PV-STAT-CHNGD-DATE-YYYYMMDD   INKCS023
W28545         SET ASC-GET-PRICE-FROM-INPD400 (PS-SUB) TO TRUE          INKCS023
W28545         MOVE IN400-PRC-ITM-NBR-OUT TO PV-DB2-ITEM-NMBR           INKCS023
01456      ELSE                                                         INKCS023
W28545         MOVE IN400-PRC-UPC-OUT TO PV-DB2-UPC-NBR                 INKCS023
01461      END-IF.                                                      INKCS023
01462                                                                   INKCS023
01464  3385-FMT-STCHNG-DTE-FOR-CMPARE.                                  INKCS023
01465                                                                   INKCS023
01466      PERFORM DP017-0000-GET-CURR-DATE-TIME.                       INKCS023
01468      SET  DPG51-ACTUAL-CALENDAR-ONLY  TO TRUE.                    INKCS023
01469      SET  DPG51-DO-NOT-INCR-DECR-DATE TO TRUE.                    INKCS023
01470      MOVE ZERO                      TO DPG51-INCR-DECR-DAYS-9     INKCS023
01471                                        DPG51-INCR-DECR-BUS-DAYS-9.INKCS023
01472      SET  DPG52-LK-DTE-GREG           TO TRUE.                    INKCS023
W28545     MOVE IN400-PRC-STAT-CHG-DTE-OUT TO PV-DATE.                  INKCS023
W21732     MOVE PV-DATE(3:2)            TO PV-YYMMDD-YY.
W21732     MOVE PV-DATE(6:2)            TO PV-YYMMDD-MM.
W21732     MOVE PV-DATE(9:2)            TO PV-YYMMDD-DD.
01474      MOVE PV-YYMMDD-MMDD      TO PV-MMDDYY-MMDD.                  INKCS023
01475      MOVE PV-YYMMDD-YY        TO PV-MMDDYY-YY.                    INKCS023
01476      MOVE PV-MMDDYY-DATE      TO DPG52-LK-DATE-INPUT.             INKCS023
01477                                                                   INKCS023
01478      CALL PC-CALENDAR-ROUTINE USING DPG51                         INKCS023
01479                                     DPG52                         INKCS023
01480                                     DPG53                         INKCS023
01481                                     DPG54                         INKCS023
01482                                     DPG55                         INKCS023
01483                                     DPG56.                        INKCS023
01484      EVALUATE TRUE                                                INKCS023
01485      WHEN DPG54-SEVERE-ERROR                                      INKCS023
01486      WHEN DPG54-DATE-INVALID                                      INKCS023
01487          MOVE '3385-FMT-STCHNG-DTE-FOR-CMPARE'                    INKCS023
01488                                TO  DP013-PARAGRAPH                INKCS023
01489          MOVE 'ERROR DETECTED BY CALENDAR ROUTINE:'               INKCS023
01490                                TO  DP013-MESSAGE-TEXT (1)         INKCS023
01491          MOVE DPG54-ERROR-MESSAGE                                 INKCS023
01492                                TO  DP013-MESSAGE-TEXT (2)         INKCS023
01493          SET DP013-LOGIC-ABEND TO  TRUE                           INKCS023
01494          PERFORM DP013-0000-PROCESS-ABEND                         INKCS023
01495      END-EVALUATE.                                                INKCS023
01496                                                                   INKCS023
01497      MOVE DPG55-DB2-ISO-DATE TO PV-STAT-CHNGD-DATE-YYYYMMDD.      INKCS023
01498                                                                   INKCS023
01499                                                                   INKCS023
01500 *--------------------------------------------------------------   INKCS023
01501 *  PERFORM THIS PARAGRAPH ITERATIVELY UNTIL THE RENUMBERED        INKCS023
01502 *  SKU IS FOUND OR THE SKU IS DETERMINED TO BE INVALID:           INKCS023
01503 *                                                                 INKCS023
01504 *   - CONVERT THE SKU TO AN INTERNAL UPC CODE.                    INKCS023
01505 *   - CONVERT THE UPC TO A SKU. (THE UPC CODE WILL ALWAYS POINT   INKCS023
01506 *     TO THE MOST RECENT RENUMBERED SKU.)                         INKCS023
01507 *   - IF THE UPC WAS NOT SUCCESSFULLY CONVERTED TO A SKU, RETURN  INKCS023
01508 *     AN ERROR.                                                   INKCS023
01509 *   - IF THE UPC WAS SUCCESSFULLY CONVERTED TO A SKU, CHECK IF    INKCS023
01510 *     THAT SKU IS RENUMBERED.  IS SO, PERFORM THE ROUTINE AGAIN.  INKCS023
01511 *     IF NOT, STOP HERE.                                          INKCS023
01512 *--------------------------------------------------------------   INKCS023
01513                                                                   INKCS023
IN2000 3480-READ-TINVPAR.
IN2000
W26600     MOVE ASC-KEY-STORE-NBR   TO INVPAR-LOC-NBR.
J28545     MOVE IN001-INV-ID        TO INVPAR-INV-ID.
W26600
IN2000     EXEC SQL
J28545       SELECT A.PLND_INV_DTE
J28545             ,A.LOC_NBR
J28545             ,A.STKRM_WDRWL_IND
IN2000       INTO  :INVPAR-PLND-INV-DTE
W26600            ,:INVPAR-LOC-NBR
22000             ,:INVPAR-STKRM-WDRWL-IND
J28545       FROM   TINVPAR A
J28545            , TINCNTL B
J28545       WHERE A.LOC_NBR   = :INVPAR-LOC-NBR
J28545         AND A.INV_ID    = :INVPAR-INV-ID
J28545         AND A.INV_ID    = B.INV_ID
W33304         AND A.ACTL_FIN_BK_DTE = '9999-09-09'
W33304         AND A.LOC_INV_STAT_CDE = 'IN'
W33304         AND B.ACTV_IND = 'Y'
IN2000     END-EXEC.
IN2000
IN2000     EVALUATE TRUE
IN2000         WHEN SQLCODE = 0
W26600             MOVE INVPAR-LOC-NBR        TO PV-STR-NBR
IN2000             MOVE INVPAR-PLND-INV-DTE   TO PV-PLND-INV-DTE
IN2000             MOVE PV-PLND-INV-TIMESTAMP TO PV-PLND-INV-TMST
SMJ            WHEN SQLCODE = -913
SMJ            WHEN SQLCODE = -904
SMJ            WHEN SQLCODE = -911
SMJ                SET  PS-ERROR
SMJ                     PS-UNAVAILABLE-RESOURCE  TO TRUE
IN2000         WHEN SQLCODE = +100
IN2000         WHEN SQLWARN NOT = SPACE
IN2000         WHEN SQLCODE NOT = ZERO
IN2000             MOVE '3480-READ-TINVPAR'
IN2000                               TO DP013-PARAGRAPH
IN2000             MOVE 'RETRIEVE STORE PLANNED INV DATE'
IN2000                               TO DP013-MESSAGE-TEXT(1)
IN2000             MOVE SQLCA        TO DP013-SQLCA
IN2000             MOVE 'TINVPAR'    TO DP013-DB2-TABLE-NAME (1)
IN2000             SET DP013-DB2-ABEND
IN2000                 DP013-XCTL-DISPLAY-RESTART TO TRUE
IN2000             PERFORM DP013-0000-PROCESS-ABEND
IN2000     END-EVALUATE.
IN2000
01594                                                                   INKCS023
01595 *----------------------------------------------------------------*INKCS023
01596 * POSITION THE CURSOR FOR THE INITIAL PANEL.                      INKCS023
01597 *----------------------------------------------------------------*INKCS023
01598                                                                   INKCS023
01599  4000-BUILD-INITIAL-PANEL.                                        INKCS023
01600                                                                   INKCS023
01601      SET DP030-SET-CURSOR-APPL-1 TO TRUE.                         INKCS023
01602      MOVE LOW-VALUES TO IN023AO.                                  INKCS023
W21732     MOVE -1 TO MR-SKUL(1).                                       INKCS023
W26600     MOVE ASC-KEY-STORE-NBR-X TO ASTRNBRO.                        INKCS023
01604      MOVE ASC-STORE-NAME      TO ASTNAMEO.                        INKCS023
01605      MOVE ASC-KEY-SHEET-NBR-X TO ASHEETO.                         INKCS023
01606                                                                   INKCS023
01607 *----------------------------------------------------------------*INKCS023
01608 * CHECK IF THE SHEET THAT IS BEING ADDED ALREADY EXISTS. IF SO,  *INKCS023
01609 * RETURN AN ERROR ON THE SELECTION SCREEN. IF NOT, SAVE THE      *INKCS023
01610 * NECESSARY INTER-APPL COMMAREA FIELDS IN THE ASC AND PROCEED    *INKCS023
01611 * WITH THE ADD.                                                   INKCS023
01612 *----------------------------------------------------------------*INKCS023
01613                                                                   INKCS023
01614  4100-CHECK-IF-SHEET-EXISTS.                                      INKCS023
01615                                                                   INKCS023
01616      PERFORM 4110-OPEN-CURSOR.                                    INKCS023
01617                                                                   INKCS023
01618      PERFORM 4120-FETCH.                                          INKCS023
01619                                                                   INKCS023
01620      EVALUATE TRUE                                                INKCS023
01621          WHEN SQLCODE = 0                                         INKCS023
01622              SET DP020-NEXT-ACT-APPL-ERROR                        INKCS023
01623                  DP020-MSG-FATAL           TO TRUE                INKCS023
01624 *            ---- SHEET HAS ALREADY BEEN ADDED ----               INKCS023
01625              MOVE PC-TSYMSG-00552 TO DP020-MSG-NUMBER             INKCS023
01626              MOVE IN001-SHEET-NBR-X TO DP020-MSG-TEXT             INKCS023
01627          WHEN SQLCODE = +100                                      INKCS023
01628              MOVE IN001-STORE-NAME  TO ASC-STORE-NAME             INKCS023
01629              MOVE IN001-STORE-NBR-X TO ASC-KEY-STORE-NBR-X        INKCS023
01630              MOVE IN001-SHEET-NBR-X TO ASC-KEY-SHEET-NBR-X        INKCS023
01631              MOVE IN001-ACTL-INV-DTE-YYYYMMDD                     INKCS023
01632                               TO ASC-ACTL-INV-DTE-YYYYMMDD        INKCS023
01633              MOVE IN001-IN-WITHDRAWAL-PERIOD-SW                   INKCS023
01634                             TO ASC-IN-WITHDRAWAL-PERIOD-SW        INKCS023
01635              MOVE IN001-ITM-CTOFF-IND                             INKCS023
01636                             TO ASC-ITM-CTOFF-IND                  INKCS023
01637          WHEN OTHER                                               INKCS023
01638              SET DP020-NEXT-ACT-APPL-ERROR                        INKCS023
01639                  DP020-MSG-WARNING         TO TRUE                INKCS023
01640 *             ---- WAIT 15 SECONDS, THEN RETRY ----               INKCS023
01641              MOVE PC-TSYMSG-00551 TO DP020-MSG-NUMBER             INKCS023
01642      END-EVALUATE.                                                INKCS023
01643                                                                   INKCS023
01644      PERFORM 4130-CLOSE-CURSOR.                                   INKCS023
01645  EJECT                                                            INKCS023
01646  4110-OPEN-CURSOR.                                                INKCS023
01647                                                                   INKCS023
01648      EXEC SQL                                                     INKCS023
01649           OPEN LINE-NBR-CSR                                       INKCS023
01650      END-EXEC.                                                    INKCS023
01651                                                                   INKCS023
01652      EVALUATE TRUE                                                INKCS023
01653          WHEN SQLCODE = ZERO                                      INKCS023
01654               CONTINUE                                            INKCS023
01655          WHEN SQLWARN0 NOT EQUAL SPACE                            INKCS023
01656          WHEN SQLCODE NOT EQUAL ZERO                              INKCS023
01657               MOVE '4110-OPEN-CURSOR'                             INKCS023
01658                            TO  DP013-PARAGRAPH                    INKCS023
01659               MOVE 'OPEN STOCKROOM WITHDRAWAL LINE-NBR CURSOR'    INKCS023
01660                            TO  DP013-MESSAGE-TEXT (1)             INKCS023
01661               MOVE SQLCA   TO  DP013-SQLCA                        INKCS023
01662               SET DP013-DB2-ABEND                                 INKCS023
01663                            TO  TRUE                               INKCS023
01664               PERFORM DP013-0000-PROCESS-ABEND                    INKCS023
01665      END-EVALUATE.                                                INKCS023
01666                                                                   INKCS023
01667                                                                   INKCS023
01668  4120-FETCH.                                                      INKCS023
01669                                                                   INKCS023
01670      EXEC SQL                                                     INKCS023
01671           FETCH LINE-NBR-CSR                                      INKCS023
01672           INTO  :STKRMW-LINE-NBR                                  INKCS023
01673      END-EXEC.                                                    INKCS023
01674                                                                   INKCS023
01675      EVALUATE TRUE                                                INKCS023
01676          WHEN SQLCODE = ZERO                                      INKCS023
01677          WHEN SQLCODE = +100                                      INKCS023
01678          WHEN SQLCODE = -904                                      INKCS023
01679          WHEN SQLCODE = -913                                      INKCS023
01680              CONTINUE                                             INKCS023
01681          WHEN SQLWARN0 NOT = SPACE                                INKCS023
01682          WHEN SQLCODE  NOT = ZERO                                 INKCS023
01683              MOVE '4120-FETCH-CURSOR'                             INKCS023
01684                           TO  DP013-PARAGRAPH                     INKCS023
01685              MOVE 'FETCH STOCKROOM WITHDRAWAL LINE-NBR CURSOR'    INKCS023
01686                           TO  DP013-MESSAGE-TEXT (1)              INKCS023
01687              MOVE SQLCA   TO  DP013-SQLCA                         INKCS023
01688              SET DP013-DB2-ABEND                                  INKCS023
01689                           TO  TRUE                                INKCS023
01690              PERFORM DP013-0000-PROCESS-ABEND                     INKCS023
01691      END-EVALUATE.                                                INKCS023
01692                                                                   INKCS023
01693                                                                   INKCS023
01694  4130-CLOSE-CURSOR.                                               INKCS023
01695                                                                   INKCS023
01696      EXEC SQL                                                     INKCS023
01697          CLOSE LINE-NBR-CSR                                       INKCS023
01698      END-EXEC.                                                    INKCS023
01699                                                                   INKCS023
01700      EVALUATE TRUE                                                INKCS023
01701          WHEN SQLCODE = ZERO                                      INKCS023
01702               CONTINUE                                            INKCS023
01703          WHEN SQLWARN0 NOT = SPACE                                INKCS023
01704          WHEN SQLCODE  NOT = ZERO                                 INKCS023
01705               MOVE '4130-CLOSE-CURSOR'                            INKCS023
01706                            TO  DP013-PARAGRAPH                    INKCS023
01707               MOVE 'CLOSE STOCKROOM WITHDRAWAL LINE-NBR CURSOR'   INKCS023
01708                            TO  DP013-MESSAGE-TEXT (1)             INKCS023
01709               MOVE SQLCA   TO  DP013-SQLCA                        INKCS023
01710               SET DP013-DB2-ABEND                                 INKCS023
01711                            TO  TRUE                               INKCS023
01712               PERFORM DP013-0000-PROCESS-ABEND                    INKCS023
01713      END-EVALUATE.                                                INKCS023
01714  EJECT                                                            INKCS023
01715 *----------------------------------------------------------------*INKCS023
01716 *    RESTORE THE DATA ON THE SCREEN FROM THE COMM AREA.          *INKCS023
01717 *----------------------------------------------------------------*INKCS023
01718                                                                   INKCS023
01719  4400-MOVE-COMMAREA-TO-SCREEN.                                    INKCS023
01720                                                                   INKCS023
W26600     MOVE ASC-KEY-STORE-NBR-X TO ASTRNBRO.                        INKCS023
01721      MOVE ASC-STORE-NAME      TO ASTNAMEO.                        INKCS023
01722      MOVE ASC-KEY-SHEET-NBR-X TO ASHEETO.                         INKCS023
01723                                                                   INKCS023
01724      IF ASC-SKU-ENTERED (PS-SUB)                                  INKCS023
W21732         MOVE ASC-SKU-NBR         (PS-SUB)  TO MR-SKU   (PS-SUB)  INKCS023
01728      ELSE                                                         INKCS023
W21732         MOVE SPACE                         TO MR-SKU   (PS-SUB)  INKCS023
01732      END-IF.                                                      INKCS023
01733                                                                   INKCS023
01734      IF ASC-UPC-NBR (PS-SUB) IS NUMERIC                           INKCS023
01735          MOVE ASC-UPC-NBR   (PS-SUB) TO MR-UPC   (PS-SUB)         INKCS023
01736      ELSE                                                         INKCS023
01737          MOVE ASC-UPC-NBR-X (PS-SUB) TO MR-UPC-X (PS-SUB)         INKCS023
01738      END-IF.                                                      INKCS023
01739                                                                   INKCS023
01740      IF ASC-INV-QTY (PS-SUB) IS NUMERIC                           INKCS023
01741          MOVE ASC-INV-QTY   (PS-SUB) TO MR-QTY   (PS-SUB)         INKCS023
01742      ELSE                                                         INKCS023
01743          MOVE ASC-INV-QTY-X (PS-SUB) TO MR-QTY-X (PS-SUB)         INKCS023
01744      END-IF.                                                      INKCS023
01745                                                                   INKCS023
01746      MOVE ASC-ITM-UNIT-PR-AMT-X-FMT (PS-SUB)                      INKCS023
01747                                             TO MR-PRCE-X (PS-SUB).INKCS023
01748  EJECT                                                            INKCS023
01749 *----------------------------------------------------------------*INKCS023
01750 * ADD A STOCKROOM WITHDRAWAL SHEET. THE SHEET CONSISTS OF        *INKCS023
01751 * MULTIPLE LINES. EACH LINE CORRESPONDS TO A ROW IN THE TABLE.   *INKCS023
01752 *----------------------------------------------------------------*INKCS023
01753                                                                   INKCS023
01754  5000-ADD-SHEET.                                                  INKCS023
01755                                                                   INKCS023
01756      INITIALIZE STKRMW-LINE-NBR.                                  INKCS023
W26600     MOVE PV-DB2-STORE-NBR   TO PV-STORE-NBR-X.                   INKCS023
W26600     MOVE PV-STORE-NBR-9     TO STKRMW-LOC-NBR.

01758      MOVE PV-DB2-SHEET-NBR   TO STKRMW-SHEET-NBR.                 INKCS023
01759      PERFORM 5025-ADD-A-LINE                                      INKCS023
01760          VARYING PS-SUB FROM 1 BY 1                               INKCS023
01761            UNTIL PS-SUB > PC-LINES-PER-PANEL OR                   INKCS023
01762                  PS-ADD-NOT-SUCCESSFUL.                           INKCS023
01763                                                                   INKCS023
01764                                                                   INKCS023
01765  5025-ADD-A-LINE.                                                 INKCS023
01766                                                                   INKCS023
W21732     IF ASC-SKU-NBR (PS-SUB) > SPACE                              INKCS023
01768          COMPUTE STKRMW-LINE-NBR                                  INKCS023
01769                = STKRMW-LINE-NBR                                  INKCS023
01770                + 1                                                INKCS023
W21732         MOVE ASC-SKU-NBR         (PS-SUB)                        INKCS023
W21732                                        TO STKRMW-SKU-NBR         INKCS023
01777                                                                   INKCS023
01778          IF ASC-UPC-NBR (PS-SUB) IS NUMERIC                       INKCS023
01779              MOVE ASC-UPC-NBR     (PS-SUB)                        INKCS023
01780                                         TO STKRMW-UPC-NBR         INKCS023
01781          ELSE                                                     INKCS023
01782              MOVE ZERO                  TO STKRMW-UPC-NBR         INKCS023
01783          END-IF                                                   INKCS023
01784                                                                   INKCS023
01785          MOVE ASC-INV-QTY         (PS-SUB)                        INKCS023
01786                                         TO STKRMW-INV-QTY         INKCS023
01787          MOVE ASC-ITM-UNIT-PR-AMT (PS-SUB)                        INKCS023
01788                                         TO STKRMW-ITM-UNIT-PR-AMT INKCS023
01789          COMPUTE STKRMW-ITM-EXTD-AMT                              INKCS023
01790                = STKRMW-INV-QTY                                   INKCS023
01791                * STKRMW-ITM-UNIT-PR-AMT                           INKCS023
01792          MOVE DP020-USERID              TO STKRMW-CHG-ID-NBR      INKCS023
01793          PERFORM 5100-INSERT-LINE                                 INKCS023
01794          IF SQLCODE NOT = 0                                       INKCS023
01795              EXEC CICS                                            INKCS023
01796                  SYNCPOINT                                        INKCS023
01797                      ROLLBACK                                     INKCS023
01798              END-EXEC                                             INKCS023
01799              SET PS-ADD-NOT-SUCCESSFUL                            INKCS023
01800                  DP020-MSG-WARNING TO TRUE                        INKCS023
01801 *            ---- WAIT 15 SECONDS, THEN RETRY ----                INKCS023
01802              MOVE PC-TSYMSG-00551  TO DP020-MSG-NUMBER            INKCS023
01803          END-IF                                                   INKCS023
01804      END-IF.                                                      INKCS023
01805                                                                   INKCS023
01806                                                                   INKCS023
01838 *----------------------------------------------------------------*INKCS023
01839 * INSERT A ROW INTO THE TSTKRMW TABLE                            *INKCS023
01840 *----------------------------------------------------------------*INKCS023
01841                                                                   INKCS023
01842  5100-INSERT-LINE.                                                INKCS023
01843                                                                   INKCS023
01844      EXEC SQL                                                     INKCS023
01845          INSERT INTO TSTKRMW                                      INKCS023
W26600              (LOC_NBR                                            INKCS023
01847             ,  SHEET_NBR                                          INKCS023
01848             ,  LINE_NBR                                           INKCS023
W21732            ,  SKU_NBR                                            INKCS023
01852             ,  UPC_NBR                                            INKCS023
01853             ,  INV_QTY                                            INKCS023
01854             ,  ITM_UNIT_PR_AMT                                    INKCS023
01855             ,  ITM_EXTD_AMT                                       INKCS023
01856             ,  CHG_TMST                                           INKCS023
01857             ,  CHG_ID_NBR)                                        INKCS023
01858          VALUES                                                   INKCS023
W26600             (:STKRMW-LOC-NBR                                     INKCS023
01860             , :STKRMW-SHEET-NBR                                   INKCS023
01861             , :STKRMW-LINE-NBR                                    INKCS023
W21732            , :STKRMW-SKU-NBR                                     INKCS023
01865             , :STKRMW-UPC-NBR                                     INKCS023
01866             , :STKRMW-INV-QTY                                     INKCS023
01867             , :STKRMW-ITM-UNIT-PR-AMT                             INKCS023
01868             , :STKRMW-ITM-EXTD-AMT                                INKCS023
01869             , CURRENT TIMESTAMP                                   INKCS023
01870             , :STKRMW-CHG-ID-NBR)                                 INKCS023
01871      END-EXEC.                                                    INKCS023
01873      EVALUATE TRUE                                                INKCS023
01874          WHEN SQLCODE = ZERO                                      INKCS023
01875          WHEN SQLCODE = -904                                      INKCS023
01876          WHEN SQLCODE = -913                                      INKCS023
01877              CONTINUE                                             INKCS023
01878          WHEN SQLWARN0 NOT = SPACES                               INKCS023
01879          WHEN SQLCODE  NOT = ZERO                                 INKCS023
01880              MOVE '5100-INSERT-LINE'                              INKCS023
01881                                TO DP013-PARAGRAPH                 INKCS023
01882              MOVE 'INSERT A ROW INTO THE STOCKROOM WITHDRAWAL     INKCS023
01883 -                 'TABLE'      TO DP013-MESSAGE-TEXT(1)           INKCS023
01884              MOVE SQLCA        TO DP013-SQLCA                     INKCS023
01885              MOVE 'TSTKRMW'    TO DP013-DB2-TABLE-NAME (1)        INKCS023
01886              SET DP013-DB2-ABEND                                  INKCS023
01887                  DP013-XCTL-DISPLAY-RESTART TO TRUE               INKCS023
01888              PERFORM DP013-0000-PROCESS-ABEND                     INKCS023
01889      END-EVALUATE.                                                INKCS023
01890  EJECT                                                            INKCS023
22000
22000 *----------------------------------------------------------------*
22000 *    SET STOCKROOM WITHDRAWAL INDICATOR TO 'Y'                   *
22000 *----------------------------------------------------------------*
22000  6000-SET-STOCKROOM-IND.
22000      IF INVPAR-STKRM-WDRWL-IND = 'N'
W26600
W26600         MOVE ASC-KEY-STORE-NBR TO INVPAR-LOC-NBR
W26600
22000          EXEC SQL
22000              UPDATE TINVPAR
22000                  SET STKRM_WDRWL_IND = 'Y'
W26600             WHERE LOC_NBR = :INVPAR-LOC-NBR
22000                  AND  UNT_BKG_STAT_CDE = 'IN'
22000                  AND  ACTL_FIN_BK_DTE = '9999-09-09'
22000                  AND  LOC_INV_STAT_CDE = 'IN'
22000          END-EXEC
22000      END-IF.
22000
W28545*----------------------------------------------------------------*
W28545*    PRICE LOOK-UP PROCEDURE DIVISION COPYBOOK
W28545*----------------------------------------------------------------*
W28545
W28545     COPY INPD400.
W28545
01891 *----------------------------------------------------------------*INKCS023
01892 *    CURRENT SYSTEM DATE/TIME MODULE                              INKCS023
01893 *----------------------------------------------------------------*INKCS023
01894                                                                   INKCS023
01895      COPY DPPD017.                                                INKCS023
01896                                                                   INKCS023
01897 *----------------------------------------------------------------*INKCS023
01898 *    ABEND PROCESSOR MODULE                                      *INKCS023
01899 *----------------------------------------------------------------*INKCS023
01900                                                                   INKCS023
01901      COPY DPPD013.                                                INKCS023
