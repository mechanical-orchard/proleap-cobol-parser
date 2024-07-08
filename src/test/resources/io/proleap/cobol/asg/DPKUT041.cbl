000100******************************************************************04/27/90
000200 IDENTIFICATION DIVISION.                                         DPKUT041
000300******************************************************************   LV001
000400                                                                  DPKUT041
000500 PROGRAM-ID.                DPKUT041.                             DPKUT041
000600 AUTHOR.                    DONALD TOMLINSON.                     DPKUT041
000700 INSTALLATION.              KOHLS DEPARTMENT STORES.              DPKUT041
000800 DATE-WRITTEN.              4-09-2002.                            DPKUT041
000900 DATE-COMPILED.                                                   DPKUT041
001000                                                                  DPKUT041
001100******************************************************************DPKUT041
001110*                   SPECIAL NOTE                                 *DPKUT041
001200*  THIS SUBROUTINE IS A COPY OF THE ORIGINAL SUBROUTINE DPKUT040.*DPKUT041
001201*  THE PURPOSE OF THIS VERSION IS TO PREVENT PROGRAMS FROM EXE-  *DPKUT041
001202*  CUTING THIS SUBROUTINE TO GET AN INTERNAL UPC NUMBER USING A  *DPKUT041
001203*  VALID SKU NUMBER THAT IS NOT A DUMMY SKU.  IF A UPC NUMBER IS *DPKUT041
001204*  NEEDED FOR A NON DUMMY SKU, THEN THE PROGRAMMER SHOULD WRITE  *DPKUT041
001205*  SQL THAT WILL OBTAIN IT FROM THE TUPC DB2 TABLE WHERE IT IS   *DPKUT041
001206*  ALREADY PRESENT, THUS NOT NECESSARY TO RE-CREATE IT HERE.  A  *DPKUT041
001207*  CHECK HAS BEEN ADDED HERE TO ENSURE THAT THE SKU PASSED IS A  *DPKUT041
001208*  DUMMY SKU (ENDS IN '000') IF UPC NUMBER GENERATION IS BEING   *DPKUT041
001209*  REQUESTED.                                                    *DPKUT041
001210*                   SPECIAL NOTE END                             *DPKUT041
001211*                                                                *DPKUT041
001220*       THIS SUBROUTINE WILL COMPUTE UPC CHECK DIGITS BASED ON   *DPKUT041
001300*  A PASSED PARAMETER LIST.  TWO OPTIONS ARE AVAILABLE:          *DPKUT041
001400*  CHECK DIGIT COMPUTATION/VERIFICATION, AND INTERNAL UPC CODE   *DPKUT041
001500*  GENERATION.  TO USE THE CHECK DIGIT COMPUTATION/VERIFICATION  *DPKUT041
001600*  OPTION, SET THE OPTION INDICATOR, AND PROVIDE A FULL UPC      *DPKUT041
001700*  (MARK THE CHECK DIGIT POSITION WITH A ZERO, IF YOU ARE JUST   *DPKUT041
001800*  COMPUTING A CHECK DIGIT).  AN ERROR INDICATOR, AND A COMPUTED *DPKUT041
001900*  CHECK DIGIT WILL BE PASSED BACK TO THE CALLING PROGRAM.  TO   *DPKUT041
002000*  USE THE INTERNAL UPC CODE GENERATION OPTION, SET THE OPTION   *DPKUT041
002100*  INDICATOR, AND PROVIDE THE SKU NUMBER ON WHICH THE INTERNAL   *DPKUT041
002200*  UPC CODE IS TO BE GENERATED.  AN ERROR INDICATOR, THE FULL    *DPKUT041
002300*  INTERAL UPC CODE, AND THE COMPUTED CHECK DIGIT WILL BE PASSED *DPKUT041
002400*  BACK TO THE CALLING PROGRAM.  USE COPY MEMBER DPWS040I FOR    *DPKUT041
002500*  THE PARAMETER LIST.                                           *DPKUT041
002600*                                                                *DPKUT041
002700*       THE SUBROUTINE BEGINS BY VALIDATING THE PARAMETERS       *DPKUT041
002800*  PASSED IN THE PARAMETER LIST (OPTION, UPC, SKU).  IF THE      *DPKUT041
002900*  GENERATE INTERNAL UPC OPTION WAS SELECTED, BUILD THE INTERNAL *DPKUT041
003000*  UPC CODE, USING THE SKU NUMBER, WITH ZERO AS THE CHECK DIGIT. *DPKUT041
003100*  COMPUTE THE CHECK DIGIT FOR THE UPC THAT WAS PASSED OR BUILT. *DPKUT041
003200*  TO COMPUTE THE CHECK DIGIT, SUM ONE OR THREE TIMES EACH UPC   *DPKUT041
003300*  DIGIT, IN AN ALTERNATING SEQUENCE, FOR ALL NON-CHECK DIGIT    *DPKUT041
003400*  NUMBERS.  SINCE WE ARE ASSUMING A 15 DIGIT UPC, THE           *DPKUT041
003500*  MULTIPLICATION SEQUENCE BEGINS WITH ONE TIMES THE DIGIT, AS   *DPKUT041
003600*  OPPOSED TO THREE TIMES THE DIGIT, FOR A STANDARD 12 DIGIT     *DPKUT041
003700*  UPC.  THE CHECK DIGIT IS THEN TEN MINUS THE LAST DIGIT OF     *DPKUT041
003800*  THAT SUM.  FINALLY, IF THE GENERATE INTERNAL UPC OPTION WAS   *DPKUT041
003900*  SELECTED, MOVE THE COMPUTED CHECK DIGIT TO THE CHECK DIGIT    *DPKUT041
004000*  POSITION OF THE UPC, TO COMPLETE THE GENERATION OF THE UPC.   *DPKUT041
004100*                                                                *DPKUT041
004200*       **  NOTE  **                                             *DPKUT041
004300*                                                                *DPKUT041
004400*       WHENEVER THIS ROUTINE IS MOVED TO PRODUCTION, THE LOAD   *DPKUT041
004500*   MODULE MUST BE COPIED INTO THE CICS LOADLIB, TO MAKE IT      *DPKUT041
004600*   AVAILABLE TO CICS, AS WELL AS BATCH.                         *DPKUT041
004700******************************************************************DPKUT041
004710*  06/04/03 - DONALD TOMLINSON                                          11
004720*             (WR25031)   NI-SKU CONVERSION.  ADD A 3RD OPTION          11
004730*    TO CONVERT A DUMMY INTERNAL UPC CODE TO A DUMMY SKU. ADDED         11
004740*    A NEW FUNCTION CODE AND A NEW FAILURE CODE TO COPY MEMBER          11
004750*    DPWS040I.                                                          11
004900******************************************************************DPKUT041
005000 ENVIRONMENT DIVISION.                                            DPKUT041
005100******************************************************************DPKUT041
005200                                                                  DPKUT041
005300 CONFIGURATION SECTION.                                           DPKUT041
005400                                                                  DPKUT041
005500 SOURCE-COMPUTER.        IBM-3090.                                DPKUT041
005600 OBJECT-COMPUTER.        IBM-3090.                                DPKUT041
005700                                                                  DPKUT041
005800 INPUT-OUTPUT SECTION.                                            DPKUT041
005900                                                                  DPKUT041
006000 FILE-CONTROL.                                                    DPKUT041
006100                                                                  DPKUT041
006200******************************************************************DPKUT041
006300 DATA DIVISION.                                                   DPKUT041
006400******************************************************************DPKUT041
006500                                                                  DPKUT041
006600 WORKING-STORAGE SECTION.                                         DPKUT041
006700                                                                  DPKUT041
006800*                                                                 DPKUT041
006900*  UPC CHECK DIGIT ROUTINE PARAMETER LIST LAYOUT.                 DPKUT041
007000*                                                                 DPKUT041
007100                                                                  DPKUT041
007200     COPY DPWS040I.                                               DPKUT041
007300                                                                  DPKUT041
007400 01  PC-PROGRAM-CONSTANTS.                                        DPKUT041
007500     05  PC-UPC-CHECK-DIGIT-BASE PIC S9(2)    VALUE +10.          DPKUT041
007600     05  PC-CHECK-DIGIT-LOCATION PIC S9(4)    VALUE +15           DPKUT041
007700                                 COMP SYNC.                       DPKUT041
007710     05  PC-ZERO                 PIC 9(03)    VALUE ZERO.         DPKUT041
007720     05  PC-000400               PIC X(06)    VALUE '000400'.     DPKUT041
007800                                                                  DPKUT041
007900 01  PV-PROGRAM-VARIABLES.                                        DPKUT041
008000     05  PV-CHECK-DIGIT-ACCUMULATOR                               DPKUT041
008100                                 PIC S9(3)    VALUE ZERO.         DPKUT041
008200     05  FILLER                  REDEFINES                        DPKUT041
008300                                     PV-CHECK-DIGIT-ACCUMULATOR.  DPKUT041
008400         10  FILLER              PIC X(2).                        DPKUT041
008500         10  PV-UPC-CHECK-DIGIT-MODIFIER                          DPKUT041
008600                                 PIC S9(1).                       DPKUT041
008700     05  PV-CHECK-DIGIT-MULTIPLIER                                DPKUT041
008800                                 PIC S9(1)    VALUE ZERO.         DPKUT041
008900         88  PV-MULTIPLY-BY-1                 VALUE +1.           DPKUT041
009000         88  PV-MULTIPLY-BY-3                 VALUE +3.           DPKUT041
009100                                                                  DPKUT041
009200 01  IU-INTERNAL-UPC-CODE-AREA.                                   DPKUT041
009300     05  IU-INTERNAL-UPC-CODE-X.                                  DPKUT041
009400         10  IU-000400           PIC X(6)     VALUE '000400'.     DPKUT041
009500         10  IU-SKU-NUMBER       PIC 9(8)     VALUE ZERO.         DPKUT041
009510         10  FILLER REDEFINES IU-SKU-NUMBER.
009520             15  IU-SKU-1-5      PIC 9(05).
009540             15  IU-SKU-000      PIC 9(03).
009610         10  FILLER              PIC 9(1)     VALUE ZERO.         DPKUT041
009700     05  IU-INTERNAL-UPC-CODE    REDEFINES IU-INTERNAL-UPC-CODE-X DPKUT041
009800                                 PIC 9(15).                       DPKUT041
009900                                                                  DPKUT041
010000 LINKAGE SECTION.                                                 DPKUT041
010100                                                                  DPKUT041
010200 01  LS-UPC-CHECK-DIGIT-PARMS    PIC X(26).                       DPKUT041
010300                                                                  DPKUT041
010400******************************************************************DPKUT041
010500 PROCEDURE DIVISION USING LS-UPC-CHECK-DIGIT-PARMS.               DPKUT041
010600******************************************************************DPKUT041
010700                                                                  DPKUT041
010800 0000-MAINLINE.                                                   DPKUT041
010900     MOVE LS-UPC-CHECK-DIGIT-PARMS TO                             DPKUT041
011000               DP040I-UPC-CHECK-DIGIT-PARMS.                      DPKUT041
011100                                                                  DPKUT041
011200*                                                                 DPKUT041
011300*  EDIT THE PARAMETERS THAT WERE PASSED                           DPKUT041
011400*                                                                 DPKUT041
011500                                                                  DPKUT041
011600     EVALUATE TRUE  ALSO  TRUE                                    DPKUT041
011700         WHEN (NOT DP040I-VALID-CHK-DIGIT-OPTION)  ALSO  ANY      DPKUT041
011800             SET DP040I-INV-CHECK-DIGIT-OPTION TO TRUE            DPKUT041
011900         WHEN (DP040I-COMP-CHECK-DIGIT-OPTION)     ALSO           DPKUT041
012000                   (DP040I-UPC-CODE NOT NUMERIC)                  DPKUT041
012100         WHEN (DP040I-GEN-INTERNAL-UPC-OPTION)     ALSO           DPKUT041
012200                   (DP040I-SKU-NUMBER NOT NUMERIC)                DPKUT041
012300             SET DP040I-UPC-SKU-NOT-NUMERIC  TO TRUE              DPKUT041
012400*        WHEN (DP040I-COMP-CHECK-DIGIT-OPTION)     ALSO           DPKUT041
012500*                  (DP040I-UPC-CODE = ZERO)                       DPKUT041
012600         WHEN (DP040I-GEN-INTERNAL-UPC-OPTION)     ALSO           DPKUT041
012700                   (DP040I-SKU-NUMBER = ZERO)                     DPKUT041
012800             SET DP040I-UPC-SKU-NOT-PROVIDED TO TRUE              DPKUT041
012810         WHEN (DP040I-GEN-INTERNAL-UPC-OPTION)     ALSO           DPKUT041
012820                   (DP040I-SEQUENCE-NUMBER NOT = ZERO)            DPKUT041
012830             SET DP040I-UPC-SKU-NOT-DUMMY    TO TRUE              DPKUT041
012900         WHEN OTHER                                               DPKUT041
013000             SET DP040I-NO-ERROR-DETECTED    TO TRUE              DPKUT041
013100     END-EVALUATE.                                                DPKUT041
013200                                                                  DPKUT041
013201                                                                  DPKUT041
013210     IF DP040I-EXTRACT-SKU-FROM-UPC
013220         PERFORM 0300-OPTION-3
013230     ELSE
013231         PERFORM 0200-OPTION-1-OR-2.
013232
013233
013234     PERFORM 1000-EXIT-PROGRAM.
013235
013236
013240 0200-OPTION-1-OR-2.
013300*                                                                 DPKUT041
013400*  GENERATE THE INTERNAL UPC CODE, IF THAT WAS THE REQUESTED      DPKUT041
013500*  OPTION.  NO CHECK DIGIT IS ASSIGNED AT THIS POINT.             DPKUT041
013600*                                                                 DPKUT041
013700                                                                  DPKUT041
013800     IF ((DP040I-NO-ERROR-DETECTED)                               DPKUT041
013900     AND (DP040I-GEN-INTERNAL-UPC-OPTION))                              41
014010         MOVE DP040I-SKU-NUMBER TO IU-SKU-NUMBER                  DPKUT041
014100         MOVE IU-INTERNAL-UPC-CODE TO DP040I-UPC-CODE             DPKUT041
014200     END-IF.                                                      DPKUT041
014300                                                                  DPKUT041
014400                                                                  DPKUT041
014500     INITIALIZE PV-PROGRAM-VARIABLES                              DPKUT041
014600                DP040I-COMPUTED-UPC-CHK-DIGIT.                    DPKUT041
014700                                                                  DPKUT041
014800*                                                                 DPKUT041
014900*  COMPUTE THE CHECK DIGIT.  SINCE WE ARE ASSUMING A 15 DIGIT UPC,DPKUT041
015000*  THE MULTIPLICATION SEQUENCE BEGINS WITH ONE TIMES THE DIGIT, ASDPKUT041
015100*  OPPOSED TO THREE TIMES THE DIGIT FOR A STANDARD 12 DIGIT UPC.  DPKUT041
015200*                                                                 DPKUT041
015300                                                                  DPKUT041
015400     IF DP040I-NO-ERROR-DETECTED                                  DPKUT041
015500         SET PV-MULTIPLY-BY-1 TO TRUE                             DPKUT041
015600         PERFORM                                                  DPKUT041
015700                 VARYING DP040I-UPC-INDEX                         DPKUT041
015800                 FROM 1 BY 1                                      DPKUT041
015900                 UNTIL DP040I-UPC-INDEX = PC-CHECK-DIGIT-LOCATION DPKUT041
016000             COMPUTE PV-CHECK-DIGIT-ACCUMULATOR =                 DPKUT041
016100                       (PV-CHECK-DIGIT-ACCUMULATOR +              DPKUT041
016200                       (DP040I-UPC-DIGIT (DP040I-UPC-INDEX) *     DPKUT041
016300                       PV-CHECK-DIGIT-MULTIPLIER))                DPKUT041
016400             IF PV-MULTIPLY-BY-1                                  DPKUT041
016500                 SET PV-MULTIPLY-BY-3 TO TRUE                     DPKUT041
016600             ELSE                                                 DPKUT041
016700                 SET PV-MULTIPLY-BY-1 TO TRUE                     DPKUT041
016800             END-IF                                               DPKUT041
016900         END-PERFORM                                              DPKUT041
017000         COMPUTE DP040I-COMPUTED-UPC-CHK-DIGIT =                  DPKUT041
017100                   (PC-UPC-CHECK-DIGIT-BASE -                     DPKUT041
017200                   PV-UPC-CHECK-DIGIT-MODIFIER)                   DPKUT041
017300     END-IF.                                                      DPKUT041
017400                                                                  DPKUT041
017500*                                                                 DPKUT041
017600*  ASSIGN THE CHECK DIGIT TO THE UPC CODE FOR REQUESTS TO         DPKUT041
017700*  GENERATE THE INTERNAL UPC CODE.                                DPKUT041
017800*                                                                 DPKUT041
017900                                                                  DPKUT041
018000     IF ((DP040I-NO-ERROR-DETECTED)                               DPKUT041
018100             AND (DP040I-GEN-INTERNAL-UPC-OPTION))                DPKUT041
018200         MOVE DP040I-COMPUTED-UPC-CHK-DIGIT TO                    DPKUT041
018300                   DP040I-UPC-CHECK-DIGIT                         DPKUT041
018400     END-IF.                                                      DPKUT041
018500                                                                  DPKUT041
018600*                                                                 DPKUT041
018700*  MOVE THE PARAMETERS LIST, WITH RESPONSES INCLUDED, TO THE      DPKUT041
018800*  LINKAGE SECTION, FOR PASSING BACK TO THE CALLING PROGRAM.      DPKUT041
018900*                                                                 DPKUT041
019000                                                                  DPKUT041
019100     MOVE DP040I-UPC-CHECK-DIGIT-PARMS TO                         DPKUT041
019200               LS-UPC-CHECK-DIGIT-PARMS.                          DPKUT041
019300                                                                  DPKUT041
019301                                                                  DPKUT041
019310 0300-OPTION-3.
019311     INITIALIZE PV-PROGRAM-VARIABLES                              DPKUT041
019312
019313     MOVE DP040I-UPC-CODE      TO IU-INTERNAL-UPC-CODE-AREA.
019314
019315     IF IU-SKU-1-5 > PC-ZERO
019316     AND IU-SKU-000 = PC-ZERO
019317     AND IU-000400 = PC-000400
019318         MOVE IU-SKU-NUMBER    TO DP040I-SKU-NUMBER
019319     ELSE
019320         SET DP040I-UPC-NOT-DUMMY-UPC TO TRUE.                    DPKUT041
019330                                                                  DPKUT041
019331     MOVE DP040I-UPC-CHECK-DIGIT-PARMS TO                         DPKUT041
019332               LS-UPC-CHECK-DIGIT-PARMS.                          DPKUT041
019333                                                                  DPKUT041
019340                                                                  DPKUT041
019400 1000-EXIT-PROGRAM.                                               DPKUT041
019500     EXIT PROGRAM.                                                DPKUT041
