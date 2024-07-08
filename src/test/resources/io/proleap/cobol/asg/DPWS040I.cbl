000100*                                                                 00010003
000200*  THIS IS THE WORKING STORAGE COPY MEMBER THAT SERVES AS THE     00020003
000300*  PARAMETER LIST FOR CALLING THE UPC CHECK DIGIT ROUTINE         00030003
000400*  DPKUT040.                                                      00040003
000500*                                                                 00050003
000600*  TWO OPTIONS ARE AVAILABLE:  CHECK DIGIT COMPUTATION/           00060003
000700*  VERIFICATION (PASS A FULL UPC (INCLUDE A ZERO CHECK DIGIT      00070003
000800*  FOR COMPUTATIONS), AND THE CORRECT CHECK DIGIT IS COMPUTED),   00080003
000900*  AND INTERNAL UPC GENERATION (PASS A SKU, AND AN INTERNAL UPC   00090003
001000*  CODE IS GENERATED, WITH A CHECK DIGIT).  THE OPTION, AND THE   00100003
001100*  APPROPRIATE UPC/SKU, MUST BE PROVIDED.                         00110003
001200*                                                                 00120003
001300*  TO INVOKE THE ROUTINE:                                         00130003
001400*                                                                 00140003
001500*       CALL DP040I-UPC-CHECK-DIGIT-ROUTINE USING                 00150003
001510*                 DP040I-UPC-CHECK-DIGIT-PARMS.                   00151003
001600*                                                                 00160003
001700 01  DP040I-UPC-CHECK-DIGIT-ROUTINE                               00170003
001800                                 PIC X(8)     VALUE 'DPKUT040'.   00180003
001810 01  DP040I-UPC-CHECK-DIGIT-WO-IUPC                               00181004
001820                                 PIC X(8)     VALUE 'DPKUT041'.   00182004
001900                                                                  00190003
002000 01  DP040I-UPC-CHECK-DIGIT-PARMS.                                00200003
002100     05  DP040I-UPC-CHECK-DIGIT-OPTION                            00210003
002200                                 PIC X(1)         VALUE SPACES.   00220003
002300         88  DP040I-VALID-CHK-DIGIT-OPTION                        00230003
002400             VALUE '1', '2', '3'.                                 00240003
002500         88  DP040I-COMP-CHECK-DIGIT-OPTION       VALUE '1'.      00250003
002600         88  DP040I-GEN-INTERNAL-UPC-OPTION       VALUE '2'.      00260003
002700         88  DP040I-EXTRACT-SKU-FROM-UPC          VALUE '3'.      00270003
002800     05  DP040I-UPC-CODE         PIC 9(15)        VALUE ZERO.     00280003
002900     05  FILLER                  REDEFINES DP040I-UPC-CODE.       00290003
003000         10  DP040I-UPC-DIGIT    OCCURS 14 TIMES                  00300003
003100                                 INDEXED BY DP040I-UPC-INDEX      00310003
003200                                 PIC 9(1).                        00320003
003300         10  DP040I-UPC-CHECK-DIGIT                               00330003
003400                                 PIC 9(1).                        00340003
003500     05  DP040I-SKU-NUMBER       PIC 9(8)         VALUE ZERO.     00350003
003600     05  DP040I-SKU-NUMBER-X     REDEFINES DP040I-SKU-NUMBER.     00360003
003700         10  DP040I-DEPARTMENT-NUMBER                             00370003
003800                                 PIC 9(3).                        00380003
003900         10  DP040I-CLASS-NUMBER PIC 9(2).                        00390003
004000         10  DP040I-SEQUENCE-NUMBER                               00400003
004100                                 PIC 9(3).                        00410003
004200     05  DP040I-ERROR-INDICATOR  PIC X(1)         VALUE SPACES.   00420003
004300         88  DP040I-NO-ERROR-DETECTED             VALUE SPACES.   00430003
004400         88  DP040I-INV-CHECK-DIGIT-OPTION        VALUE '1'.      00440003
004500         88  DP040I-UPC-SKU-NOT-NUMERIC           VALUE '2'.      00450003
004600         88  DP040I-UPC-SKU-NOT-PROVIDED          VALUE '3'.      00460003
004700         88  DP040I-UPC-SKU-NOT-DUMMY             VALUE '4'.      00470003
004800         88  DP040I-UPC-NOT-DUMMY-UPC             VALUE '5'.      00480003
004900     05  DP040I-COMPUTED-UPC-CHK-DIGIT                            00490003
005000                                 PIC 9(1)         VALUE ZERO.     00500003