******************************************************************
      * PROGRAM NAME: CUSTOMER-BILLING
      * AUTHOR:       AI Assistant (Gemini)
      * DATE-WRITTEN: 2025-05-05
      * PURPOSE:      Demonstrate a complex customer billing process.
      * Reads Customer Master and Transaction files,
      * updates master based on transactions, calculates
      * finance charges and minimum payment, and
      * produces a billing report and updated master file.
      * FILES:
      * INPUT:  CUSTOMER-MASTER-IN (Sequential)
      * TRANSACTION-FILE   (Sequential - assumed sorted by Cust ID)
      * OUTPUT: CUSTOMER-MASTER-OUT (Sequential)
      * BILLING-REPORT     (Printer/Sequential)
      *
      * COMPLEXITY ELEMENTS:
      * - Multiple file I/O (Sequential)
      * - Control break logic (processing transactions per customer)
      * - Calculations (New Balance, Finance Charge, Minimum Payment)
      * - Report Generation with Headers and Detail Lines
      * - Basic File Status Checking
      * - Packed Decimal (COMP-3) usage for calculations
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-BILLING.
       AUTHOR. AI Assistant (Gemini).
       DATE-WRITTEN. 2025-05-05.
       INSTALLATION. Bengaluru, Karnataka, India.
       SECURITY. Non-Confidential.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME. *> Or your target environment
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-MASTER-IN ASSIGN TO 'CUSTMAST.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CUST-IN-STATUS.

           SELECT TRANSACTION-FILE ASSIGN TO 'CUSTTRAN.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.

           SELECT CUSTOMER-MASTER-OUT ASSIGN TO 'CUSTMSTN.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CUST-OUT-STATUS.

           SELECT BILLING-REPORT ASSIGN TO 'CUSTREPT.LST'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-MASTER-IN
           RECORD CONTAINS 150 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 CUSTOMER-MASTER-IN-REC.
           05 CM-CUST-ID-IN          PIC 9(08).
           05 CM-CUST-NAME-IN        PIC X(30).
           05 CM-CUST-ADDR1-IN       PIC X(30).
           05 CM-CUST-ADDR2-IN       PIC X(30).
           05 CM-CUST-CITY-IN        PIC X(20).
           05 CM-CUST-STATE-IN       PIC X(02).
           05 CM-CUST-ZIP-IN         PIC X(10).
           05 CM-CUST-BALANCE-IN     PIC S9(9)V99 COMP-3.
           05 FILLER                 PIC X(10). *> Reserved space

       FD TRANSACTION-FILE
           RECORD CONTAINS 30 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 TRANSACTION-REC.
           05 TR-CUST-ID             PIC 9(08).
           05 TR-DATE                PIC 9(08). *> YYYYMMDD
           05 TR-TYPE                PIC X(01). *> 'P'=Purchase, 'Y'=Payment
              88 TR-TYPE-PURCHASE    VALUE 'P'.
              88 TR-TYPE-PAYMENT     VALUE 'Y'.
           05 TR-AMOUNT              PIC S9(7)V99 COMP-3.
           05 FILLER                 PIC X(01).

       FD CUSTOMER-MASTER-OUT
           RECORD CONTAINS 150 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 CUSTOMER-MASTER-OUT-REC    PIC X(150).

       FD BILLING-REPORT
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 REPORT-RECORD              PIC X(132).


       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS-CODES.
           05 WS-CUST-IN-STATUS      PIC XX VALUE '00'.
              88 WS-CUST-IN-EOF      VALUE '10'.
           05 WS-TRANS-STATUS        PIC XX VALUE '00'.
              88 WS-TRANS-EOF        VALUE '10'.
           05 WS-CUST-OUT-STATUS     PIC XX VALUE '00'.
           05 WS-REPORT-STATUS       PIC XX VALUE '00'.

       01 WS-FLAGS.
           05 WS-MORE-RECORDS-FLAG   PIC X VALUE 'Y'.
              88 WS-NO-MORE-RECORDS  VALUE 'N'.
           05 WS-PROCESS-TYPE        PIC X.
              88 PROCESS-MATCH       VALUE 'M'.
              88 PROCESS-CUST-ONLY   VALUE 'C'.
              88 PROCESS-TRANS-ONLY  VALUE 'T'.

       01 WS-CURRENT-PROCESSING-VARS.
           05 WS-CURRENT-CUST-ID     PIC 9(08) VALUE ZEROES.
           05 WS-PREVIOUS-CUST-ID    PIC 9(08) VALUE ZEROES.
           05 WS-CURRENT-BALANCE     PIC S9(9)V99 COMP-3 VALUE ZEROES.
           05 WS-FINANCE-CHARGE      PIC S9(7)V99 COMP-3 VALUE ZEROES.
           05 WS-MINIMUM-PAYMENT     PIC S9(7)V99 COMP-3 VALUE ZEROES.
           05 WS-TOTAL-PURCHASES     PIC S9(9)V99 COMP-3 VALUE ZEROES.
           05 WS-TOTAL-PAYMENTS      PIC S9(9)V99 COMP-3 VALUE ZEROES.

       01 WS-CONSTANTS.
           05 WS-FINANCE-RATE        PIC 9V999 VALUE 0.015. *> 1.5%
           05 WS-MIN-PAY-RATE        PIC 9V99  VALUE 0.10.  *> 10%
           05 WS-MIN-PAY-FLAT        PIC S9(3)V99 COMP-3 VALUE +25.00.

       01 WS-COUNTERS.
           05 WS-CUST-READ-COUNT     PIC 9(7) VALUE ZEROES.
           05 WS-TRANS-READ-COUNT    PIC 9(7) VALUE ZEROES.
           05 WS-CUST-WRITTEN-COUNT  PIC 9(7) VALUE ZEROES.
           05 WS-REPORTS-WRITTEN-COUNT PIC 9(7) VALUE ZEROES.

       01 WS-CURRENT-DATETIME.
           05 WS-CURRENT-DATE-YYYYMMDD PIC 9(8).
           05 WS-CURRENT-DATE-FORMATTED.
               10 WS-CD-YEAR         PIC 9(4).
               10 FILLER             PIC X VALUE '/'.
               10 WS-CD-MONTH        PIC 9(2).
               10 FILLER             PIC X VALUE '/'.
               10 WS-CD-DAY          PIC 9(2).
           05 WS-CURRENT-TIME        PIC 9(8).

       01 WS-CUSTOMER-MASTER-HOLD REDEFINES CUSTOMER-MASTER-IN-REC.
           05 WS-HOLD-CUST-ID          PIC 9(08).
           05 WS-HOLD-CUST-DATA        PIC X(142).

       01 WS-REPORT-HEADER-1.
           05 FILLER                 PIC X(50) VALUE SPACES.
           05 FILLER                 PIC X(32) VALUE 'CUSTOMER BILLING STATEMENT'.
           05 FILLER                 PIC X(50) VALUE SPACES.

       01 WS-REPORT-HEADER-2.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 FILLER                 PIC X(10) VALUE 'REPORT DATE:'.
           05 WS-HDR2-DATE           PIC X(10).
           05 FILLER                 PIC X(111) VALUE SPACES.

       01 WS-REPORT-HEADER-3.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 FILLER                 PIC X(10) VALUE 'CUSTOMER'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(30) VALUE 'NAME'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(15) VALUE 'PREV BALANCE'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(15) VALUE 'FIN CHARGE'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(15) VALUE 'NEW BALANCE'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(15) VALUE 'MIN PAYMENT'.
           05 FILLER                 PIC X(11) VALUE SPACES.

       01 WS-REPORT-DETAIL-LINE.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 WS-DET-CUST-ID         PIC 9(08).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 WS-DET-CUST-NAME       PIC X(30).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 WS-DET-PREV-BAL        PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 WS-DET-FIN-CHG         PIC ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 WS-DET-NEW-BAL         PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 WS-DET-MIN-PAY         PIC ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(11) VALUE SPACES.

       01 WS-ABEND-MESSAGE.
           05 FILLER                 PIC X(15) VALUE 'ABEND OCCURRED: '.
           05 WS-ABEND-MSG-TEXT      PIC X(50).
           05 WS-ABEND-FILE          PIC X(20).
           05 WS-ABEND-STATUS        PIC X(2).

       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-RECORDS UNTIL WS-NO-MORE-RECORDS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           DISPLAY "CUSTOMER-BILLING PROGRAM STARTING...".
           ACCEPT WS-CURRENT-DATE-YYYYMMDD FROM DATE YYYYMMDD.
           MOVE WS-CURRENT-DATE-YYYYMMDD TO WS-CURRENT-DATE-FORMATTED.

           OPEN INPUT CUSTOMER-MASTER-IN TRANSACTION-FILE
           OPEN OUTPUT CUSTOMER-MASTER-OUT BILLING-REPORT

           PERFORM 8100-CHECK-FILE-STATUS VARYING WS-FILE-STATUS-CODES
             FROM WS-CUST-IN-STATUS BY 1
             UNTIL WS-FILE-STATUS-CODES = WS-REPORT-STATUS + 1 *> Hacky way

           IF WS-CUST-IN-STATUS NOT = '00'
              MOVE 'CUSTOMER-MASTER-IN' TO WS-ABEND-FILE
              MOVE WS-CUST-IN-STATUS TO WS-ABEND-STATUS
              PERFORM 8000-ABEND-RTN
           END-IF
           IF WS-TRANS-STATUS NOT = '00'
              MOVE 'TRANSACTION-FILE' TO WS-ABEND-FILE
              MOVE WS-TRANS-STATUS TO WS-ABEND-STATUS
              PERFORM 8000-ABEND-RTN
           END-IF
           IF WS-CUST-OUT-STATUS NOT = '00'
              MOVE 'CUSTOMER-MASTER-OUT' TO WS-ABEND-FILE
              MOVE WS-CUST-OUT-STATUS TO WS-ABEND-STATUS
              PERFORM 8000-ABEND-RTN
           END-IF
            IF WS-REPORT-STATUS NOT = '00'
              MOVE 'BILLING-REPORT' TO WS-ABEND-FILE
              MOVE WS-REPORT-STATUS TO WS-ABEND-STATUS
              PERFORM 8000-ABEND-RTN
           END-IF

           PERFORM 4100-WRITE-REPORT-HEADERS

           *> Priming Reads
           PERFORM 3100-READ-CUSTOMER-MASTER
           PERFORM 3200-READ-TRANSACTION-FILE
           .

       2000-PROCESS-RECORDS.
           *> Determine which record key is lower or if they match
           EVALUATE TRUE
               WHEN WS-CUST-IN-EOF AND WS-TRANS-EOF
                   MOVE 'N' TO WS-MORE-RECORDS-FLAG
               WHEN WS-TRANS-EOF
                   MOVE 'C' TO WS-PROCESS-TYPE
               WHEN WS-CUST-IN-EOF
                   MOVE 'T' TO WS-PROCESS-TYPE
               WHEN CM-CUST-ID-IN < TR-CUST-ID
                   MOVE 'C' TO WS-PROCESS-TYPE
               WHEN CM-CUST-ID-IN > TR-CUST-ID
                   MOVE 'T' TO WS-PROCESS-TYPE
               WHEN CM-CUST-ID-IN = TR-CUST-ID
                   MOVE 'M' TO WS-PROCESS-TYPE
           END-EVALUATE

           *> Perform processing based on the determined type
           EVALUATE WS-PROCESS-TYPE
               WHEN 'C' *> Process unmatched Customer Master
                   PERFORM 2100-PROCESS-CUSTOMER-ONLY
                   PERFORM 3100-READ-CUSTOMER-MASTER
               WHEN 'T' *> Process Transaction without matching Master
                   PERFORM 2200-PROCESS-ORPHAN-TRANSACTION
                   PERFORM 3200-READ-TRANSACTION-FILE
               WHEN 'M' *> Process matching Customer and Transaction(s)
                   MOVE CM-CUST-ID-IN TO WS-CURRENT-CUST-ID
                   PERFORM 2300-PROCESS-CUSTOMER-MATCH
                   PERFORM 3100-READ-CUSTOMER-MASTER
           END-EVALUATE
           .

       2100-PROCESS-CUSTOMER-ONLY.
           *> Customer exists, but no transactions this period.
           MOVE CM-CUST-BALANCE-IN TO WS-CURRENT-BALANCE
           PERFORM 2400-CALCULATE-CHARGES
           MOVE CM-CUST-ID-IN TO WS-CURRENT-CUST-ID *> For writing report
           MOVE CM-CUST-NAME-IN TO WS-DET-CUST-NAME *> For writing report

           PERFORM 4200-WRITE-DETAIL-REPORT
           PERFORM 3300-WRITE-UPDATED-CUSTOMER
           .

       2200-PROCESS-ORPHAN-TRANSACTION.
           *> Transaction record exists, but no matching master record
           DISPLAY "WARNING: Transaction for non-existent Customer ID: "
                   TR-CUST-ID
           *> Optionally write to an error report
           ADD 1 TO WS-TRANS-READ-COUNT *> Count it as read
           .

       2300-PROCESS-CUSTOMER-MATCH.
           *> Customer and at least one transaction match
           MOVE CM-CUST-BALANCE-IN TO WS-CURRENT-BALANCE
           MOVE ZEROES TO WS-TOTAL-PURCHASES WS-TOTAL-PAYMENTS

           *> Process all transactions for this customer (Control Break)
           PERFORM 2310-PROCESS-CUSTOMER-TRANSACTIONS
             UNTIL TR-CUST-ID NOT = WS-CURRENT-CUST-ID
                OR WS-TRANS-EOF

           *> After processing all transactions for this customer
           PERFORM 2400-CALCULATE-CHARGES

           MOVE CM-CUST-NAME-IN TO WS-DET-CUST-NAME *> For writing report
           PERFORM 4200-WRITE-DETAIL-REPORT
           PERFORM 3300-WRITE-UPDATED-CUSTOMER
           .

       2310-PROCESS-CUSTOMER-TRANSACTIONS.
           EVALUATE TRUE
               WHEN TR-TYPE-PURCHASE
                   ADD TR-AMOUNT TO WS-CURRENT-BALANCE
                   ADD TR-AMOUNT TO WS-TOTAL-PURCHASES
               WHEN TR-TYPE-PAYMENT
                   SUBTRACT TR-AMOUNT FROM WS-CURRENT-BALANCE
                   ADD TR-AMOUNT TO WS-TOTAL-PAYMENTS *> Payments are positive
               WHEN OTHER
                   DISPLAY "WARNING: Invalid transaction type for Cust: "
                           WS-CURRENT-CUST-ID ", Type: " TR-TYPE
           END-EVALUATE
           PERFORM 3200-READ-TRANSACTION-FILE
           .

       2400-CALCULATE-CHARGES.
           *> Calculate Finance Charge (only if balance > 0 before charges)
           IF CM-CUST-BALANCE-IN > ZERO
               COMPUTE WS-FINANCE-CHARGE ROUNDED =
                   CM-CUST-BALANCE-IN * WS-FINANCE-RATE
           ELSE
               MOVE ZERO TO WS-FINANCE-CHARGE
           END-IF
           ADD WS-FINANCE-CHARGE TO WS-CURRENT-BALANCE.

           *> Calculate Minimum Payment (only if new balance > 0)
           IF WS-CURRENT-BALANCE > ZERO
               COMPUTE WS-MINIMUM-PAYMENT ROUNDED =
                   WS-CURRENT-BALANCE * WS-MIN-PAY-RATE
               IF WS-MINIMUM-PAYMENT < WS-MIN-PAY-FLAT
                   MOVE WS-MIN-PAY-FLAT TO WS-MINIMUM-PAYMENT
               END-IF
               IF WS-MINIMUM-PAYMENT > WS-CURRENT-BALANCE
                   MOVE WS-CURRENT-BALANCE TO WS-MINIMUM-PAYMENT
               END-IF
           ELSE
               MOVE ZERO TO WS-MINIMUM-PAYMENT
           END-IF.

       3100-READ-CUSTOMER-MASTER.
           READ CUSTOMER-MASTER-IN
               AT END MOVE '10' TO WS-CUST-IN-STATUS
           END-READ
           IF NOT WS-CUST-IN-EOF AND WS-CUST-IN-STATUS = '00'
              ADD 1 TO WS-CUST-READ-COUNT
           ELSE
              IF WS-CUST-IN-STATUS NOT = '10' AND
                 WS-CUST-IN-STATUS NOT = '00'
                 MOVE 'CUSTOMER-MASTER-IN READ' TO WS-ABEND-FILE
                 MOVE WS-CUST-IN-STATUS TO WS-ABEND-STATUS
                 PERFORM 8000-ABEND-RTN
              END-IF
           END-IF
           IF WS-CUST-IN-EOF
              MOVE HIGH-VALUES TO CM-CUST-ID-IN *> Ensure last cust processes
           END-IF
           .

       3200-READ-TRANSACTION-FILE.
           READ TRANSACTION-FILE
               AT END MOVE '10' TO WS-TRANS-STATUS
           END-READ
           IF NOT WS-TRANS-EOF AND WS-TRANS-STATUS = '00'
              ADD 1 TO WS-TRANS-READ-COUNT
           ELSE
              IF WS-TRANS-STATUS NOT = '10' AND
                 WS-TRANS-STATUS NOT = '00'
                 MOVE 'TRANSACTION-FILE READ' TO WS-ABEND-FILE
                 MOVE WS-TRANS-STATUS TO WS-ABEND-STATUS
                 PERFORM 8000-ABEND-RTN
              END-IF
           END-IF
           IF WS-TRANS-EOF
              MOVE HIGH-VALUES TO TR-CUST-ID *> Ensure last cust processes
           END-IF
           .

       3300-WRITE-UPDATED-CUSTOMER.
           *> Build the output record - assuming layout matches input + new bal
           MOVE CUSTOMER-MASTER-IN-REC TO CUSTOMER-MASTER-OUT-REC.
           *> Re-find balance field in output record structure (if different)
           *> For this example, assume it's the same layout, just update balance
           MOVE WS-CURRENT-BALANCE TO CM-CUST-BALANCE-IN. *> Overwrite in buffer
           WRITE CUSTOMER-MASTER-OUT-REC FROM CUSTOMER-MASTER-IN-REC.
           IF WS-CUST-OUT-STATUS = '00'
               ADD 1 TO WS-CUST-WRITTEN-COUNT
           ELSE
               MOVE 'CUSTOMER-MASTER-OUT WRITE' TO WS-ABEND-FILE
               MOVE WS-CUST-OUT-STATUS TO WS-ABEND-STATUS
               PERFORM 8000-ABEND-RTN
           END-IF.

       4100-WRITE-REPORT-HEADERS.
           WRITE REPORT-RECORD FROM WS-REPORT-HEADER-1
               AFTER ADVANCING PAGE.
           MOVE WS-CURRENT-DATE-FORMATTED TO WS-HDR2-DATE.
           WRITE REPORT-RECORD FROM WS-REPORT-HEADER-2
               AFTER ADVANCING 1 LINE.
           WRITE REPORT-RECORD FROM WS-REPORT-HEADER-3
               AFTER ADVANCING 2 LINES.
           MOVE SPACES TO REPORT-RECORD.
           WRITE REPORT-RECORD AFTER ADVANCING 1 LINE. *> Blank line
           ADD 4 TO WS-REPORTS-WRITTEN-COUNT.

       4200-WRITE-DETAIL-REPORT.
           MOVE WS-CURRENT-CUST-ID TO WS-DET-CUST-ID.
           *> WS-DET-CUST-NAME is moved in calling paragraph
           MOVE CM-CUST-BALANCE-IN TO WS-DET-PREV-BAL. *> Original balance
           MOVE WS-FINANCE-CHARGE TO WS-DET-FIN-CHG.
           MOVE WS-CURRENT-BALANCE TO WS-DET-NEW-BAL.
           MOVE WS-MINIMUM-PAYMENT TO WS-DET-MIN-PAY.

           WRITE REPORT-RECORD FROM WS-REPORT-DETAIL-LINE
               AFTER ADVANCING 1 LINE.

           IF WS-REPORT-STATUS = '00'
               ADD 1 TO WS-REPORTS-WRITTEN-COUNT
           ELSE
               MOVE 'BILLING-REPORT WRITE' TO WS-ABEND-FILE
               MOVE WS-REPORT-STATUS TO WS-ABEND-STATUS
               PERFORM 8000-ABEND-RTN
           END-IF.

       8000-ABEND-RTN.
           MOVE 'ABNORMAL TERMINATION' TO WS-ABEND-MSG-TEXT.
           DISPLAY "**************************************************".
           DISPLAY WS-ABEND-MESSAGE.
           DISPLAY "CHECK FILE STATUS CODES AND LOGS.".
           DISPLAY "**************************************************".
           PERFORM 9100-CLOSE-FILES. *> Attempt to close files
           STOP RUN.

       8100-CHECK-FILE-STATUS.
           *> Simple loop to check initial OPEN status, add more details PRN
           CONTINUE.

       9000-TERMINATE.
           DISPLAY "CUSTOMER-BILLING PROGRAM ENDING...".
           DISPLAY "--------------------------------------------------".
           DISPLAY "CUSTOMERS READ:      " WS-CUST-READ-COUNT.
           DISPLAY "TRANSACTIONS READ:   " WS-TRANS-READ-COUNT.
           DISPLAY "CUSTOMERS WRITTEN:   " WS-CUST-WRITTEN-COUNT.
           DISPLAY "REPORT LINES WRITTEN:" WS-REPORTS-WRITTEN-COUNT.
           DISPLAY "--------------------------------------------------".
           PERFORM 9100-CLOSE-FILES.

       9100-CLOSE-FILES.
           CLOSE CUSTOMER-MASTER-IN
                 TRANSACTION-FILE
                 CUSTOMER-MASTER-OUT
                 BILLING-REPORT.
           *> Add checks for close status if needed.

      * END OF PROGRAM CUSTOMER-BILLING.