       IDENTIFICATION DIVISION.
       PROGRAM-ID. BESTBOOKS.
       AUTHOR. PRESSPAGE ENTERTAINMENT INC dba PINGLEWARE.
       DATE-WRITTEN. 2022-NOV-28.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEDGER
               ASSIGN TO "LEDGER.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
            FD LEDGER.
            01 ENTRIES.
               02 TRANSACTION-DATE  PIC A(10).
               02 TRANSACTION-ID    PIC 99999.
               02 ACCOUNT           PIC A(15).
               02 DEBIT             PIC 9999V99.
               02 CREDIT            PIC 9999V99.
               02 DESCRIPTION       PIC A(30).
       WORKING-STORAGE SECTION.
      * temporary variables in computational usage.
            01 TOTAL-DEBITS    PIC 9(4)V99 USAGE COMP.
            01 TOTAL-CREDITS   PIC 9(4)V99 USAGE COMP.
      * print format of the ledger
            01 LEDGER-RECORD.
                02 PRT-TRANSACTION-DATE  PIC A(10).
                02 FILLER                PIC X.
                02 PRT-TRANSACTION-ID    PIC 9(5).
                02 FILLER                PIC X.
                02 PRT-ACCOUNT           PIC A(15).
                02 FILLER                PIC X.
                02 PRT-DESSCRIPTION      PIC A(30).
                02 FILLER                PIC X.
                02 PRT-DEBIT             PIC $Z,999.99.
                02 FILLER                PIC X(5).
                02 PRT-CREDIT            PIC $Z,999.99.
            01 TOTAL.
                02 FILLER               PIC X(64).
                02 PRT-TOTAL-DEBITS     PIC $Z,999.99.
                02 FILLER               PIC X(5).
                02 PRT-TOTAL-CREDITS    PIC $Z,999.99.
      * 88 Level is for conditions.
               01 END-FILE              PIC X.
                  88  EOF VALUE "T".
       PROCEDURE DIVISION.
       BEGIN.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM PROCESS-LINE WITH TEST BEFORE UNTIL EOF
           PERFORM CLEAN-UP.
          
       INITIALIZE-PROGRAM.
            OPEN INPUT LEDGER.
       PROCESS-LINE.
            READ LEDGER INTO ENTRIES
               AT END MOVE "T" TO END-FILE
            END-READ.

            IF NOT EOF THEN
                PERFORM COMPUTE-TOTALS
                PERFORM PRINT-LEDGER
            ELSE 
                PERFORM PRINT-TOTALS
            END-IF.
        PRINT-LEDGER.
            MOVE TRANSACTION-DATE To PRT-TRANSACTION-DATE
            MOVE TRANSACTION-ID TO PRT-TRANSACTION-ID
            MOVE ACCOUNT TO PRT-ACCOUNT
            MOVE DEBIT TO PRT-DEBIT
            MOVE CREDIT TO PRT-CREDIT
            MOVE DESCRIPTION TO PRT-DESSCRIPTION
            DISPLAY LEDGER-RECORD.
       PRINT-TOTALS.
            MOVE TOTAL-DEBITS TO PRT-TOTAL-DEBITS
            MOVE TOTAL-CREDITS TO PRT-TOTAL-CREDITS
            DISPLAY TOTAL.
       COMPUTE-TOTALS.
            COMPUTE TOTAL-DEBITS = DEBIT + TOTAL-DEBITS
            COMPUTE TOTAL-CREDITS = CREDIT + TOTAL-CREDITS
            .
       CLEAN-UP.
            CLOSE LEDGER.
            STOP RUN.
       END PROGRAM BESTBOOKS.
