       IDENTIFICATION DIVISION.
	   PROGRAM-ID. bestbooks_add_entry.

	   ENVIRONMENT DIVISION.
	   CONFIGURATION SECTION.

	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.
	       SELECT OPTIONAL bestbooks
	        ASSIGN TO "LEDGER.DAT"
	            ORGANIZATION IS LINE SEQUENTIAL
                ACCESS IS SEQUENTIAL.
	    DATA DIVISION.
	    FILE SECTION.
            FD bestbooks.
            01 ENTRIES.
               02 TRANSACTION-DATE  PIC A(10).
               02 TRANSACTION-ID    PIC 99999.
               02 ACCOUNT           PIC A(15).
               02 DEBIT             PIC 9999V99.
               02 CREDIT            PIC 9999V99.
               02 DESCRIPTION       PIC A(30).

	   WORKING-STORAGE SECTION.
            01 WS-TODAY             PIC X(10).
            01 WS-YEAR              PIC X(4).
            01 WS-MONTH             PIC X(2).
            01 WS-DAY               PIC X(2).
            01 WS-FORMATTED-DATE    PIC X(12).

            01 USER-ACCOUNT         PIC X(15).
            01 USER-AMOUNT          PIC X(10).
            01 USER-DESCRIPTION     PIC X(30).

            01 USER-TRANSACTION-ID       PIC 9(5). 

            01 END-FILE              PIC X.
                  88  EOF VALUE "T".

	   PROCEDURE DIVISION.
       BEGIN.
            ACCEPT USER-ACCOUNT FROM ARGUMENT-VALUE
            ACCEPT USER-AMOUNT FROM ARGUMENT-VALUE
            ACCEPT USER-DESCRIPTION FROM ARGUMENT-VALUE

            IF LENGTH USER-ACCOUNT < 0
                MOVE "Cash" TO USER-ACCOUNT 
            END-IF.

            IF LENGTH USER-AMOUNT < 0
                MOVE 100.00 TO USER-AMOUNT
            END-IF.   

            MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY

            MOVE WS-TODAY(1:4) TO WS-YEAR.
            MOVE WS-TODAY(5:2) TO WS-MONTH.
            MOVE WS-TODAY(7:2) TO WS-DAY.

            MOVE WS-YEAR  TO WS-FORMATTED-DATE(1:4).
            MOVE "-"       TO WS-FORMATTED-DATE(5:1).
            MOVE WS-MONTH TO WS-FORMATTED-DATE(6:2).
            MOVE "-"       TO WS-FORMATTED-DATE(8:1).
            MOVE WS-DAY   TO WS-FORMATTED-DATE(9:2).

            OPEN INPUT bestbooks.
                PERFORM PROCESS-LINE WITH TEST BEFORE UNTIL EOF
            CLOSE bestbooks.

            DISPLAY USER-TRANSACTION-ID.

	        OPEN EXTEND bestbooks.

            MOVE WS-FORMATTED-DATE TO TRANSACTION-DATE
            
            MOVE USER-ACCOUNT TO ACCOUNT
            IF FUNCTION NUMVAL(USER-AMOUNT) < 0
                MOVE 0.00 TO DEBIT
                MOVE FUNCTION NUMVAL(USER-AMOUNT) TO CREDIT
            ELSE
                MOVE FUNCTION NUMVAL(USER-AMOUNT) TO DEBIT
                MOVE 0.00 TO CREDIT
            END-IF.
            IF LENGTH OF USER-DESCRIPTION = 0
                MOVE "N/A" TO USER-DESCRIPTION
            ELSE
                MOVE USER-DESCRIPTION TO DESCRIPTION
            END-IF.
            COMPUTE USER-TRANSACTION-ID = USER-TRANSACTION-ID + 1
            MOVE USER-TRANSACTION-ID TO TRANSACTION-ID
            WRITE ENTRIES.

	        CLOSE bestbooks.
            STOP RUN.
       PROCESS-LINE.
            READ bestbooks INTO ENTRIES
               AT END MOVE "T" TO END-FILE
            END-READ.
            IF NOT EOF
                ADD 1 TO USER-TRANSACTION-ID
            END-IF.

	   END PROGRAM bestbooks_add_entry.

