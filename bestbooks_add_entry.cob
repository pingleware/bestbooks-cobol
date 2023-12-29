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

	   PROCEDURE DIVISION.
       BEGIN.
	        OPEN EXTEND bestbooks.

            MOVE FUNCTION CURRENT-DATE TO TRANSACTION-DATE
            MOVE 1 TO TRANSACTION-ID
            MOVE "Cash" TO ACCOUNT
            MOVE 100.00 TO DEBIT
            MOVE 0.00 TO CREDIT
            MOVE "Investor Deposit" TO DESCRIPTION
            WRITE ENTRIES.

	        CLOSE bestbooks.
            STOP RUN.
	   END PROGRAM bestbooks_add_entry.

