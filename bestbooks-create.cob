        IDENTIFICATION DIVISION.
	PROGRAM-ID. BESTBOOKS-CREATE.

	ENVIRONMENT DIVISION.
	CONFIGURATION SECTION.

	INPUT-OUTPUT SECTION.
	FILE-CONTROL.
	   SELECT OPTIONAL bestbooks
	   ASSIGN TO "BESTBOOKS.DAT"
	   ORGANIZATION IS INDEXED
	   ACCESS MODE IS DYNAMIC
	   RECORD KEY IS ledgerid or bestbooks-record

	DATA DIVISION.
	FILE SECTION.
	FD bestbooks.
	01 bestbooks-record.
	   03 ledgerid		PIC 99999.
           03 accountNo		PIC 999999.
	   03 accountName	PIC X(80).
	   03 debit		PIC S9(9).
	   03 credit		PIC S9(9).
	   03 balance		PIC S9(10).

	WORKING-STORAGE SECTION.
	01 display-record.
	   03 filler		PIC X(4)   value spaces.
	   03 ledgerid		PIC 99999.
	   03 filler            PIC X(4)   value spaces.
	   03 accountNo		PIC 999999.
	   03 filler            PIC X(4)   value spaces.
	   03 accountName	PIC X(80).
	   03 filler            PIC X(4)   value spaces.
	   03 debit		PIC S9(9).
	   03 credit		PIC S9(9).
	   03 balance	        PIC S9(10).

	PROCEDURE DIVISION.

	OPEN i-o bestbooks

	MOVE "00001 100000 Cash 0000100.00 0000000.00 00000100.00" TO bestbooks-record
	PERFORM write-bestbooks-record

	CLOSE bestbooks

	write-bestbooks-record.
	  write bestbooks-record
          end-write

	END PROGRAM BESTBOOKS-CREATE.

