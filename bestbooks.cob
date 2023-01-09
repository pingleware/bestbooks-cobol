       IDENTIFICATION DIVISION.
       PROGRAM-ID. BESTBOOKS.
       AUTHOR. PRESSPAGE ENTERTAINMENT INC dba PINGLEWARE.
       DATE-WRITTEN. 2022-NOV-28.
       CLASS-ID TAccount.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
         CLASS TAccount is "Object"
   
       IDENTIFICATION DIVISION.
       Object.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 AccountName PIC X(80).
        01 AccountType PIC X(10).
        01 Debit PIC S9(9) VALUE ZERO.
        01 Credit PIC S9(9) VALUE ZERO.
        01 Balance PIC S9(9) VALUE ZERO.

       PROCEDURE DIVISION.

       End Object
       End Class TAccount.
