  *
  *----------------------------------------------------------------*
  *                                                                *
  *                                                                *
  *                                                                *
  * SYSTEM                                                         *
  *                                                                *
  * PROGRAM-ID    Pxxxxxx
  *                                                                *
  * AUTHOR                                                         *
  *                                                                *
  * DATE          10/2016                                          *
  *                                                                *
  *                                                                *
  *                                                                *
  *                                                                *
  *----------------------------------------------------------------*
  *                                                                *
  *                                                                *
  *                                                                *
  *----------------------------------------------------------------*
   IDENTIFICATION DIVISION.
   PROGRAM-ID.  Pxxxxx.

   ENVIRONMENT DIVISION.
   CONFIGURATION SECTION.

  /----------------------------------------------------------------*
   INPUT-OUTPUT SECTION.
   FILE-CONTROL.

  *----------------------------------------------------------------*



  /----------------------------------------------------------------*
   DATA DIVISION.
   FILE SECTION.
  *----------------------------------------------------------------*

  /----------------------------------------------------------------*
   WORKING-STORAGE SECTION.
  *----------------------------------------------------------------*



   01 proc-ptr usage procedure-pointer.


   01 sqlite3-db         pointer.

   01 err-msg            pointer.
   01 sqlite             pointer.
   01 res                pointer.



   01 rc                 pic 9 comp-5.
   01 dbName             pic x(08).






   01 sqlQuery       pic x(100).

   01 result         pic x(100).
   01 argv.
       03  firstColumn   pointer.
       03  secondColumn  pointer.

   01 azColName          pointer.
   01 argc               pic 99 comp-5.
   01 notused            pointer.

   01 Writefunction-Ptr  procedure-pointer.







  *-----------------------------------------------------------------
   Local-storage Section.
   Linkage Section.
   01 Column-Id       pic X(3).
   01 Column-Name     pic X(20).





  /-----------------------------------------------------------------
   procedure division.
  *-----------------------------------------------------------------

  *
            set proc-ptr to entry "sqlite3.dll"

            display sqlQuery


            set sqlite3-db to null
            set err-msg    to null
            set res        to null

            move z"test.db" to dbName

            display "Running sqlite3_open"

            call "sqlite3_open" using
                    by reference  z"test.db"
                    by reference  sqlite3-db
                    returning     rc
            end-call


            if rc not = zero
               display "error opening database."

            else
               display "database opened."

            end-if


            move "INSERT INTO tabla VALUES ('002', '8855');"
               to sqlQuery
  *
  *
  *
            call "sqlite3_exec" using
               by value sqlite3-db
               by reference sqlQuery
               by value     0
               by value     0
               by reference err-msg
               returning rc
            end-call
  *
         set Writefunction-Ptr to entry "sqlite-callback".

          initialize sqlQuery
          move "SELECT * FROM tabla;" to sqlQuery


          call "sqlite3_exec" using
             by value sqlite3-db
             by reference sqlQuery
             by value Writefunction-Ptr
             by value 0
             by reference err-msg
           returning rc
          end-call

          call "sqlite3_close" using
                  by reference sqlite3-db
          end-call

          display "sqlite3_close"

            .
  * -------------------------------------------------------
   stop run.
         entry "sqlite-callback"
         using
             by value notused
             by value argc
             by reference argv
             by reference azColName.

        set address of Column-Id   to firstColumn
        set address of Column-Name to secondColumn

        display Column-id "|" Column-Name
        goback.
   Entry-Termination.
