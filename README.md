# BestBooks - COBOL

A COBOL version of BestBooks CORE.

https://youtu.be/u9M52sAnrOs?si=NEzNogKgm06YljFo

To build the COBOL files, use the script,

```
./build.sh FILENAME
```

where FILENAME is the COBOL root filename. The executable file will reside in the dist/ directory.

## Working Example

The [paycheck.cob](paycheck.cob) is a working example that reads the TIMECARDS.DAT file and displays pay ledger,

```
Charlie   Martin          41.0 10.0 $415.00  $68.06  $29.05  $25.73     $**292.16
Terry     Lacy            32.0 15.0 $480.00  $78.72  $33.60  $29.76     $**337.92
Donald    Trump           40.0 05.0 $200.00  $32.80  $14.00  $12.40     $**140.80
Patrick   Ingle           40.0 12.0 $480.00  $78.72  $33.60  $29.76     $**337.92
```

to compile,

```
cobc -x paycheck.cob -o dist/paycheck
```

to execute,

```
./dist/paycheck
```

the result shown above.

There is a bug, if you change a hourly rate to calculate a gross pay of $1,000 or more, the output is not display. For example, give Donald Trump a raise to $28 per hour,

```
Donald    Trump          40028
```

you see,

```
Charlie   Martin          41.0 10.0 $415.00  $68.06  $29.05  $25.73     $**292.16
Terry     Lacy            32.0 15.0 $480.00  $78.72  $33.60  $29.76     $**337.92
Donald    Trump           40.0 28.0$,120.00 $183.68  $78.40  $69.44     $**788.48
Patrick   Ingle           40.0 12.0 $480.00  $78.72  $33.60  $29.76     $**337.92
```

there is no thousands indicator.

To fix this, change the picture clause

```
 02 PRT-GROSS-PAY        PIC $,$$9.99.
```

to

```
02 PRT-GROSS-PAY        PIC $Z,999.99.
```

and added a spacing between pay rate and gross pay,

```
02 FILLER               PIC X.
```

which states prefix the dollar symbol and zero suppress values less than 1,000, the result is,

```
Charlie   Martin          41.0 10.0 $  415.00  $68.06  $29.05  $25.73     $**292.16
Terry     Lacy            32.0 15.0 $  480.00  $78.72  $33.60  $29.76     $**337.92
Donald    Trump           40.0 28.0 $1,120.00 $183.68  $78.40  $69.44     $**788.48
Patrick   Ingle           40.0 12.0 $  480.00  $78.72  $33.60  $29.76     $**337.92
```

The program with the fix is [paycheck_fixed.cob](paycheck_fixed.cob).

```
cobc -x paycheck_fixed.cob -o dist/paycheck_fixed 
```

# Open Cobol

OpenCobol is used to build BestBooks COBOL.

To install,

    sudo apt install gnucobol3

on Mac,

    brew install gnucobol

To compile a cobol source file,

    cobc -m bestbooks.cob

## OpenCobol Manual

The manual is available at [https://edoras.sdsu.edu/doc/opencobol.pdf ](https://edoras.sdsu.edu/doc/opencobol.pdf)and [https://gnucobol.sourceforge.io/guides/OpenCOBOL%20Programmers%20Guide.pdf](https://gnucobol.sourceforge.io/guides/OpenCOBOL%20Programmers%20Guide.pdf)

# Bestbooks for COBOL

### Reading the Ledger

The first example is reading from a LEDGER file and display the contents of the LEDGER. To build the executable,

```
2023-01-0100001Cash           0100000000  Initial deposit           
2023-01-0500002Supplies       0000000010  Purchase of office supplies   
2023-01-1000003Cash           0050000000  Sale of goods             
2023-12-3100004Equipment      0000000100  Purchase of equipment  

```

    cobc -x bestbooks_list_ledger.cob -o dist/bestbooks_list_ledger

In the FILE CONTROL section, we define the file to read,

```
FILE-CONTROL.
           SELECT LEDGER
               ASSIGN TO "LEDGER.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
```

we must also define the format of the file,

```
DATA DIVISION.
       FILE SECTION.
            FD LEDGER.
            01 ENTRIES.
               02 TRANSACTION-DATE  PIC A(10).
               02 TRANSACTION-ID    PIC 99999.
               02 ACCOUNT           PIC A(15).
               02 DEBIT             PIC 9999V99.
               02 CREDIT            PIC 9999V99.
               02 DESCRIPTION      PIC A(30).
```

and formats the output,

```
WORKING-STORAGE SECTION.
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
```

which is displayed as,

```
2023-01-01 00001 Cash            Initial deposit                $  100.00     $  000.00
2023-01-05 00002 Supplies        Purchase of office supplies    $  000.00     $  010.00
2023-01-10 00003 Cash            Sale of goods                  $  050.00     $  000.00
2023-12-31 00004 Equipment       Purchase of equipment          $  000.00     $  100.00
2023-12-31 00004 Equipment       Purchase of equipment          $  000.00     $  100.00
```

however, the last entry is being duplicated? To fix this problem, put the DISPLAY inside an IF dierctive testing for the end of file. 

```
IF NOT EOF THEN
                PERFORM PRINT-LEDGER
            END-IF.
```

and now displays correctly,

```
2023-01-01 00001 Cash            Initial deposit                $  100.00     $  000.00
2023-01-05 00002 Supplies        Purchase of office supplies    $  000.00     $  010.00
2023-01-10 00003 Cash            Sale of goods                  $  050.00     $  000.00
2023-12-31 00004 Equipment       Purchase of equipment          $  000.00     $  100.00
```

then we need to keep track of the total debits and credits, by creating a display for the totals and computing temporary variables for each ledger entry,

```
 * temporary variables in computational usage.
            01 TOTAL-DEBITS    PIC 9(4)V99 USAGE COMP.
            01 TOTAL-CREDITS   PIC 9(4)V99 USAGE COMP.
 * the display format
01 TOTAL.
                02 FILLER               PIC X(64).
                02 PRT-TOTAL-DEBITS     PIC $Z,999.99.
                02 FILLER               PIC X(5).
                02 PRT-TOTAL-CREDITS    PIC $Z,999.99.
 * the compute total function
COMPUTE-TOTALS.
            COMPUTE TOTAL-DEBITS = DEBIT + TOTAL-DEBITS
            COMPUTE TOTAL-CREDITS = CREDIT + TOTAL-CREDITS
            .
 * the display totals function
PRINT-TOTALS.
            MOVE TOTAL-DEBITS TO PRT-TOTAL-DEBITS
            MOVE TOTAL-CREDITS TO PRT-TOTAL-CREDITS
            DISPLAY TOTAL.
```

the output will result in,

```
2023-01-01 00001 Cash            Initial deposit                $  100.00     $  000.00
2023-01-05 00002 Supplies        Purchase of office supplies    $  000.00     $  010.00
2023-01-10 00003 Cash            Sale of goods                  $  050.00     $  000.00
2023-12-31 00004 Equipment       Purchase of equipment          $  000.00     $  100.00
                                                                $  150.00     $  110.00
```

### Adding an entry to the Ledger

In file [bestbooks_add_entry.cob](bestbooks_add_entry.cob), will add a new entry to the ledger file of [LEDGER.DAT](LEDGER.DAT),


## GNUCobol Command Line Options

```
GnuCOBOL compiler for most COBOL dialects with lots of extensions

Usage: cobc [options]... file...

Options:
  -h, --help            display this help and exit
  -V, --version         display compiler version information and exit
  -dumpversion          display compiler version and exit
  -i, --info            display compiler information (build/environment)
                        and exit
  -v, --verbose         verbose mode, display additional information;
                        multiple -v options increase the verbosity,
                        the maximum is 3 as follows:
                        (1) display compiler version and the commands
                        invoked by the compiler,
                        (2) pass verbose option to assembler/compiler
                        (3) pass verbose option to linker
  -q, --brief           reduced displays, commands invoked not shown
  -###                  like -v but commands not executed
  -x                    build an executable program
  -m                    build a dynamically loadable module (default)
  -j [<args>], --job[=<args>]   run program after build, passing <args>
  -std=<dialect>        warnings/features for a specific dialect
                        <dialect> can be one of:
                        default, cobol2014, cobol2002, cobol85, xopen,
                        ibm-strict, ibm, mvs-strict, mvs,
                        mf-strict, mf, bs2000-strict, bs2000,
                        acu-strict, acu, rm-strict, rm, gcos-strict,
                        gcos;
                        see configuration files in directory config
  -F, --free            use free source format (alias for -fformat=free)
  --fixed               use fixed source format (default; alias for
                        -fformat=fixed)
  -O, -O2, -O3, -Os     enable optimization
  -O0                   disable optimization
  -g                    enable C compiler debug and stack check
  -d, --debug           enable all run-time error checking,
                        equal to -fstack-check -fec=EC-ALL
  -fec=<exception-name> enable code generation for <exception-name>,
                        see --list-exceptions for the possible values,
                        sets -fsource-location
  -fno-ec=<exception-name>      disable code generation for <exception-name>
  -o <file>             place the output into <file>
  -b                    combine all input files into a single
                        dynamically loadable module
  -E                    preprocess only; do not compile or link
  -C                    translation only; convert COBOL to C
  -S                    compile only; output assembly file
  -c                    compile and assemble, but do not link
  -T <file>             generate and place a wide program listing into <file>
  -t <file>             generate and place a program listing into <file>
  --tlines=<lines>      specify lines per page in listing, default = 55
  -P[=<dir or file>]    generate preprocessed program listing (.lst)
  -X, --Xref            specify cross reference in listing
  -I <directory>        add <directory> to copy/include search path
  -L <directory>        add <directory> to library search path
  -l <lib>              link the library <lib>
  -K <entry>            generate CALL to <entry> as static
  -D <define>           define <define> for COBOL compilation
  -A <options>          add <options> to the C compile phase
  -Q <options>          add <options> to the C link phase
  --coverage            instrument generated binaries for coverage
  --conf=<file>         user-defined dialect configuration; see -std
  --list-reserved       display reserved words
  --list-intrinsics     display intrinsic functions
  --list-mnemonics      display mnemonic names
  --list-exceptions     display exception names
  --list-system         display system routines
  --save-temps[=<dir>]  save intermediate files
                        * default: current directory
  -MT <target>          set/add target file used in dependency list
  -MF <file>            place dependency list into <file>
  -ext <extension>      add file extension for resolving COPY

Warning options:
  -Wall                 enable most warnings (all except as noted below)
  -Wextra               like -Wall but enable some extra warning flags
  -w                    disable all warnings
  -Wno-<warning>        disable warning enabled by default, -Wall or -Wextra
  -Wadditional          additional warnings only raised with -Wall
  -Wno-unfinished       do not warn if unfinished features are used
                        * ALWAYS active
  -Wno-pending          do not warn if pending features are used
                        * ALWAYS active
  -Wno-repository-checks        do not warn/check for program/function/external signature mismatch
                        * ALWAYS active
  -Wno-ignored-error    do not warn about errors in code parts which are unreachable and so normally ignored
                        * ALWAYS active
  -Wobsolete            warn if obsolete features are used
  -Warchaic             warn if archaic features are used
  -Wredefinition        warn about non-referenced ambiguous data items
  -Wtruncate            warn about field truncation from constant assignments
  -Wpossible-truncate   warn about possible field truncation
                        * NOT set with -Wall
  -Woverlap             warn about overlapping MOVE of items
  -Wpossible-overlap    warn about MOVE of items that may overlap depending on variables
                        * NOT set with -Wall
  -Wparentheses         warn if parentheses are omitted around AND within OR
  -Wstrict-typing       warn strictly about type mismatch, even when same size
                        * NOT set with -Wall
  -Wtyping              warn about type mismatch
  -Wimplicit-define     warn whenever data items are implicitly defined
                        * NOT set with -Wall
  -Wno-corresponding    do not warn about CORRESPONDING with no matching items
                        * ALWAYS active
  -Winitial-value       warn if initial VALUE clause is ignored
  -Wprototypes          warn about missing FUNCTION prototypes/definitions
  -Warithmetic-osvs     warn if arithmetic expression precision has changed
  -Wcall-params         warn about non 01/77 items for CALL parameters
                        * NOT set with -Wall
  -Wconstant-expression  warn about expressions that always resolve to true/false
  -Wconstant-numlit-expression  warn about numeric expressions that always resolve to true/false
  -Wlarger-01-redefines  warn about larger redefines allowed by COBOL standards
  -Wcolumn-overflow     warn about text after program-text area, FIXED format
                        * NOT set with -Wall
  -Wterminator          warn about lack of scope terminator END-XXX
                        * NOT set with -Wall
  -Wlinkage             warn about dangling LINKAGE items
                        * NOT set with -Wall
  -Wunreachable         warn about likely unreachable statements
                        * NOT set with -Wall
  -Wno-dialect          do not warn about dialect specific issues
                        * ALWAYS active
  -Wno-goto-section     do not warn about GO TO section-name
                        * ALWAYS active
  -Wgoto-different-section      warn about GO TO a praragraph defined in a different section
  -Wsuspicious-perform-thru     warn if PERFORM THRU references procedures not in ascending order or multiple sections
                        * ALWAYS active
  -Wdangling-text       warn about source text after program-area
                        * NOT set with -Wall
  -Wno-missing-newline  do not warn about missing newlines
                        * ALWAYS active
  -Wno-others           do not warn about different issues
                        * ALWAYS active
  -Wno-unsupported      do not warn if runtime does not support a feature used
  -fdiagnostics-plain-output    make diagnostic output as plain as possible
  -Werror               treat all warnings as errors
  -Wno-error            don't treat warnings as errors
  -Werror=<warning>     treat specified <warning> as error
  -Wno-error=<warning>  don't treat specified <warning> as error

Compiler options:
  -fsign=[ASCII|EBCDIC] define display sign representation
                        * default: machine native
  -ffold-copy=[UPPER|LOWER]     fold COPY subject to value
                        * default: no transformation
  -ffold-call=[UPPER|LOWER]     fold PROGRAM-ID, CALL, CANCEL subject to value
                        * default: no transformation
  -fmax-errors=<number> maximum number of errors to report before
                        compilation is aborted
                        * default: 128
  -fintrinsics=[ALL|intrinsic function name(,name,...)]
                        intrinsics to be used without FUNCTION keyword
  -fdump=<scope>        dump data fields on abort, <scope> may be
                        a combination of: ALL, WS, LS, RD, FD, SC, LO
  -fcallfh=<name>       specifies <name> to be used for I/O
                        as external provided EXTFH interface module
  -febcdic-table=<cconv-table>/<file>   EBCDIC/ASCII translation table
                        * e.g. default, ebcdic500_latin1...
  -fdefault-colseq=[ASCII|EBCDIC|NATIVE]        define default collating sequence
                        * default: NATIVE
  -fstack-extended      store origin of entrypoints and PERFORM
                        * turned on by --debug/-fdump
  -fno-remove-unreachable       disable remove of unreachable code
                        * turned off by -g
  -ftrace               generate trace code
                        * scope: executed SECTION/PARAGRAPH
  -ftraceall            generate trace code
                        * scope: executed SECTION/PARAGRAPH/STATEMENTS
  -fsyntax-only         syntax error checking only; don't emit any output
  -fdebugging-line      enable debugging lines
                        * 'D' in indicator column or floating >>D
  -fsource-location     generate source location code
                        * turned on by --debug/-ftraceall/-fec/-fdump
  -fimplicit-init       automatic initialization of the COBOL runtime system
  -fno-recursive-check  disable check of recursive program call;
                        effectively compiling as RECURSIVE program
  -fstack-check         PERFORM stack checking
                        * turned on by --debug/-g
  -fmemory-check=<scope>     checks for invalid writes to internal storage,
                        <scope> may be one of: all, pointer, using, none
                        * default: none, set to all by --debug
  -fsection-exit-check  check that code execution does not leave the scope of SECTIONs
  -fimplicit-goback-check       check that code execution does not end implicit at end of PROCEDURE DIVISION
  -fwrite-after         use AFTER 1 for WRITE of LINE SEQUENTIAL
                        * default: BEFORE 1
  -fmfcomment           '*' in column 1 treated as comment with listing suppression
                        * FIXED/COBOL85/VARIABLE format only
  -facucomment          '$' in indicator area treated as '*',
                        '|' treated as floating comment
  -fno-trunc            allow numeric field overflow
                        * non-ANSI behaviour
  -fsingle-quote        use a single quote (apostrophe) for QUOTE
                        * default: double quote
  -foptional-file       treat all files as OPTIONAL
                        * unless NOT OPTIONAL specified
  -fstatic-call         output static function calls for the CALL statement
  -fno-gen-c-decl-static-call   disable generation of C function declarations
                        for subroutines with static CALL
  -fgen-c-line-directives       generate source location directives in C code;
                        * turned on by -g/--coverage
  -fgen-c-labels        generate extra labels in C sources;
                        * turned on by -g
  -fno-theaders         suppress all headers from listing while keeping
                        page breaks
  -fno-tsource          suppress source from listing
  -fno-tmessages        suppress warning and error summary from listing
  -ftsymbols            specify symbols in listing
  -ftcmd                specify command line in listing
  -fno-ttimestamp       suppress timestamp in listing headers
  -fttitle=<title>      set listing title with '_' replaced by spaces;
                        defaults to package name and version
  -fno-diagnostics-show-option  suppress output of option that directly
                        controls the diagnostic
  -fno-diagnostics-show-caret   do not display source context on warning/error diagnostic
  -fno-diagnostics-show-line-numbers    suppress display of line numbers in diagnostics

Compiler dialect configuration options:
  -freserved-words=<value>      use of complete/fixed reserved words
  -ftab-width=1..12      number of spaces that are assumed for tabs
  -ftext-column=72..255  right margin column number for fixed-form reference-format
  -fpic-length=<number>  maximum number of characters allowed in the PICTURE character-string
  -fword-length=1..63    maximum word-length for COBOL (= programmer defined) words
  -fliteral-length=<number>     maximum literal size in general
  -fnumeric-literal-length=1..38        maximum numeric literal size
  -fdefaultbyte=<value>  default initialization for fields without VALUE, may be one of
                         * character in quotes
                         * decimal 0..255 representing a character
                         * "init" to initialize to PICTURE/USAGE
                         * "none" to do no explicit initialization
                         * default: "init"
  -fformat=<value>       default reference-format, may be one of: FIXED, FREE, COBOL85, VARIABLE, XOPEN, XCARD, CRT, TERMINAL, COBOLX
  -fbinary-size=<value>  binary byte size - defines the allocated bytes according to PIC, may be one of: 2-4-8, 1-2-4-8, 1--8
  -fbinary-byteorder=<value>    binary byte order, may be one of: native, big-endian
  -fassign-clause=<value>       how to interpret 'ASSIGN word': as 'ASSIGN EXTERNAL word' or 'ASSIGN DYNAMIC word', may be one of: dynamic, external, ibm (= external), mf (= dynamic)
  -fscreen-section-rules=<value>        which compiler's rules to apply to SCREEN SECTION item clauses, may be one of: acu, gc, mf, rm, std, xopen
  -fdpc-in-data=<value>  whether DECIMAL-POINT IS COMMA has effect in XML/JSON GENERATE, may be one of: none, xml, json, all
  -fsubscript-check=<value>     checking for subscript (only done with EC-BOUND-SUBSCRIPT active), may be one of: full, max, record
  -ffilename-mapping     resolve file names at run time using environment variables
  -fpretty-display       alternate formatting of numeric fields
  -fbinary-truncate      numeric truncation according to ANSI
  -fcomplex-odo          allow non-standard OCCURS DEPENDING ON syntax
  -fodoslide             adjust items following OCCURS DEPENDING (implies complex-odo)
  -finit-justify         applies JUSTIFY with VALUE clause
  -findirect-redefines   allow REDEFINES to other than last equal level number
  -frelax-syntax-checks  allow certain syntax variations (e.g. REDEFINES position)
  -fref-mod-zero-length  allow zero length reference-modification (only changed with EC-BOUND-REF-MOD active)
  -frelax-level-hierarchy       allow non-matching level numbers
  -fselect-working       require ASSIGN USING items to be in WORKING-STORAGE
  -flocal-implies-recursive     LOCAL-STORAGE SECTION implies RECURSIVE attribute
  -fsticky-linkage       LINKAGE SECTION items remain allocated between invocations
  -fmove-ibm             MOVE operates as on IBM (left to right, byte by byte)
  -fperform-osvs         exit point of any currently executing perform is recognized if reached
  -farithmetic-osvs      limit precision in intermediate results to precision of final result (less accurate)
  -fconstant-folding     evaluate constant expressions at compile time
  -fhostsign             allow hexadecimal value 'F' for NUMERIC test of signed PACKED DECIMAL field
  -fprogram-name-redefinition   program names don't lead to a reserved identifier
  -faccept-update        set WITH UPDATE clause as default for ACCEPT dest-item, instead of WITH NO UPDATE
  -faccept-auto          set WITH AUTO clause as default for ACCEPT dest-item, instead of WITH TAB
  -fconsole-is-crt       assume CONSOLE IS CRT if not set otherwise
  -fno-echo-means-secure        NO-ECHO hides input with asterisks like SECURE
  -fline-col-zero-default       assume a field DISPLAY starts at LINE 0 COL 0 (i.e. at the cursor), not LINE 1 COL 1
  -fdisplay-special-fig-consts  special behaviour of DISPLAY SPACE/ALL X'01'/ALL X'02'/ALL X'07'
  -fbinary-comp-1        COMP-1 is a 16-bit signed integer
  -fnumeric-pointer      POINTER is a 64-bit unsigned integer
  -fmove-non-numeric-lit-to-numeric-is-zero     imply zero in move of non-numeric literal to numeric items
  -fimplicit-assign-dynamic-var implicitly define a variable if an ASSIGN DYNAMIC does not match any data item
  -fdevice-mnemonics     specifying device by mnemonic
  -fxml-parse-xmlss      XML PARSE XMLSS
  -fareacheck            check contents of Area A (when reference format supports Area A enforcement),
                         enabled checks include:
                         * division, section, paragraph names, level indicators (FD, SD, RD, and CD),
                           and toplevel numbers (01 and 77) must start in Area A;
                         * statements must not start in Area A; and
                         * separator periods must not be within Area A
  -fcomment-paragraphs=<support>        comment paragraphs in IDENTIFICATION DIVISION (AUTHOR, DATE-WRITTEN, ...)
  -fcontrol-division=<support>  CONTROL DIVISION
  -fpartial-replace-when-literal-src=<support>  apply partial replacing with literal source operand even when it replaces with spaces only;
                         * "skip" prevents such replacements
  -fmemory-size-clause=<support>        MEMORY-SIZE clause
  -fmultiple-file-tape-clause=<support> MULTIPLE-FILE-TAPE clause
  -flabel-records-clause=<support>      LABEL-RECORDS clause
  -fvalue-of-clause=<support>   VALUE-OF clause
  -fdata-records-clause=<support>       DATA-RECORDS clause
  -ftop-level-occurs-clause=<support>   OCCURS clause on top-level
  -fsame-as-clause=<support>    SAME AS clause
  -ftype-to-clause=<support>    TYPE TO clause
  -fusage-type=<support>        USAGE type-name
  -fsynchronized-clause=<support>       SYNCHRONIZED clause
  -fsync-left-right=<support>   LEFT/RIGHT phrases in SYNCHRONIZED clause
  -fspecial-names-clause=<support>      SPECIAL-NAMES clause
  -fgoto-statement-without-name=<support>       GO TO statement without name
  -fstop-literal-statement=<support>    STOP-literal statement
  -fstop-identifier-statement=<support> STOP-identifier statement
  -fstop-error-statement=<support>      STOP ERROR statement
  -fdebugging-mode=<support>    DEBUGGING MODE and debugging indicator
  -fuse-for-debugging=<support> USE FOR DEBUGGING
  -fpadding-character-clause=<support>  PADDING CHARACTER clause
  -fnext-sentence-phrase=<support>      NEXT SENTENCE phrase
  -flisting-statements=<support>        listing-directive statements EJECT, SKIP1, SKIP2, SKIP3
  -ftitle-statement=<support>   listing-directive statement TITLE
  -fentry-statement=<support>   ENTRY statement
  -fmove-noninteger-to-alphanumeric=<support>   move noninteger to alphanumeric
  -fmove-figurative-constant-to-numeric=<support>       move figurative constants to numeric
  -fmove-figurative-space-to-numeric=<support>  move figurative constant SPACE to numeric
  -fmove-figurative-quote-to-numeric=<support>  move figurative constant QUOTE to numeric
  -fodo-without-to=<support>    OCCURS DEPENDING ON without to
  -fsection-segments=<support>  section segments
  -falter-statement=<support>   ALTER statement
  -fcall-overflow=<support>     OVERFLOW clause for CALL
  -fnumeric-boolean=<support>   boolean literals (B'1010')
  -fhexadecimal-boolean=<support>       hexadecimal-boolean literals (BX'A')
  -fnational-literals=<support> national literals (N'UTF-16 string')
  -fhexadecimal-national-literals=<support>     hexadecimal-national literals (NX'265E')
  -fnational-character-literals=<support>       non-standard national literals (NC'UTF-16 string')
  -fhp-octal-literals=<support> HP COBOL octal literals (%377)
  -facu-literals=<support>      ACUCOBOL-GT literals (#B #O #H #X)
  -febcdic-symbolic-characters  EBCDIC symbolic characters in literals (" "135,151,151"bar"195, 194"Z" for " foobarBAZ")
  -fword-continuation=<support> continuation of COBOL words
  -fnot-exception-before-exception=<support>    NOT ON EXCEPTION before ON EXCEPTION
  -faccept-display-extensions=<support> extensions to ACCEPT and DISPLAY
  -frenames-uncommon-levels=<support>   RENAMES of 01-, 66- and 77-level items
  -flarger-redefines=<support>  allow larger REDEFINES items
  -fsymbolic-constant=<support> constants defined in SPECIAL-NAMES
  -fconstant-78=<support>       constant with level 78 item (note: has left to right precedence in expressions)
  -fconstant-01=<support>       constant with level 01 CONSTANT AS/FROM item
  -fperform-varying-without-by=<support>        PERFORM VARYING without BY phrase (implies BY 1)
  -freference-out-of-declaratives=<support>     references to sections not in DECLARATIVES from within DECLARATIVES
  -fprogram-prototypes=<support>        CALL/CANCEL with program-prototype-name
  -fcall-convention-mnemonic=<support>  specifying call-convention by mnemonic
  -fcall-convention-linkage=<support>   specifying call-convention by WITH ... LINKAGE
  -fusing-optional=<support>    support for PROCEDURE DIVISION USING OPTIONAL
  -fnumeric-value-for-edited-item=<support>     numeric literals in VALUE clause of numeric-edited items
  -fincorrect-conf-sec-order=<support>  incorrect order of CONFIGURATION SECTION paragraphs
  -fdefine-constant-directive=<support> allow >> DEFINE CONSTANT var AS literal
  -ffree-redefines-position=<support>   REDEFINES clause not following entry-name in definition
  -frecords-mismatch-record-clause=<support>    record sizes does not match RECORD clause
  -frecord-delimiter=<support>  RECORD DELIMITER clause
  -fsequential-delimiters=<support>     BINARY-SEQUENTIAL and LINE-SEQUENTIAL phrases in RECORD DELIMITER
  -frecord-delim-with-fixed-recs=<support>      RECORD DELIMITER clause on file with fixed-length records
  -fmissing-statement=<support> missing statement (e.g. empty IF / PERFORM)
  -fmissing-period=<support>    missing period in PROCEDURE DIVISION (when reference format supports Area A enforcement)
  -fzero-length-literals=<support>      zero-length literals, e.g. '' and ""
  -fxml-generate-extra-phrases=<support>        XML GENERATE's phrases other than COUNT IN
  -fcontinue-after=<support>    AFTER phrase in CONTINUE statement
  -fgoto-entry=<support>        ENTRY FOR GO TO and GO TO ENTRY statements
  -fassign-variable=<support>   ASSIGN [TO] variable in SELECT
  -fassign-using-variable=<support>     ASSIGN USING/VARYING variable in SELECT
  -fassign-ext-dyn=<support>    ASSIGN EXTERNAL/DYNAMIC in SELECT
  -fassign-disk-from=<support>  ASSIGN DISK FROM variable in SELECT
  -fvsam-status=<support>       VSAM status in FILE STATUS
  -fself-call-recursive=<support>       CALL to own PROGRAM-ID implies RECURSIVE attribute
  -frecord-contains-depending-clause=<support>  DEPENDING clause in RECORD CONTAINS
  -fpicture-l=<support>  PICTURE string with 'L' character
        where <support> is one of the following:
        'ok', 'warning', 'archaic', 'obsolete', 'skip', 'ignore', 'error', 'unconformable'
  -fnot-reserved=<word>  word to be taken out of the reserved words list
  -freserved=<word>      word to be added to reserved words list
  -freserved=<word>:<alias>     word to be added to reserved words list as alias
  -fnot-register=<word>  special register to disable
  -fregister=<word> or <word>:<definition>, where definition uses backslash
```
