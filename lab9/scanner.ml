(*

  SCANNER. Lexical scanner for a subset of Lisp.

  James Moen
  01 Apr 26

  Beware! There may be bugs.

*)

(* SCANNERLY. The type of module SCANNER. *)

module type Scannerly =
sig

 type token =
   CloseParenToken |
   EndToken |
   NumberToken of int |
   OpenParenToken |
   SymbolToken of string ;;

 val makeScanner: string -> (unit -> token) ;;

end ;;

(* SCANNER. Lexical scanner for a subset of Lisp. *)

module Scanner: Scannerly =
struct

(* TOKEN. A token for an expression in a subset of Lisp. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a scanner function NEXT TOKEN that reads TOKENs from a
   file whose pathname is PATH. INPUT is a channel connected to that file. CH
   holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT COMMENT. Skip a comment. It starts with a ';' and ends with a newline
   '\n' or an end of file '\000'. We skip the '\n', but not the '\000'. *)

  let rec nextComment () =
    match ! ch
    with '\000' ->
           () |
         '\n' ->
           nextChar () |
         _ ->
           nextChar () ;
           nextComment ()
  in

(* NEXT END TOKEN. Read an END TOKEN that indicates the end of INPUT. We don't
   skip a CHAR because there are no more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN. If it doesn't denote an INT then we
   read it as a SYMBOL TOKEN instead. *)

  let nextNumberToken () =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             (try
                NumberToken (int_of_string chars)
              with
                Failure _ ->
                  SymbolToken chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering ""
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN. *)

  let nextSymbolToken () =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling ""
  in

(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         ';' ->
           nextComment () ;
           nextToken () |
         '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken () |
         _ ->
           nextSymbolToken ()

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR
   from INPUT, then return (but do not call!) NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;

end ;;
