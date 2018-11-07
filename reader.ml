
(* reader.ml
 * A compiler from Scheme to x86/64
 *
 * Programmer: Mayer Goldberg, 2018
 *)

#use "pc.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Int of int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | Vector of sexpr list;;

let _symbol_ = 
	let _digit_ = PC.range '0' '9' in
	let _aTOz_ = PC.range 'a' 'z' in
	let _ATOZ_ = PC.range 'A' 'Z' in
	let _punctuations_ = PC.one_of "!$^*-_=+<>/?:" in
	let _symbolChar_ = PC.disj (PC.disj (PC.disj _digit_ _aTOz_) _ATOZ_) _punctuations_ in
	let _symbols_ = PC.plus _symbolChar_ in
	PC.pack _symbols_ (fun (a)-> Symbol(list_to_string (List.map Char.lowercase a)));;


let _boolean_ =
	let _true_ = PC.char_ci 't' in
	let _hash_ = PC.char '#' in
	let _false_ = PC.char_ci 'f' in
  	let _bool_ = PC.not_followed_by (PC.caten _hash_ (PC.disj _true_ _false_)) _symbol_ in
  	PC.pack _bool_ (fun ((a,b)) -> Bool(b = 't' || b = 'T'));;

(* NEEEED TO ADD PAGE*)

let _namedChar_ = 
	let _newline_ = 
		let _newlines_ = PC.word_ci "newline" in
		PC.pack _newlines_ (fun (s) -> ('\n')) in
	let _nul_ = 
		let _nuls_ = PC.word_ci "nul" in
		PC.pack _nuls_ (fun (s) -> ('\000')) in
	let _return_ =
		let _returns_ = PC.word_ci "return" in
		PC.pack _returns_ (fun (s) -> ('\r')) in
	let _space_ =
		let _spaces_ = PC.word_ci "space" in
		PC.pack _spaces_ (fun (s) -> (' ')) in
	let _tab_ =
	 	let _tabs_ = PC.word_ci "tab" in
	 	PC.pack _tabs_ (fun (s) -> ('\t')) in
		PC.disj_list ([_newline_;_nul_;_return_;_space_;_tab_]);;

let _hexDigit_ = 
	let _digit_ = PC.range '0' '9' in
	let _aTOf_ = PC.range 'a' 'f' in
	let _ATOF_ = PC.range 'A' 'F' in
	PC.disj (PC.disj _digit_ _aTOf_) _ATOF_;;

let _hexChar_ = 
	let _x_ = PC.char 'x' in
	let _hexNatural_ = PC.plus _hexDigit_ in
	let _hexChars_ = PC.caten _x_ _hexNatural_ in
	PC.pack _hexChars_ (fun ((a,b)) -> 
		let hexInt = list_to_string b in
		let hexSymbol = "0x" in
		let newInt = (String.concat "" [hexSymbol;hexInt]) in
		Char.chr(int_of_string(newInt)));;

let _char_ = 
	let _charPrefix_ = PC.caten (PC.char '#') (PC.char '\\') in
	let _visibleSimpleChar_ = PC.range (Char.chr 33) (Char.chr 127) in
	let _chars_ = PC.caten _charPrefix_ (PC.disj_list([_namedChar_;_hexChar_;_visibleSimpleChar_;])) in
	PC.pack _chars_ (fun ((a,b),c) -> Char(c));;


let _stringMetaChar_ =
	let _slash_ =
		let _slashs_ = PC.caten (PC.char '\\') (PC.char '\\') in
		PC.pack _slashs_ (fun (a,b) -> '\\') in
(* NEED TO DOOOO
let _page_ = 
	let _pages_ = PC.caten (PC.char '\\') (PC.char 'f') in
	PC.pack _pages_ (fun (a,b) -> '\f');;*)
	let _tab_ = 
		let _tabs_ = PC.caten (PC.char '\\') (PC.char 't') in 
		PC.pack _tabs_ (fun (a,b) -> '\t') in
	let _merchaot_ =
		let _mercahots_ = PC.caten (PC.char '\\') (PC.char '\"') in 
		PC.pack _mercahots_ (fun (a,b) -> '\"') in
	let _newline_ = 
		let _newlines_ = PC.caten (PC.char '\\') (PC.char 'n') in
		PC.pack _newlines_ (fun (a,b) -> '\n') in
	let _r_ = 
		let _rs_ = PC.caten (PC.char '\\') (PC.char 'r') in
		PC.pack _rs_ (fun (a,b) -> '\r') in
	 	PC.disj_list ([_slash_;_tab_;_merchaot_;_newline_;_r_]);;

let _stringHexChar_ = 
	let _hexNatural_ = PC.plus _hexDigit_ in
	let _slashX_ = PC.caten (PC.char '\\') (PC.char 'x') in
	let _semiColon_ = PC.char ';' in
	let _stringHexChars_ =	 PC.caten (PC.caten _slashX_ _hexNatural_) _semiColon_ in
	PC.pack _stringHexChars_ (fun ((a,b), c) -> 
		let hexInt = list_to_string b in
		let hexSymbol = "0x" in
		let newInt = (String.concat "" [hexSymbol;hexInt]) in
		Char.chr(int_of_string(newInt)));;

let _stringLiteralChar_ = 
	let _backSlashOrDoubleQuote_ = PC.disj (PC.char '\"') (PC.char '\\') in
	let _otherThanSlashOrDoubleQuote = PC.range (Char.chr 0) (Char.chr 127) in
	PC.diff _otherThanSlashOrDoubleQuote _backSlashOrDoubleQuote_;;

let _stringChar_ = PC.disj_list ([_stringHexChar_;_stringMetaChar_;_stringLiteralChar_;]);;
(*
                NEED TO CHECK                *)
let _string_ = 
	let _doubleQuote_ =  PC.char '\"' in
	PC.caten (PC.caten (_doubleQuote_ (PC.star _stringChar_))) _doubleQuote_;;
	

let _integer_ =
	let _plusOrMinus_ = PC.maybe (PC.disj (PC.char '+') (PC.char '-')) in
	let _digit_ = PC.range '0' '9' in
	let _natural_ = PC.plus _digit_ in
  	let _int_ = PC.caten _plusOrMinus_ _natural_ in
  	PC.pack _int_ (fun ((sign,nat)) ->
	  match sign with
	  | Some('-') -> Int ((-1) * int_of_string (list_to_string nat))
	  | Some('+') -> Int (int_of_string (list_to_string nat))
  	  | _ -> Int (int_of_string (list_to_string nat)));;

let _float_ =
	let _digit_ = PC.range '0' '9' in
	let _natural_ = PC.plus _digit_ in
	let _dot_ = PC.char '.' in
  	let _floats_ = PC.caten (PC.caten _integer_ _dot_) _natural_ in
  	PC.pack _floats_ (fun (((a,b), c)) ->
  	let afterDot = float_of_string (list_to_string c) *. 10.0**(float_of_int ((-1)*String.length (list_to_string c))) in
  	match a with
  	| (Int x_int) -> Float(if float_of_int x_int > 0.0 then float_of_int x_int +. afterDot
  	else float_of_int x_int -. afterDot)
  	| _ -> Float(0.0));;



let _hexInteger_ = 
	let _plusOrMinus_ = PC.maybe (PC.disj (PC.char '+') (PC.char '-')) in
	let _hexPrefix_ = PC.word "#x" in
	let _hexNatural_ = PC.plus _hexDigit_ in
	let _hexIntegers_ = PC.caten (PC.caten _hexPrefix_ _plusOrMinus_) _hexNatural_ in
	PC.pack _hexIntegers_ (fun ((a,sign),i) -> 
	  let hexNum = int_of_string (String.concat "" ["0x";(list_to_string i)]) in
	  match sign with
	  | Some('-') -> Int ((-1) * hexNum)
	  | Some('+') -> Int (hexNum)
  	  | _ -> Int (hexNum));;

let _hexExample_ = 
	let _dot_ = PC.char '.' in
	let _hexNatural_ = PC.plus _hexDigit_ in
	PC.caten (PC.caten _hexInteger_ _dot_) _hexNatural_;;

let _hexFloat_ = 
	let _dot_ = PC.char '.' in
	let _hexNatural_ = PC.plus _hexDigit_ in
	let _hexFloats_ = PC.caten (PC.caten _hexInteger_ _dot_) _hexNatural_ in 
	PC.pack _hexFloats_ (fun ((a,b),c) ->
  		let afterDot =  float_of_string (String.concat "" ["0x";(list_to_string c)]) *. 10.0**(float_of_int ((-1)*String.length (list_to_string c))) in
  		match a with
  		| (Int x_int) -> Float(if float_of_int x_int > 0.0 then float_of_int x_int +. afterDot
  		else float_of_int x_int -. afterDot)
  		| _ -> Float(0.0));;

let _number_ = 
	let _numbers_ = PC.disj (PC.disj (PC.disj _hexFloat_ _float_) _hexInteger_) _integer_ in
	PC.pack _numbers_ (fun (a) -> Number(a));;

let _space_ = PC.char ' ';;
let _lparen_ = make_spaced (PC.char '(');;
let _rparen_ = make_spaced (PC.char ')');;
let _mulop_ = make_spaced (PC.char '*');;
let _addop_ = make_spaced (PC.char '+');;
let make_enclosed _l_ _p_ _r_ =
  let _enclosed_ = PC.caten (PC.caten _l_ _p_) _r_ in
  PC.pack _enclosed_ (fun ((l, p), r) -> p);;
let make_spaced _p_ = 
  let _st_space_ = PC.star _space_ in
  make_enclosed _st_space_ _p_ _st_space_;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(n1), Number(n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | Vector(l1), Vector(l2) -> List.for_all2 sexpr_eq l1 l2
  | _ -> false;;

module Reader: sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (Char.lowercase ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

let read_sexpr string = raise X_not_yet_implemented ;;

let read_sexprs string = raise X_not_yet_implemented;;
  
end;; (* struct Reader *)
