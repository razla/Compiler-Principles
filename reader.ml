
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


let make_enclosed _l_ _p_ _r_ =
  let _enclosed_ = PC.caten (PC.caten _l_ _p_) _r_ in
  PC.pack _enclosed_ (fun ((l, p), r) -> p);;
let _space_ = PC.char ' ';;

let _whiteSpaces_ = PC.pack (PC.star (PC.nt_whitespace)) (fun (a) -> Nil);;

(*** NEED TO CHECK ABOUT ENF OF INPUT ***)

let _comments_ = 
	let _semicolon_ = PC.char ';' in
	let _smartComment1_ = PC.diff (PC.range (Char.chr 0) (Char.chr 127)) (PC.char (Char.chr 10)) in
	let _smartComments2_ = PC.caten (PC.caten _semicolon_ (PC.star _smartComment1_)) (PC.maybe (PC.char (Char.chr 10))) in
	PC.pack _smartComments2_ (fun ((a,b), c) -> Nil);;

let make_spaced _p_ = 
  let _st_space_ = PC.star _space_ in
  make_enclosed _st_space_ _p_ _st_space_;;


  

let _tripleDot_ = make_spaced (PC.word"...");;(*to chack pack*)
let _lparen_ = make_spaced (PC.char '(');;
let _rparen_ = make_spaced (PC.char ')');; 
let _lparenBracket_ = make_spaced (PC.char '[');;
let _rparenBracket_ = make_spaced (PC.char ']');; 
let _mulop_ = make_spaced (PC.char '*');;
let _addop_ = make_spaced (PC.char '+');;  


let _boolean_ =
	let _true_ = PC.char_ci 't' in
	let _hash_ = PC.char '#' in
	let _false_ = PC.char_ci 'f' in
  	let _bool_ = PC.not_followed_by (PC.caten _hash_ (PC.disj _true_ _false_)) _symbol_ in
  	PC.pack _bool_ (fun ((a,b)) -> Bool(b = 't' || b = 'T'));;

let _namedChar_ = 
	let _newline_ = 
		let _newlines_ = PC.word_ci "newline" in
		PC.pack _newlines_ (fun (s) -> ('\n')) in
	let _page_ = 
		let _pages_ = PC.word_ci "page" in
		PC.pack _pages_ (fun (s) -> Char.chr 12) in
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
		PC.disj_list ([_page_;_newline_;_nul_;_return_;_space_;_tab_]);;

let _hexDigit_ = 
	let _digit_ = PC.range '0' '9' in
	let _aTOf_ = PC.range 'a' 'f' in
	let _ATOF_ = PC.range 'A' 'F' in
	PC.disj (PC.disj _digit_ _aTOf_) _ATOF_;;

let _hexChar_ = 
	let _x_ = PC.char_ci 'x' in
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
	let _chars_ = PC.caten _charPrefix_ (PC.disj_list([_hexChar_;_namedChar_;_visibleSimpleChar_;])) in
	PC.pack _chars_ (fun ((a,b),c) -> Char(c));;


let _stringMetaChar_ =
	let _slash_ =
		let _slashs_ = PC.caten (PC.char '\\') (PC.char '\\') in
		PC.pack _slashs_ (fun (a,b) -> '\\') in
	let _page_ = 
		let _pages_ = PC.caten (PC.char '\\') (PC.char 'f') in
		PC.pack _pages_ (fun (a,b) -> Char.chr 12) in
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
	 	PC.disj_list ([_page_;_slash_;_tab_;_merchaot_;_newline_;_r_]);;

let _stringHexChar_ = 
	let _hexNatural_ = PC.plus _hexDigit_ in
	let _slashX_ = PC.caten (PC.char '\\') (PC.char_ci 'x') in
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

let _string_ = 
	let _doubleQuote_ = PC.char '\"' in
	let _strings_ = PC.caten (PC.caten _doubleQuote_ (PC.star _stringChar_)) _doubleQuote_ in
	PC.pack _strings_ (fun ((a,b) , c) -> String(list_to_string (b)));;


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

(* NEED TO FIX IT *)

let _float_ =
	let _integerHelper_ = 
		let _digit_ = PC.range '0' '9' in
		let _natural_ = PC.plus _digit_ in
		let _dot_ = PC.char '.' in
		let _plusOrMinus_ = PC.maybe (PC.disj (PC.char '+') (PC.char '-')) in
  		let _int_ = PC.caten (PC.caten _plusOrMinus_ _natural_) _dot_ in
  		PC.caten _int_ _natural_ in
  	PC.pack _integerHelper_ (fun (((sign,nat),dot),nat2) ->
		  	let afterDot = float_of_string (list_to_string nat2) *. 10.0**(float_of_int ((-1)*String.length (list_to_string nat2))) in
		  		match sign with
		  	| Some('-') -> Float((-1.0)*.float_of_string (list_to_string nat) -. afterDot)
		  	| Some('+') -> Float(float_of_string (list_to_string nat) +. afterDot)
		  	| _ -> Float(float_of_string (list_to_string nat) +. afterDot));;


let _hexInteger_ = 
	let _plusOrMinus_ = PC.maybe (PC.disj (PC.char '+') (PC.char '-')) in
	let _hexPrefix_ = PC.word_ci "#x" in
	let _hexNatural_ = PC.plus _hexDigit_ in
	let _hexIntegers_ = PC.caten (PC.caten _hexPrefix_ _plusOrMinus_) _hexNatural_ in
	PC.pack _hexIntegers_ (fun ((a,sign),i) -> 
	  let hexNum = int_of_string (String.concat "" ["0x";(list_to_string i)]) in
	  match sign with
	  | Some('-') -> Int ((-1) * hexNum)
	  | Some('+') -> Int (hexNum)
  	  | _ -> Int (hexNum));;

let _hexFloat_ = 
	let _dot_ = PC.char '.' in
	let _hexNatural_ = PC.plus _hexDigit_ in
	let _hexIntegerLeft_ = 
			let _plusOrMinus_ = PC.maybe (PC.disj (PC.char '+') (PC.char '-')) in
			let _hexPrefix_ = PC.word_ci "#x" in
			let _hexNatural_ = PC.plus _hexDigit_ in
			let _hexIntegers_ = PC.caten (PC.caten _hexPrefix_ _plusOrMinus_) _hexNatural_ in
			PC.pack _hexIntegers_ (fun ((a,sign),i) -> 
				match sign with
				| Some('-') -> String.concat "" ["-";"0x";(list_to_string i)]
			   	| _ -> String.concat "" ["0x";(list_to_string i)]) in
	
	let _hexFloats_ = PC.caten (PC.caten _hexIntegerLeft_ _dot_) _hexNatural_ in 
	PC.pack _hexFloats_ (fun ((a,b),c) ->
  		let afterDot =  float_of_string (String.concat "" ["0x0.";(list_to_string c)]) in
  		Float(if float_of_string a > 0.0 then float_of_string a +. afterDot
  		else float_of_string a -. afterDot))


let _scientificNotation_ = 
	let _begin_ = PC.disj _float_ _integer_  in
	let _beginAndE_ = PC.caten _begin_ (PC.char_ci 'e') in 
	let _ready4pack_ = PC.caten _beginAndE_ _integer_ in
	PC.pack _ready4pack_ (fun ((a,b), c) -> 
	match a with
	  | (Int x_int1) -> (
		match c with  
			| (Int x_int2) -> Float(float_of_int(x_int1) *. 10.0**(float_of_int x_int2))
			| _ -> Float(0.0))
	  | (Float x_float1) -> (
		match c with 
			| (Int x_int3) -> Float (x_float1 *. 10.0 **(float_of_int x_int3))
			| _ -> Float(0.0) ));;

let _number_ = 
	let _numbers_ = PC.disj _scientificNotation_ (PC.disj (PC.disj (PC.disj _hexFloat_ _float_) _hexInteger_) _integer_) in
	PC.pack _numbers_ (fun (a) -> Number(a));;



let rec _list_ s=
        let make_list x y =
        	let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _whiteSpace_and_comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
			let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
	        let _smartSexpr_= PC.diff _sexpr_  x in 
	        let _smartSexprs_ = make_spacedAndAllComments (PC.star _smartSexpr_) in
	        let _ready4pack_= PC.caten y (PC.caten  _smartSexprs_ x) in
	        PC.pack (_ready4pack_) (fun (a,(b,c)) ->
	        										List.fold_right 
	                                                (fun sexp1 sexp2 -> 
	                                                	 Pair (sexp1, sexp2))
	                                                 b
	                                                 Nil) in
	    PC.disj (make_list _rparen_ _lparen_) (make_list _rparenBracket_ _lparenBracket_) s
	    

and _maybeList_ s = 
        let make_list x y =
        	let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _whiteSpace_and_comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
			let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
	        let _smartSexpr_= PC.diff _maybeSexpr_  x in 
	        let _smartSexprs_ = make_spacedAndAllComments (PC.star _smartSexpr_) in
	        let _ready4pack_= PC.caten y (PC.caten  _smartSexprs_ (PC.maybe x)) in
	        PC.pack (_ready4pack_) (fun (a,(b,c)) ->
	        										List.fold_right 
	                                                (fun sexp1 sexp2 -> Pair (sexp1, sexp2))
	                                                 b
	                                                 Nil) in
	    PC.disj (make_list _rparen_  _lparen_) (make_list  _rparenBracket_ _lparenBracket_) s
	    
and _whiteSpace_and_comments_ s=

        let _catenSpacesComments_= PC.pack (PC.star(PC.caten _whiteSpaces_ (PC.disj _commentsSexpr_ _comments_))) (fun (a)->Nil) in
	PC.pack (PC.caten  _catenSpacesComments_ _whiteSpaces_) (fun (a) -> Nil) s
	
and _sexpr_ s = (PC.disj _nonOptSexpr_ _optSexpr_) s

and _maybeSexpr_ s = (PC.disj _nonOptSexpr_ _maybeOptSexpr_) s

and _nonOptSexpr_ s =
		let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj_list [ _whiteSpace_and_comments_ ; _whiteSpaces_])) _whiteSpaces_) (fun (a,b)->Nil) in
		let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
        let _sexprs_=make_spacedAndAllComments (PC.disj_list ([_boolean_;_char_;_number_;_string_;_symbol_; _list_;_dottedList_;_vector_;_quote_;_quasiQuoted_;_Unquoted_;_UnquotedSpliced_])) in
        PC.pack _sexprs_ (fun a->a)  s

        
and _optSexpr_  s =
		let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _whiteSpace_and_comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
		let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
        let _sexprs_=PC.caten (make_spacedAndAllComments (PC.disj_list ([_boolean_;_char_;_number_;_string_;_symbol_;
        _maybeDottedList_;_maybeList_;_maybeVector_;_quote_;_quasiQuoted_;_Unquoted_;_UnquotedSpliced_]))) _tripleDot_ in
        PC.pack _sexprs_ (fun (a,b)->a) s
        
and _maybeOptSexpr_  s =
		let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
		let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
        let _sexprs_=make_spacedAndAllComments (PC.disj_list ([_boolean_;_char_;_number_;_string_;_symbol_;
        _maybeDottedList_;_maybeList_;_maybeVector_;_quote_;_quasiQuoted_;_Unquoted_;_UnquotedSpliced_])) in
        PC.pack _sexprs_ (fun (a)->a) s

and _commentsSexpr_ s = 
		PC.pack (PC.caten (PC.word "#;") _sexpr_) (fun (a,b) -> Nil) s


and _dottedList_ s = 
		let make_dottedList_ x y = 
			let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _whiteSpace_and_comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
			let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
			let _dot_ = PC.char '.' in
			let _smartSexprs1_ = PC.diff _sexpr_ _dot_ in
			let _smartSexprs2_ = PC.diff _sexpr_ x in
			let _smartSexprs3_ = make_spacedAndAllComments (PC.plus _smartSexprs1_) in 
			PC.pack (PC.caten (PC.caten (PC.caten (PC.caten y _smartSexprs3_) _dot_ ) _smartSexprs2_ ) x)
			(fun ((((a,b),c),d),e) -> 
				List.fold_right 
				(fun sexp1 sexp2 -> match sexp2 with 
									| Nil -> Pair(sexp1,d)
									| _ -> Pair (sexp1, sexp2))
				b
				Nil) in

PC.disj (make_dottedList_ _rparen_ _lparen_) (make_dottedList_ _rparenBracket_ _lparenBracket_) s


and _maybeDottedList_ s = 
		let make_dottedList_ x y = 
			let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _whiteSpace_and_comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
			let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
			let _dot_ = PC.char '.' in
			let _smartSexprs1_ = PC.diff _maybeSexpr_ _dot_ in
			let _smartSexprs2_ = PC.diff _maybeSexpr_ x in
			let _smartSexprs3_ = make_spacedAndAllComments (PC.plus _smartSexprs1_) in 
			PC.pack (PC.caten (PC.caten (PC.caten (PC.caten y _smartSexprs3_) _dot_ ) _smartSexprs2_ ) (PC.maybe x))
                        (fun ((((a,b),c),d),e) -> 
				List.fold_right 
				(fun sexp1 sexp2 -> match sexp2 with 
									| Nil -> Pair(sexp1,d)
									| _ -> Pair (sexp1, sexp2))
				b
				Nil) in

PC.disj (make_dottedList_ _rparen_  _lparen_) (make_dottedList_ _rparenBracket_  _lparenBracket_) s

and _vector_ s = 
		let make_vector x y = 
			let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _whiteSpace_and_comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
			let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
			let _hash_ = PC.char '#' in 
			let _smartSexprs1_ = PC.diff _sexpr_ x in 
			let _smartSexprs2_ = make_spacedAndAllComments (PC.star _smartSexprs1_) in
			PC.pack (PC.caten (PC.caten (PC.caten _hash_ y) _smartSexprs2_) x)
			(fun ((((a,b),c),d)) -> Vector (c)) in 
	PC.disj (make_vector _rparen_ _lparen_) (make_vector _rparenBracket_ _lparenBracket_) s

and _maybeVector_ s = 
                let make_vector x y = 
                	let _whiteSpacedAndAllComments_ = PC.pack (PC.caten (PC.caten _whiteSpaces_ (PC.disj (PC.disj _whiteSpace_and_comments_ _commentsSexpr_) _whiteSpaces_)) _whiteSpaces_) (fun (a,b)->Nil) in
			let make_spacedAndAllComments _p_ = make_enclosed _whiteSpacedAndAllComments_ _p_ _whiteSpacedAndAllComments_ in 
                    let _hash_ = PC.char '#' in 
                    let _smartSexprs1_ = PC.diff _optSexpr_ x in 
                    let _smartSexprs2_ = make_spacedAndAllComments (PC.star _smartSexprs1_) in
                    PC.pack (PC.caten (PC.caten (PC.caten _hash_ y) _smartSexprs2_) (PC.maybe x))
                    (fun ((((a,b),c),d)) -> Vector (c)) in 
	PC.disj (make_vector _rparen_  _lparen_) (make_vector _rparenBracket_  _lparenBracket_) s

and _quote_ s = 
		let _quotes_ = PC.char '\'' in 
		PC.pack (PC.caten _quotes_ _sexpr_) (fun (a, b)-> Pair(Symbol("quote"), Pair(b, Nil))) s 

and _quasiQuoted_ s = 
		let _quasiQuote_ = PC.char '`' in 
		PC.pack (PC.caten _quasiQuote_ _sexpr_) (fun (a, b)-> Pair(Symbol("quasiquote"), Pair(b, Nil))) s 

and _Unquoted_ s = 
		let _unquote_ = PC.char ',' in 
		PC.pack (PC.caten _unquote_ _sexpr_) (fun (a, b)-> Pair(Symbol("unquote"), Pair(b, Nil))) s

and _UnquotedSpliced_ s = 
		let _unquoteSpliced_ = PC.word ",@" in 
		PC.pack (PC.caten _unquoteSpliced_ _sexpr_) (fun (a, b)-> Pair(Symbol("unquote-splicing"), Pair(b, Nil))) s;;



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

module Reader : sig
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

let read_sexpr string = 
	let parser,b = (PC.not_followed_by _sexpr_ _sexpr_) (string_to_list string) in 
	parser;;

let read_sexprs string = 
	let parser,b = PC.star _sexpr_ (string_to_list string) in 
	parser;;
;;
  
end;; (* struct Reader *)
