type json =
  | JObject of (string * json) list
  | JArray of json list
  | JString of string
  | JNumber of int
  | JBool of bool
  | JNull

type token =
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Colon
  | Comma
  | String of string
  | Number of int
  | True
  | False
  | Null

type person = {
   name: string;
   age: int;
   active: bool;
   data: json list;
}

(* Needed to be ablue to retrieve different types *)
type field_value =
  | StringVal of string
  | IntVal of int
  | BoolVal of bool
  | ArrayVal of json list

let rec tokenize input pos =
  if pos >= String.length input then []
  else
     match input.[pos] with
       | ' ' | '\n' | '\t' | '\r' -> tokenize input (pos + 1) (* skip that whitespace baby *)
       | '{' -> LBrace :: tokenize input (pos + 1)
       | '}' -> RBrace :: tokenize input (pos + 1)
       | '[' -> LBracket :: tokenize input (pos + 1)
       | ']' -> RBracket :: tokenize input (pos + 1)
       | ':' -> Colon :: tokenize input (pos + 1)
       | ',' -> Comma :: tokenize input (pos + 1)
       | '"' -> tokenize_string input (pos + 1) []
       | 't' when String.sub input pos 4 = "true" -> True::tokenize input (pos + 4)
       | 'f' when String.sub input pos 5 = "false" -> False::tokenize input (pos + 5)
       | c when c >= '0' && c <= '9' -> tokenize_number input pos []
       | _ -> failwith ("Unexpected character: " ^ String.make 1 input.[pos])

and tokenize_string input pos acc =
  if pos >= String.length input then failwith "Unterminated string"
  else
   match input.[pos] with
   | '"' -> String (String.concat "" (List.rev acc)) :: tokenize input (pos + 1)
   | c -> tokenize_string input (pos + 1) (String.make 1 c :: acc )

and tokenize_number input pos acc =
  if pos >= String.length input || not (String.contains "0123456789" input.[pos]) then
    let num_str = String.concat "" (List.rev acc) in
    Number (int_of_string num_str) :: tokenize input pos
  else
    tokenize_number input (pos + 1) (String.make 1 input.[pos] :: acc)

let rec parse tokens =
  match tokens with
  | [] -> failwith "Unexpected end of input"
  | t :: rest ->
      match t with
      | LBrace -> parse_object rest [] (* Entry *)
      | LBracket -> parse_array rest []
      | String s -> (JString s, rest)
      | Number n -> (JNumber n, rest)
      | True -> (JBool true, rest)
      | False -> (JBool false, rest)
      | Null -> (JNull, rest)
      | _ -> failwith "Invalid JSON"

and parse_object tokens acc =
  match tokens with
  | RBrace :: rest -> (JObject (List.rev acc), rest) (* Last point *)
  | String key :: Colon :: rest ->
      let (value, rest') = parse rest in
      parse_object rest' ((key, value) :: acc )
  | Comma :: rest -> parse_object rest acc
  | _ -> failwith "Invalid Object"

and parse_array tokens acc =
  match tokens with
  | RBracket :: rest -> (JArray (List.rev acc), rest)
  | Comma :: rest -> parse_array rest acc
  | _ -> let (value, rest') = parse tokens in
         parse_array rest' (value :: acc)

let parse_json input =
   let tokens = tokenize input 0 in
   let (json, rest) = parse tokens in
   if rest <> [] then failwith "Extra tokens after JSON"
   else json

let json_to_person json =
  match json with
  | JObject fields ->
    let find_field key expected_type =
      try
        let ( _, value ) = List.find (fun (k, _ ) -> k = key) fields in
        match value, expected_type with
        | JString s, `String -> StringVal s
        | JNumber n, `Int -> IntVal n
        | JBool b, `Bool -> BoolVal b
        | JArray a, `Array -> ArrayVal a
        | _ -> failwith ("Invalid type for field " ^ key)
      with
      | Not_found -> failwith ("Missing field " ^ key)
      in
      let unwrap_string = function StringVal s -> s | _ -> failwith "Expected string" in
      let unwrap_int = function IntVal n -> n | _ -> failwith "Expected int" in
      let unwrap_bool = function BoolVal b -> b | _ -> failwith "Expected bool" in
      let unwrap_array = function ArrayVal a -> a | _ -> failwith "Expected array" in
      {
        name = unwrap_string (find_field "name" `String);
        age = unwrap_int (find_field "age" `Int);
        active = unwrap_bool (find_field "active" `Bool);
        data = unwrap_array (find_field "data" `Array);
      }
  | _ -> failwith "Expected a JSON object"


let print_person p =
  Printf.printf "Name: %s\nAge: %d\nActive: %b\nData: [" p.name p.age p.active;
    List.iter (fun v ->
      match v with
      | JString s -> Printf.printf "%s, " s
      | JNumber n -> Printf.printf "%d, " n
      | _ -> Printf.printf "other, ") p.data;
    Printf.printf "]\n"

let print_json_obj json =
  match json with
  | JObject fields ->
      List.iter (fun (k, v) ->
        match v with
        | JString s -> Printf.printf "%s: %s\n" k s
        | JNumber n -> Printf.printf "%s: %d\n" k n
        | JBool b -> Printf.printf "%s: %b\n" k b
        | JArray a ->
              List.iter (fun elem ->
                match elem with
                | JString s -> Printf.printf "%s: [%s]\n" k s
                | JNumber n -> Printf.printf "%s: [%d]\n" k n
                | JBool b -> Printf.printf "%s: [%b]\n" k b
                | JNull -> Printf.printf "%s: [null]\n" k
                | _ -> ()) a
        | _ -> ()) fields;
  | _ -> ();
  flush stdout

let () =
  let input = "{\"name\": \"Alice\", \"age\": 30, \"active\": true, \"data\": [1,3,4,5,\"sandri\", \"blub\"]}" in
  let json = parse_json input in
  Printf.printf "Casting Json into Person p\n---------\n";
  let p = json_to_person json in
  print_person p;
  Printf.printf "---------\n";
  print_json_obj json;
