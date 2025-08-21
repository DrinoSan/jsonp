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

let rec tokenize input pos = Printf.printf "Upsi"
