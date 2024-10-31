open Base
open Option

type token = Def of string list | Op of string list | Nop

let tokenize s =
  let open String in
  let s = lowercase s in
  match (is_prefix s ~prefix:": ", is_suffix s ~suffix:" ;") with
  | true, true -> Def (sub ~pos:2 ~len:(length s - 4) s |> split ~on:' ')
  | false, false -> Op (split s ~on:' ')
  | _ -> Nop

module StringMap = Map.M (String)

type stack_fn_type = int list -> int list option
type env_type = stack_fn_type StringMap.t

let is_nb = String.for_all ~f:Char.is_digit

let calculate_op env stk op =
  match is_nb op with
  | true -> stk >>= fun stk' -> Some (Int.of_string op :: stk')
  | false -> Map.find env op >>= fun fn -> stk >>= fn

let get_fn (env : env_type) (args : string list) : stack_fn_type =
  let fn_list =
    List.map args ~f:(fun tkn ->
        match is_nb tkn with
        | true -> fun stk -> Some (Int.of_string tkn :: stk)
        | false -> value (Map.find env tkn) ~default:(Fn.const None))
  in
  fun stk -> List.fold fn_list ~init:(Some stk) ~f:( >>= )

let process (env, stk) tkn =
  match tkn with
  | Def (key :: args) -> (
      match is_nb key with
      | true -> Error "cannot redefine numbers"
      | false ->
          Ok
            ( Map.update env key ~f:(function None | Some _ -> get_fn env args),
              stk ))
  | Op op ->
      value_map
        (List.fold op ~init:stk ~f:(calculate_op env))
        ~default:(Error "Wrong operation")
        ~f:(fun stk -> Ok (env, Some stk))
  | _ -> Error "Unrecognized token"

let builtin_env =
  [
    ("+", function f :: s :: t -> Some ((s + f) :: t) | _ -> None);
    ("-", function f :: s :: t -> Some ((s - f) :: t) | _ -> None);
    ("/", function f :: s :: t when f <> 0 -> Some ((s / f) :: t) | _ -> None);
    ("*", function f :: s :: t -> Some ((s * f) :: t) | _ -> None);
    ("swap", function f :: s :: t -> Some (s :: f :: t) | _ -> None);
    ("over", function f :: s :: t -> Some (s :: f :: s :: t) | _ -> None);
    ("dup", function f :: t -> Some (f :: f :: t) | _ -> None);
    ("drop", function _ :: t -> Some t | _ -> None);
  ]

let evaluate l =
  let open List in
  let env = Map.of_alist_exn (module String) builtin_env in
  match map l ~f:tokenize |> fold_result ~init:(env, Some []) ~f:process with
  | Error _ -> None
  | Ok (_, s) -> s |> Option.bind ~f:(fun l -> Some (rev l))
