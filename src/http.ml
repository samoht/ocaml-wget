(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)


open Stringext

exception Http_parse_failure
exception Unauthorised of string
exception Forbidden
exception Malformed_url of string

module Hdr = struct
  let content_type = "Content-Type"
  let content_length = "Content-Length"
  let user_agent = "User-Agent"
  let cookie = "Cookie"
  let transfer_encoding = "Transfer-encoding"
  let authorization = "Authorization"
  let connection = "Connection"
end

let output_http fd headers =
  Unixext.really_write_string fd
    (String.concat "" (List.map (fun x -> x ^ "\r\n") headers))

let urldecode url =
  let chars = String.explode url in
  let rec fn ac = function
  |'+'::tl -> fn (' ' :: ac) tl
  |'%'::a::b::tl ->
      let cs =
        try int_of_string (String.implode ['0';'x';a;b])
        with _ -> raise (Malformed_url url) in
      fn (Char.chr cs :: ac) tl
  |x::tl -> fn (x :: ac) tl
  |[] ->
      String.implode (List.rev ac)
  in fn [] chars

(* Encode @param suitably for appearing in a query parameter in a
   URL. *)
let urlencode param =
  let chars = String.explode param in
  let rec fn = function
  | x::tl ->
      begin
        let s =
          if x = ' ' then "+"
          else match x with
          | 'A'..'Z'
          | 'a'..'z'
          | '0'..'9'
          | '$' | '-' | '_' | '.' | '!'
          | '*' | '\'' | '(' | ')' | ',' ->
              String.of_char x
          | _ ->
              Printf.sprintf "%%%2x" (Char.code x)
        in
        s ^ fn tl
      end
  | [] ->
      ""
  in fn chars

(** Parses strings of the form a=b&c=d into ["a", "b"; "c", "d"] *)
let parse_keyvalpairs xs = 
  let kvpairs = List.map (String.split '=') (String.split '&' xs) in
  List.map (function
	    | k :: vs -> ((urldecode k), urldecode (String.concat "=" vs))
	    | [] -> raise Http_parse_failure) kvpairs

let parse_uri x = match String.split '?' x with
| [ uri ] -> uri, []
| [ uri; params ] -> uri, parse_keyvalpairs params
| _ -> raise Http_parse_failure


type authorization =
  | Basic of string * string
  | UnknownAuth of string

let authorization_of_string x =
  let basic = "Basic " in
  if String.startswith basic x
  then
    let end_of_string s from =
      String.sub s from ((String.length s)-from) in
    let userpass = Base64.decode (end_of_string x (String.length basic)) in
    match String.split ':' userpass with
    | [ username; password ] -> Basic(username, password)
    | _ -> UnknownAuth x
  else UnknownAuth x

let string_of_authorization = function
| UnknownAuth x -> x
| Basic(username, password) -> "Basic " ^ (Base64.encode (username ^ ":" ^ password))

type method_t = Get | Post | Put | Connect | Unknown of string

let string_of_method_t = function
  | Get -> "GET" | Post -> "POST" | Put -> "PUT" | Connect -> "CONNECT" | Unknown x -> "Unknown " ^ x
let method_t_of_string = function
  | "GET" -> Get | "POST" -> Post | "PUT" -> Put | "CONNECT" -> Connect | x -> Unknown x

module Request = struct
  type t = {
    m: method_t;
    uri: string;
    query: (string*string) list;
    version: string;
    transfer_encoding: string option;
    content_length: int64 option;
    auth: authorization option;
    cookie: (string * string) list;
    content_type: string option;
    user_agent: string option;
    mutable close: bool;
    additional_headers: (string*string) list;
    body: string option;
  }

  let empty = {
    m=Unknown "";
    uri="";
    query=[];
    version="";
    transfer_encoding=None;
    content_length=None;
    auth=None;
    cookie=[];
    content_type = None;
    user_agent = None;
    close= true;
    additional_headers=[];
    body = None;
  }

  let make 
      ?(version="1.0") ?(keep_alive=false) ?cookie ?length ?body
      ?(headers=[]) ?content_type ~user_agent meth path = 
    { empty with
      version = version;
      close = not keep_alive;
      cookie = (match cookie with None -> [] | Some c -> c);
      content_length = length;
      content_type = content_type;
      user_agent = Some user_agent;
      m = meth;
      uri = path;
      additional_headers = headers;
      body = body;
    }

  let get_version x = x.version

  let of_request_line x = match String.split_f String.isspace x with
  | [ m; uri; version ] ->
      (* Request-Line   = Method SP Request-URI SP HTTP-Version CRLF *)
      let uri, query = parse_uri uri in
      { m = method_t_of_string m; uri = uri; query = query;
	content_length = None; transfer_encoding = None;
	version = version; cookie = []; auth = None;
	content_type = None; user_agent = None;
	close=false; additional_headers=[]; body = None }
  | _ -> raise Http_parse_failure

  let to_string x =
    let kvpairs x = String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x) in
    Printf.sprintf "{ method = %s; uri = %s; \
                      query = [ %s ]; \
                      content_length = [ %s ]; \
                      transfer encoding = %s; \
                      version = %s; \
                      cookie = [ %s ]; \
                      content-type = %s; \
                      user_agent = %s }" 
      (string_of_method_t x.m) x.uri
      (kvpairs x.query)
      (match x.content_length with None -> "" | Some n -> Int64.to_string n)
      (match x.transfer_encoding with None -> "" | Some s -> s)
      x.version
      (kvpairs x.cookie)
      (match x.content_type with None -> "" | Some c -> c)
      (match x.user_agent with None -> "" | Some c -> c)

  let to_header_list x =
    let kvpairs x = String.concat "&" (List.map (fun (k, v) -> urlencode k ^ "=" ^ (urlencode v)) x) in
    let query = if x.query = [] then "" else "?" ^ (kvpairs x.query) in
    let cookie = if x.cookie = [] then [] else [ Hdr.cookie ^": " ^ (kvpairs x.cookie) ] in
    let transfer_encoding =
      match x.transfer_encoding with
      | None   -> []
      | Some x -> [ Hdr.transfer_encoding ^": " ^ x] in
    let content_length =
      match x.content_length with
      | None   -> []
      | Some x -> [ Printf.sprintf "%s: %Ld" Hdr.content_length x ] in
    let auth =
      match x.auth with
      | None   -> []
      | Some x -> [ Hdr.authorization ^": " ^ (string_of_authorization x) ] in
    let content_type =
      match x.content_type with
      | None   -> []
      | Some x -> [ Hdr.content_type ^": " ^ x ] in
    let user_agent =
      match x.user_agent with
      | None   -> []
      | Some x -> [ Hdr.user_agent^": " ^ x ] in
    let close = [ Hdr.connection ^": " ^ (if x.close then "close" else "keep-alive") ] in
    [ Printf.sprintf "%s %s%s HTTP/%s" (string_of_method_t x.m) x.uri query x.version ]
    @ cookie @ transfer_encoding @ content_length @ auth @ content_type @ user_agent @ close
    @ (List.map (fun (k, v) -> k ^ ":" ^ v) x.additional_headers)

  let to_wire_string (x: t) =
    (* If the body is given then compute a content length *)
    let x = match x.body with
    | None -> x
    | Some b -> { x with content_length = Some (Int64.of_int (String.length b)) } in
    let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") (to_header_list x @ [""])) in
    let body = match x.body with None -> "" | Some x -> x in
    headers ^ body

end

module Response = struct
  type t = {
    version: string;
    code: string;
    message: string;
    content_length: int64 option;
    additional_headers: (string*string) list;
    body: string option;
  }
  let to_string x =
    let kvpairs x = String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x) in
    Printf.sprintf "{ version = %s; \
                      code = %s; \
                      message = %s; \
                      content_length = %s; \
                      additional_headers = [ %s ] }"
      x.version x.code x.message
      (match x.content_length with
      | None   -> "None"
      | Some x -> "Some " ^ (Int64.to_string x))
      (kvpairs x.additional_headers)

  let empty = {
    version = "1.0";
    code = "500";
    message = "Unknown error message";
    content_length = None;
    additional_headers = [];
    body = None;
  }

  let make ?(version="1.0") ?length ?(headers=[]) ?body ~code message = {
    version = version;
    code = code;
    message = message;
    content_length = length;
    additional_headers = headers;
    body = body
  }

  let internal_error = {
    empty with
      code = "500";
      message = "internal error";
      content_length = Some 0L }

  let to_header_list (x: t) =
    let status = Printf.sprintf "HTTP/%s %s %s" x.version x.code x.message in
    let content_length =
      match x.content_length with
      | None   ->[]
      | Some x ->[ Printf.sprintf "%s: %Ld" Hdr.content_length x ] in
    let headers = List.map (fun (k, v) -> k ^ ":" ^ v) x.additional_headers in
    status :: (content_length @ headers)

  let to_wire_string (x: t) =
    (* If the body is given then compute a content length *)
    let x = match x.body with
    | None -> x
    | Some b -> { x with content_length = Some (Int64.of_int (String.length b)) } in
    let headers = String.concat "" (List.map (fun x -> x ^ "\r\n") (to_header_list x @ [""])) in
    let body = match x.body with None -> "" | Some x -> x in
    headers ^ body
end


(* For transfer-encoding: chunked *)

type 'a ll = End | Item of 'a * (unit -> 'a ll)

let rec ll_iter f = function
  | End -> ()
  | Item (x, xs) -> f x; ll_iter f (xs ())









