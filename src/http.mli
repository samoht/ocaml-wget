(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Recognised HTTP methods *)
type method_t = Get | Post | Put | Connect | Unknown of string
val string_of_method_t : method_t -> string

(** Exception raised when parsing start line of request *)
exception Http_parse_failure
exception Unauthorised of string
exception Forbidden

type authorization = 
    | Basic of string * string
    | UnknownAuth of string

(** Parsed form of the HTTP request line plus cookie info *)
module Request : sig
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

  val empty: t

  (** [make] is the standard constructor for [t] *)
  val make:
    ?version:string ->
    ?keep_alive:bool ->
    ?cookie:(string*string) list ->
    ?length:int64 ->
    ?body:string ->
    ?headers:(string*string) list ->
    ?content_type:string ->
    user_agent:string ->
    method_t ->
    string ->
    t

  (** [get_version t] returns the HTTP protocol version *)
  val get_version: t -> string

  (** [of_request_line l] parses [l] of the form "METHOD HTTP/VERSION" and
      returns the corresponding [t] *)
  val of_request_line: string -> t

  (** [to_string t] returns a short string summarising [t] *)
  val to_string: t -> string

  (** [to_header_list t] returns the list of HTTP headers associated
      with [t] *)
  val to_header_list: t -> string list

  (** [to_wire_string t] returns a string which could be sent to a server *)
  val to_wire_string: t -> string
end

(** Parsed form of the HTTP response *)
module Response : sig
  type t = {
    version: string;
    code: string;
    message: string;
    content_length: int64 option;
    additional_headers: (string*string) list;
    body: string option
  }

  (** Returns an instance of type t *)
  val make:
    ?version:string ->
    ?length:int64 ->
    ?headers:(string*string) list ->
    ?body:string ->
    code:string ->
    string ->
    t

  val internal_error: t

  (** [to_string t] returns a short string summarising [t] *)
  val to_string: t -> string

  (** [to_header_list t] returns the list of HTTP headers associated
      with [t] *)
  val to_header_list: t -> string list

  (** [to_wire_string t] returns a string which could be sent to a client *)
  val to_wire_string: t -> string
end

val authorization_of_string : string -> authorization

module Hdr : sig
  (** Header used for User-Agent string *)
  val user_agent: string
  val content_type: string
  val content_length: string
  val cookie: string
  val transfer_encoding: string
  val authorization: string
  val connection: string
end

val urlencode : string -> string

(** For transfer-encoding: chunked *)

type 'a ll = End | Item of 'a * (unit -> 'a ll)
val ll_iter : ('a -> unit) -> 'a ll -> unit
