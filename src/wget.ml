(*
 * Copyright (C) 2012 Thomas Gazagnaire <thomas@ocamlpro.com>
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

open Unix

let get ~hostname ~src ~dst =
  let host =  (gethostbyname hostname).h_addr_list.(0) in
  let addr = ADDR_INET (host, 80) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  Unix.connect socket addr;

  if Sys.file_exists dst then
    Unix.unlink dst;
  let fd_out =
    openfile dst [O_WRONLY; O_NONBLOCK; O_APPEND; O_CREAT; O_TRUNC] 0o640 in

  let request = Http.Request.make ~user_agent:"OWGET" Http.Get src in

  let w = Http_client.rpc socket request (fun _ fd_in -> Unixext.copy_file fd_in fd_out) in

  Printf.printf "Download complete ! (%Ld bytes)\n%!" w;





















