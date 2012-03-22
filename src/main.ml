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

open Wget

let _ =
  if Array.length Sys.argv <> 3 then begin
    Printf.eprintf "usage: owget host file\n%!";
    exit 1
  end else
    get Sys.argv.(1) Sys.argv.(2) (Filename.basename Sys.argv.(2))




















