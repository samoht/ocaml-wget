(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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

let really_write fd string off n =
  let written = ref 0 in
  while !written < n
  do
    let wr = Unix.write fd string (off + !written) (n - !written) in
    written := wr + !written
  done

let really_write_string fd string =
  really_write fd string 0 (String.length string)

let copy_file_internal ?limit reader writer =
  let buffer = String.make 65536 '\000' in
  let buffer_len = Int64.of_int (String.length buffer) in
  let finished = ref false in
  let total_bytes = ref 0L in
  let limit = ref limit in
  while not(!finished) do
    let requested =
      min (match !limit with None -> buffer_len | Some l -> l) buffer_len in
    let num = reader buffer 0 (Int64.to_int requested) in
    let num64 = Int64.of_int num in

    limit := (match !limit with None -> None | Some x -> Some (Int64.sub x num64));
    let (_ : int) = writer buffer 0 num in
    total_bytes := Int64.add !total_bytes num64;
    finished := num = 0 || !limit = Some 0L;
  done;
  !total_bytes

let copy_file ?limit ifd ofd =
  copy_file_internal ?limit (Unix.read ifd) (Unix.write ofd)













