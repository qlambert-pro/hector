(*
 * Copyright 2013, Inria
 * Suman Saha, Julia Lawall, Gilles Muller
 * This file is part of Hector.
 *
 * Hector is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Hector is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Hector.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Hector under other licenses.
 *)


# 0 "./oarray.mli"
(* !!take care!!, this is not a pure data structure *)

class ['a] oarray : int -> 'a ->
object ('o)
  inherit ['a] Osequence.osequence

  (* ocollection concrete instantiation of virtual methods *)
  method empty : 'o
  method add : (int * 'a) -> 'o

  method iter : (int * 'a -> unit) -> unit
  method view : (int * 'a, 'o) Ocollection.view

  method del : (int * 'a) -> 'o
  method mem : int * 'a -> bool
  method null : bool


  (* oassoc concrete instantiation of virtual methods *)
  method assoc : int -> 'a
  method delkey : int -> 'o

  method keys: int list

  (* osequence concrete instantiation of virtual methods *)
  method first : 'a
  method last : 'a
  method nth : int -> 'a

end
