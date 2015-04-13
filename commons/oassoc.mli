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


# 0 "./oassoc.mli"

class virtual ['a, 'b] oassoc :
object ('o)
  inherit ['a * 'b] Ocollection.ocollection

  method virtual assoc : 'a -> 'b
  method virtual delkey : 'a -> 'o

  (* may raise NotFound *)
  method find : 'a -> 'b
  method find_opt: 'a -> 'b option

  method haskey : 'a -> bool
  method replkey : 'a * 'b -> 'o

  (* better to implement it yourself *)
  method virtual keys: 'a list

  method apply : 'a -> ('b -> 'b) -> 'o
  method apply_with_default : 'a -> ('b -> 'b) -> (unit -> 'b) -> 'o

  (* effect version *)
  method apply_with_default2 : 'a -> ('b -> 'b) -> (unit -> 'b) -> unit

end
