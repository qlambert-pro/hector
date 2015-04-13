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


# 0 "./objet.mli"
class virtual objet :
object('o)
  method invariant: unit -> unit
  (* method check: unit -> unit *)

  method of_string: string -> unit
  method to_string: unit -> string
  method debug: unit -> unit

  (* ugly (but convenient): those methods allow to extend an interface without
   * changing its interface. For instance in oassocbtree I want to
   * provide a method to commit, but doing so will mean break the interface
   * of oassoc. But if provide the commit code via a misc_op_hook, then
   * I will not break the interface.
   *)
  method misc_op_hook: unit -> 'o
  method misc_op_hook2: unit
end
