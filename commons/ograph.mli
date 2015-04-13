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


# 0 "./ograph.mli"
class virtual ['a] ograph :
object ('o)
  method virtual empty : 'o

  method virtual add_node : 'a -> 'o
  method virtual del_node : 'a -> 'o

  method virtual add_arc : 'a * 'a -> 'o
  method virtual del_arc : 'a * 'a -> 'o


  method virtual nodes : 'a Oset.oset
  method virtual predecessors : 'a -> 'a Oset.oset
  method virtual successors : 'a -> 'a Oset.oset

  method virtual ancestors : 'a Oset.oset -> 'a Oset.oset
  method virtual brothers : 'a -> 'a Oset.oset
  method virtual children : 'a Oset.oset -> 'a Oset.oset

  method mydebug : ('a * 'a list) list
end
