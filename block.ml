(* **
 * Copyright 2013-2016, Inria
 * Suman Saha, Julia Lawall, Gilles Muller, Quentin Lambert
 * This file is part of Hector.

 * Hector is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.

 * Hector is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with Hector.  If not, see <http://www.gnu.org/licenses/>.

 * The authors reserve the right to distribute this or future versions of
 * Hector under other licenses.
 * *)

open Common

module GO = Graph_operations
module ACFG = Annotated_cfg
module R = Resource

let get_resource_release cfg block_head acc =
  GO.breadth_first_fold
    (GO.get_forward_config
       (fun _ _ -> true)
       (fun _ _ _ -> ())
       (fun _ (cn, _) res ->
          match cn.GO.node.Annotated_cfg.resource_handling_type with
            ACFG.Release r ->
            ({R.node = cn; R.resource = r}, block_head)::res
          | _         -> res)
       () acc)
    cfg block_head
