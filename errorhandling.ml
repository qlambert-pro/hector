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

open Common

let defined_alloc id =
  if id =~ "__dev_get_by_name" then true
  else if id =~ "wiphy_to_dev" then true
  else if id =~ "container_of" then true
  else if id =~ "i2c_get_adapdata" then true
  else if id =~ "snd_lookup_oss_minor_data" then true
  else if id =~ "snd_lookup_minor_data" then true
  else if id =~ "pci_get_drvdata" then true
  else if id =~ "dev_get_by_name" then true
  else if id =~ "l2cap_load" then true
  else if id =~ "platform_get_irq" then true
  else if id =~ "wpan_phy_find" then true
  else if id =~ "nla_nest_start" then true
  else if id =~ "sx_init_drivers" then true
  else if id =~ "clk_get" then true
  else if id =~ "irq_of_parse_and_map" then true
  else if id =~ "netdev_priv" then true
  else false
