(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Extended version of Set, Map and Hashtbl functors *)

module type PrintableHashOrdered = sig
  type t
  val compare : t -> t -> int
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end

module type ExtMap = sig
  module M : PrintableHashOrdered
  include Map.S with type key = M.t
                 and type 'a t = 'a Map.Make(M).t
  val map_option : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val of_list : (key * 'a) list -> 'a t
  val disjoint_union : 'a t -> 'a t -> 'a t
  val last_union : 'a t -> 'a t -> 'a t
  val rename : key t -> key -> key
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type ExtSet = sig
  module M : PrintableHashOrdered
  include Set.S with type elt = M.t
                 and type t = Set.Make(M).t
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
end

module type ExtHashtbl = sig
  module M : PrintableHashOrdered
  include Hashtbl.S with type key = M.t
                     and type 'a t = 'a Hashtbl.Make(M).t
  val to_map : 'a t -> 'a Map.Make(M).t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
end

module ExtMap :
  functor (M : PrintableHashOrdered) -> ExtMap with module M := M

module ExtSet :
  functor (M : PrintableHashOrdered) -> ExtSet with module M := M

module ExtHashtbl :
  functor (M : PrintableHashOrdered) -> ExtHashtbl with module M := M

module type Empty = sig end

module type BaseId =
  sig
    type t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val name : t -> string option
    val to_string : t -> string
    val output : out_channel -> t -> unit
    val print : Format.formatter -> t -> unit
  end

module type Id =
  sig
    type t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val name : t -> string option
    val to_string : t -> string
    val output : out_channel -> t -> unit
    val print : Format.formatter -> t -> unit
    val create : ?name:string -> unit -> t
  end

module type UnitId =
  sig
    type t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val name : t -> string option
    val to_string : t -> string
    val output : out_channel -> t -> unit
    val print : Format.formatter -> t -> unit
    val create : ?name:string -> string -> t
    val unit : t -> string
  end

module Id : functor (E : Empty) -> Id
module UnitId : functor (Id : Id) -> UnitId

module Int :
  sig
    type t = int
    val compare : int -> int -> int
    val output : out_channel -> int -> unit
    val hash : 'a -> 'a
    val equal : int -> int -> bool
    val print : Format.formatter -> int -> unit
  end

module IntSet : ExtSet with module M := Int
module IntMap : ExtMap with module M := Int
module IntTbl : ExtHashtbl with module M := Int

module StringSet : Set.S
  with type elt = string
   and type t = Set.Make(String).t

module StringMap : Map.S
  with type key = string
   and type 'a t = 'a Map.Make(String).t
