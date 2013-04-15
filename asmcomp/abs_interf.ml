module type TypeSet = sig
  type t
  type set
  val singleton : t -> set
  val union : set -> set -> set
end

module type Domain = sig

  type ctx
  type values

  (* asking *)

  type 'a answer =
    { known : 'a; (* all known values matching the request *)
      has_unknown : bool; (* does it have unknown values matching the request *)
      has_other : bool; (* are there values not matching the request *) }

  type partial_parameters =
    | Known of values list
    | Unknown of values

  type func =
    { func : Ident.t;
      (* closure : ClosureId.t; usefull ? *)
      closure : values IdentMap.t;
      partial_application: partial_parameters }

  type block =
    { tag : int;
      fields : values list;
      mut : bool } (* mutable *)

  module BoolSet : TypeSet with type t = bool
  module IntSet : TypeSet with type t = int
  module FloatSet : TypeSet with type t = float
  module FloatArraySet : TypeSet with type t = float array
  module BoxedintSet : TypeSet with type t = Int64.t
  module StringSet : Typeset with type t = string

  module FunSet : Typeset with type t = func
  module BlockSet : Typeset with type t = block

  val functions : ctx -> values -> FunSet.t answer
  val block : ctx -> values -> BlockSet.t answer

  val integers : ctx -> values -> IntSet.set answer
  val float : ctx -> values -> IntSet.set answer
  val float : ctx -> values -> IntSet.set answer
  val bool : ctx -> values -> BoolSet.set answer

  type boxed_int_answer :
      { int32 : BoxedintSet.t answer;
        int64 : BoxedintSet.t answer;
        nativeint : BoxedintSet.t answer }

  val boxedint : ctx -> values -> boxed_int_answer


  (* building *)

  type op =
    | Afield : int * values
    | Amakeblock : int * values list

  val apply : ctx -> op -> values

end
