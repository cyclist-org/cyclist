open Lib

module Make (T : Utilsigs.BasicType) = struct
  module MSet = Listmultiset.Make (T)
  include MSet
  include Fixpoint.Make (MSet)
end
