
open Common

type 'a t = {view: 'a; loc: Loc.t}

let view self = self.view
let loc self = self.loc
let pp ppx out (self:_ t) =
  Fmt.fprintf out "%a@ %a" Loc.pp self.loc ppx self.view

let make ~loc view : _ t = {view; loc}
