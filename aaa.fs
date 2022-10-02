  type Typ = O | Hom of Typ * Typ | Box of Typ

  /// Terms of the simply typed very parsimonious lambda calculus.
  type Term =
    | Var of string
    | LamAff of string * (Term -> Term)
    | LamBox of string * (Term -> Term)
    | App of Term * Term
    | Box of Term
    | Ann of Term * Typ
  
  /// Weak head normal form reduction.
  let rec reduce t =
    match t with
    | App (f, a) ->
      match reduce f, a with
      | LamAff (_, b), a -> reduce (b a)
      | LamBox (_, b), Box a -> reduce (b a)
      | _ -> t
    | _ -> t
  
  type Usage = Lin of int | Exp of int | Used
  
  type Typing = { Name : string; mutable Usage : Usage; Typ : Typ }

  /// Bidirectional type inference.
  let rec infer ctx dep trm =
    match trm with
    | Var x ->
      match List.tryFind (fun ting -> ting.Name = x) ctx with
      | None -> failwith $"Untyped var {x}."
      | Some ting ->
        match ting.Usage with
        | Lin d when d = dep -> ting.Usage <- Used; ting.Typ
        | Lin _ -> failwith $"Linear var {x} breaks depth restriction."
        | Exp d when d = dep -> ting.Typ
        | Exp d when d = dep + 1 -> ting.Usage <- Used; ting.Typ
        | Exp _ -> failwith $"Exponential var {x} breaks depth restriction."
        | Used -> failwith $"Var {x} breaks affine usage rules."
    | LamAff _ | LamBox _ -> failwith "Can't infer types of raw lambdas."
    | App (f, a) ->
      match infer ctx dep f with
      | Hom (d, c) -> check ctx dep a d; c
      | _ -> failwith $"Inferred non-function application in {trm}."
    | Box t -> Typ.Box (infer ctx (dep + 1) t)
    | Ann (t, typ) -> check ctx dep t typ; typ
  
  /// Bidirectional type checking.
  and check ctx dep trm typ =
    match trm with
    | LamAff (x, b) ->
      match typ with
      | Hom (c, d) ->
        let xc = { Name = x; Usage = Lin dep; Typ = c }
        check (xc :: ctx) dep (b (Var x)) d
      | _ -> failwith $"Affine lambda can't have non-function type {typ}."
    | LamBox (x, b) ->
      match typ with
      | Hom (Typ.Box c, d) ->
        let xc = { Name = x; Usage = Exp dep; Typ = c }
        check (xc :: ctx) dep (b (Var x)) d
      | _ -> failwith $"Box-lambda can't have non-box-function type {typ}."
    | Box t ->
      match typ with
      | Typ.Box ty -> check ctx (dep + 1) t ty
      | _ -> failwith $"Type mismatch, box {trm} cannot have non-box type {typ}."
    | _ ->
      let inf = infer ctx dep trm
      if inf <> typ then
        failwith $"Type mismatch, inferred {inf} instead of {typ}."