(**
   Functions related to Coccinelle's objects.
*)

(**{2 Testing equality }*)

(**
   Because Coccinelle's function [Lib_parsing_c.real_al_node] is buggy, we need
   to apply this function to the nodes feeded to [Lib_parsing_c.real_al_node]
   for it to correctly check structural egality.

   @author Julia Lawall
*)
let prepare_node_for_semantic_equality ((node2,_),_) =
  Control_flow_c.rewrap ((node2,
                          {Control_flow_c.labels=[];
                           Control_flow_c.bclabels=[];
                           Control_flow_c.is_loop=false;
                           Control_flow_c.is_fake=false}),
                         "") (
    let u = node2 in
    match u with
    | Control_flow_c.FunHeader (def) -> u

    | Control_flow_c.Decl declb -> u
    | Control_flow_c.ExprStatement (st, (eopt, ii)) ->
      Control_flow_c.ExprStatement ((Ast_c.MacroStmt,[]), (eopt, ii))

    | Control_flow_c.IfHeader (st, (e,ii)) ->
      Control_flow_c.IfHeader ((Ast_c.MacroStmt,[]), (e,ii))
    | Control_flow_c.SwitchHeader (st, (e,ii)) ->
      Control_flow_c.SwitchHeader ((Ast_c.MacroStmt,[]), (e,ii))
    | Control_flow_c.WhileHeader (st, (e,ii)) ->
      Control_flow_c.WhileHeader ((Ast_c.MacroStmt,[]), (e,ii))
    | Control_flow_c.DoWhileTail (e,ii) -> u

    | Control_flow_c.ForHeader (st, ((first, (e2opt,i2), (e3opt,i3)), ii)) ->
      Control_flow_c.ForHeader ((Ast_c.MacroStmt,[]),
                                ((first, (e2opt,i2), (e3opt,i3)), ii))

    | Control_flow_c.MacroIterHeader (st, ((s,es), ii)) ->
      Control_flow_c.MacroIterHeader ((Ast_c.MacroStmt,[]), ((s,es), ii))

    | Control_flow_c.ReturnExpr (st, (e,ii)) ->
      Control_flow_c.ReturnExpr ((Ast_c.MacroStmt,[]), (e,ii))

    | Control_flow_c.Case (st, (e,ii)) ->
      Control_flow_c.Case ((Ast_c.MacroStmt,[]), (e,ii))
    | Control_flow_c.CaseRange (st, ((e1, e2),ii)) ->
      Control_flow_c.CaseRange ((Ast_c.MacroStmt,[]), ((e1, e2),ii))

    | Control_flow_c.CaseNode i -> u

    | Control_flow_c.DefineHeader((s,ii), (defkind)) -> u

    | Control_flow_c.DefineExpr e -> u
    | Control_flow_c.DefineType ft -> u
    | Control_flow_c.DefineDoWhileZeroHeader ((),ii) -> u
    | Control_flow_c.DefineTodo -> u

    | Control_flow_c.PragmaHeader ((s,ii),pragmainfo) -> u

    | Control_flow_c.Include _ -> u

    | Control_flow_c.MacroTop (s, args, ii) -> u


    | Control_flow_c.MacroStmt (st, ((),ii)) ->
      Control_flow_c.MacroStmt ((Ast_c.MacroStmt,[]), ((),ii))
    | Control_flow_c.Asm (st, (body,ii)) ->
      Control_flow_c.Asm ((Ast_c.MacroStmt,[]), (body,ii))
    | Control_flow_c.Exec(st, (code,ii)) ->
      Control_flow_c.Exec((Ast_c.MacroStmt,[]), (code,ii))

    | Control_flow_c.Break(st,((),ii),fromswitch) ->
      Control_flow_c.Break((Ast_c.MacroStmt,[]),((),ii),fromswitch)
    | Control_flow_c.Continue (st,((),ii)) ->
      Control_flow_c.Continue ((Ast_c.MacroStmt,[]),((),ii))
    | Control_flow_c.Default  (st,((),ii)) ->
      Control_flow_c.Default  ((Ast_c.MacroStmt,[]),((),ii))
    | Control_flow_c.Return   (st,((),ii)) ->
      Control_flow_c.Return   ((Ast_c.MacroStmt,[]),((),ii))
    | Control_flow_c.Goto  (st, name, ((),ii)) ->
      Control_flow_c.Goto  ((Ast_c.MacroStmt,[]), name, ((),ii))
    | Control_flow_c.Label (st, name, ((),ii)) ->
      Control_flow_c.Label ((Ast_c.MacroStmt,[]), name, ((),ii))
    | Control_flow_c.EndStatement iopt -> u
    | Control_flow_c.DoHeader (st, info) ->
      Control_flow_c.DoHeader ((Ast_c.MacroStmt,[]), info)
    | Control_flow_c.Else info -> u
    | Control_flow_c.SeqEnd (i, info) -> u
    | Control_flow_c.SeqStart (st, i, info) ->
      Control_flow_c.SeqStart ((Ast_c.MacroStmt,[]), i, info)

    | Control_flow_c.IfdefHeader (info) -> u
    | Control_flow_c.IfdefElse (info) -> u
    | Control_flow_c.IfdefEndif (info) -> u

    | Control_flow_c.IfdefIteHeader ii -> u

    | (
      (
        Control_flow_c.TopNode|
        Control_flow_c.EndNode|
        Control_flow_c.ErrorExit|
        Control_flow_c.Exit|
        Control_flow_c.Enter|
        Control_flow_c.LoopFallThroughNode|
        Control_flow_c.FallThroughNode|
        Control_flow_c.AfterNode _|
        Control_flow_c.FalseNode|
        Control_flow_c.TrueNode _|
        Control_flow_c.InLoopNode|
        Control_flow_c.Fake
      ) as x) -> x
  )

let rec definition_semantic_equality
    ((def1,_) : Ast_c.definition)
    ((def2,_) : Ast_c.definition) : bool =
  (name_semantic_equality def1.Ast_c.f_name def2.Ast_c.f_name) &&
  (functionType_semantic_equality def1.Ast_c.f_type def2.Ast_c.f_type) &&
  (List.length def1.Ast_c.f_attr = List.length def2.Ast_c.f_attr) &&
  (List.fold_left2 (fun result attr1 attr2 ->
       result && (attribute_semantic_equality attr1 attr2))
      true def1.Ast_c.f_attr def2.Ast_c.f_attr)

and attribute_semantic_equality
    ((Ast_c.Attribute(str1),_) : Ast_c.attribute)
    ((Ast_c.Attribute(str2),_) : Ast_c.attribute) : bool =
  str1 = str2

and parameterType_semantic_equality
    (partyp1 : Ast_c.parameterType)
    (partyp2 : Ast_c.parameterType) : bool =
  (
    match (partyp1.Ast_c.p_namei,partyp2.Ast_c.p_namei) with
    | (Some(name1),Some(name2)) ->
      name_semantic_equality name1 name2
    | (None,None) -> true
    | _ -> false
  ) &&
  (fullType_semantic_equality partyp1.Ast_c.p_type partyp2.Ast_c.p_type)

and functionType_semantic_equality
    ((typ1,(paramlist1,_)) : Ast_c.functionType)
    ((typ2,(paramlist2,_)) : Ast_c.functionType) : bool =
  (fullType_semantic_equality typ1 typ2) &&
  (List.length paramlist1 = List.length paramlist2) &&
  (List.fold_left2 (fun result (par1,_) (par2,_) ->
       result && (parameterType_semantic_equality par1 par2))
      true paramlist1 paramlist2)

and v_init_semantic_equality
    (vinit1 : Ast_c.v_init)
    (vinit2 : Ast_c.v_init) : bool =
  match (vinit1,vinit2) with
  |(Ast_c.NoInit,Ast_c.NoInit) -> true
  | (Ast_c.ValInit(_,init1),Ast_c.ValInit(_,init2)) ->
    initialiser_semantic_equality init1 init2
  | (Ast_c.ConstrInit(arglist1,_),Ast_c.ConstrInit(arglist2,_)) ->
    (List.length arglist1 = List.length arglist2) &&
    (List.fold_left2 (fun result (arg1,_) (arg2,_) ->
         result && argument_semantic_equality arg1 arg2)
        true arglist1 arglist2)
  | _ -> false

and onedecl_semantic_equality
    (ondec1 : Ast_c.onedecl)
    (ondec2 : Ast_c.onedecl) : bool =
  (
    match (ondec1.Ast_c.v_namei,ondec2.Ast_c.v_namei) with
    | (Some(name1,vinit1),Some(name2,vinit2)) ->
      (name_semantic_equality name1 name2) &&
      (v_init_semantic_equality vinit1 vinit2)
    | (None,None) -> true
    | _ -> false
  ) &&
  (fullType_semantic_equality ondec1.Ast_c.v_type ondec2.Ast_c.v_type)


and declaration_semantic_equality
    (decl1 : Ast_c.declaration)
    (decl2 : Ast_c.declaration) : bool =
  match (decl1,decl2) with
  | (Ast_c.DeclList(declist1,_),Ast_c.DeclList(declist2,_)) ->
    (List.length declist1 = List.length declist2) &&
    (List.fold_left2 (fun result (onedec1,_) (onedec2,_) ->
         onedecl_semantic_equality onedec1 onedec2)
        true declist1 declist2)
  | (Ast_c.MacroDecl((_,str1,arglist1,_),_),
     Ast_c.MacroDecl((_,str2,arglist2,_),_)) ->
    (str1 = str2) &&
    (List.length arglist1 = List.length arglist2) &&
    (List.fold_left2 (fun result (arg1,_) (arg2,_) ->
         result && argument_semantic_equality arg1 arg2)
        true arglist1 arglist2)
  | (Ast_c.MacroDeclInit((_,str1,arglist1,init1),_),
     Ast_c.MacroDeclInit((_,str2,arglist2,init2),_)) ->
    (str1 = str2) &&
    (List.length arglist1 = List.length arglist2) &&
    (List.fold_left2 (fun result (arg1,_) (arg2,_) ->
         result && argument_semantic_equality arg1 arg2)
        true arglist1 arglist2) &&
    (initialiser_semantic_equality init1 init2)
  | _ -> false

and constant_semantic_equality
    (const1 : Ast_c.constant)
    (const2 : Ast_c.constant) : bool =
  (** No info, only constructors : classic structural equality works. *)
  const1 = const2

and name_semantic_equality
    (name1 : Ast_c.name)
    (name2 : Ast_c.name) : bool =
  match (name1,name2) with
  | (Ast_c.RegularName(str1,_),Ast_c.RegularName(str2,_))
  | (Ast_c.CppVariadicName(str1,_),Ast_c.CppVariadicName(str2,_)) ->
    str1 = str2
  | (Ast_c.CppConcatenatedName(strlist1),
     Ast_c.CppConcatenatedName(strlist2)) ->
    (List.length strlist1 = List.length strlist2) &&
    (List.fold_left2 (fun result ((str1',_),_) ((str2',_),_) ->
         result && (str1' = str2'))
        true strlist1 strlist2)
  | (Ast_c.CppIdentBuilder((str1,_),strlist1),
     Ast_c.CppIdentBuilder((str2,_),strlist2)) ->
    (str1 = str2) &&
    (List.length strlist1 = List.length strlist2) &&
    (List.fold_left2 (fun result ((str1',_),_) ((str2',_),_) ->
         result && (str1' = str2'))
        true strlist1 strlist2)
  | _ -> false

and designator_semantic_equality
    ((des1,_) : Ast_c.designator)
    ((des2,_) : Ast_c.designator) : bool =
  match (des1,des2) with
  | (Ast_c.DesignatorField(str1),Ast_c.DesignatorField(str2)) ->
    str1 = str2
  | (Ast_c.DesignatorIndex(expr1),Ast_c.DesignatorIndex(expr2)) ->
    expression_semantic_equality expr1 expr2
  | (Ast_c.DesignatorRange(expr11,expr12),Ast_c.DesignatorRange(expr21,expr22)) ->
    (expression_semantic_equality expr11 expr21) &&
    (expression_semantic_equality expr12 expr22)
  | _ -> false

and initialiser_semantic_equality
    ((init1,_) : Ast_c.initialiser)
    ((init2,_) : Ast_c.initialiser) : bool =
  match (init1,init2) with
  | (Ast_c.InitExpr(expr1),Ast_c.InitExpr(expr2)) ->
    expression_semantic_equality expr1 expr2
  | (Ast_c.InitList(initlist1),Ast_c.InitList(initlist2)) ->
    (List.length initlist1 = List.length initlist2) &&
    (List.fold_left2 (fun result (init1',_) (init2',_) ->
         result && (initialiser_semantic_equality init1' init2'))
        true initlist1 initlist2)
  | (Ast_c.InitDesignators(deslist1,init1),
     Ast_c.InitDesignators(deslist2,init2)) ->
    (List.length deslist1 = List.length deslist2) &&
    (List.fold_left2 (fun result des1 des2 ->
         result && designator_semantic_equality des1 des2)
        true deslist1 deslist2)
  | (Ast_c.InitFieldOld(str1,init1'),Ast_c.InitFieldOld(str2,init2')) ->
    (str1 = str2) &&
    (initialiser_semantic_equality init1' init2')
  | (Ast_c.InitIndexOld(expr1,init1'),Ast_c.InitIndexOld(expr2,init2')) ->
    (expression_semantic_equality expr1 expr2) &&
    (initialiser_semantic_equality init1' init2')
  | _ -> false

and expression_semantic_equality
    (((expr1,_),_) : Ast_c.expression)
    (((expr2,_),_) : Ast_c.expression) : bool =
  match (expr1,expr2) with
  | (Ast_c.Ident(name1),Ast_c.Ident(name2)) ->
    name_semantic_equality name1 name2
  | (Ast_c.Constant(const1),Ast_c.Constant(const2)) ->
    constant_semantic_equality const1 const2
  | (Ast_c.StringConstant(_,str1,_),Ast_c.StringConstant(_,str2,_)) ->
    str1 = str2
  | (Ast_c.FunCall(expr1,arglist1),Ast_c.FunCall(expr2,arglist2)) ->
    (expression_semantic_equality expr1 expr2) &&
    (List.length arglist1 = List.length arglist2) &&
    (List.fold_left2
       (fun result (arg1,_) (arg2,_) ->
          result && argument_semantic_equality arg1 arg2)
       true arglist1 arglist2)
  | (Ast_c.CondExpr(expr11,Some(expr12),expr13),
     Ast_c.CondExpr(expr21,Some(expr22),expr23)) ->
    (expression_semantic_equality expr11 expr21) &&
    (expression_semantic_equality expr12 expr22) &&
    (expression_semantic_equality expr13 expr23)
  | (Ast_c.CondExpr(expr11,None,expr12),
     Ast_c.CondExpr(expr21,None,expr22)) ->
    (expression_semantic_equality expr11 expr21) &&
    (expression_semantic_equality expr12 expr22)
  | (Ast_c.Sequence(expr11,expr12),Ast_c.Sequence(expr21,expr22)) ->
    (expression_semantic_equality expr11 expr21) &&
    (expression_semantic_equality expr12 expr22)
  | (Ast_c.Assignment(expr11,(assOp1,_),expr12),
     Ast_c.Assignment(expr21,(assOp2,_),expr22)) ->
    (expression_semantic_equality expr11 expr21) &&
    (assOp1 = assOp2) &&
    (expression_semantic_equality expr12 expr22)
  | (Ast_c.Postfix(expr1,fixOp1),Ast_c.Postfix(expr2,fixOp2))
  | (Ast_c.Infix(expr1,fixOp1),Ast_c.Infix(expr2,fixOp2)) ->
    (expression_semantic_equality expr1 expr2) && (fixOp1 = fixOp2)
  | (Ast_c.Unary(expr1,unOp1),Ast_c.Unary(expr2,unOp2)) ->
    (expression_semantic_equality expr1 expr2) && (unOp1 = unOp2)
  | (Ast_c.Binary(expr11,op1,expr12),Ast_c.Binary(expr21,op2,expr22)) ->
    (expression_semantic_equality expr11 expr21)
  | (Ast_c.ArrayAccess(expr11,expr12),Ast_c.ArrayAccess(expr21,expr22)) ->
    (expression_semantic_equality expr11 expr21) &&
    (expression_semantic_equality expr12 expr22)
  | (Ast_c.RecordAccess(expr1,name1),Ast_c.RecordAccess(expr2,name2))
  | (Ast_c.RecordPtAccess(expr1,name1),Ast_c.RecordPtAccess(expr2,name2))->
    (expression_semantic_equality expr1 expr2) &&
    (name_semantic_equality name1 name2)
  | (Ast_c.SizeOfExpr(expr1),Ast_c.SizeOfExpr(expr2)) ->
    (expression_semantic_equality expr1 expr2)
  | (Ast_c.SizeOfType(typ1),Ast_c.SizeOfType(typ2)) ->
    fullType_semantic_equality typ1 typ2
  | (Ast_c.Cast(typ1,expr1),Ast_c.Cast(typ2,expr2)) ->
    (fullType_semantic_equality typ1 typ2) &&
    (expression_semantic_equality expr1 expr2)
  | (Ast_c.StatementExpr(_),Ast_c.StatementExpr(_))
  | (Ast_c.New(_,_),Ast_c.New(_,_))
  | (Ast_c.Delete(_),Ast_c.Delete(_))
  | (Ast_c.Defined(_),Ast_c.Defined(_)) ->
    assert false (** This should not be in the CFG. *)
  | (Ast_c.Constructor(typ1,init1),Ast_c.Constructor(typ2,init2)) ->
    (fullType_semantic_equality typ1 typ2) &&
    (initialiser_semantic_equality init1 init2)
  | (Ast_c.ParenExpr(expr1),Ast_c.ParenExpr(expr2)) ->
    expression_semantic_equality expr1 expr2

  | _ -> false

and declOrExpr_semantic_equality
    (declOrExpr1 : Ast_c.declOrExpr)
    (declOrExpr2 : Ast_c.declOrExpr) : bool =
  match (declOrExpr1,declOrExpr2) with
  | (Ast_c.ForDecl(decl1),Ast_c.ForDecl(decl2)) ->
    declaration_semantic_equality decl1 decl2
  | (Ast_c.ForExp(Some(expr1),_)),Ast_c.ForExp(Some(expr2),_) ->
    expression_semantic_equality expr1 expr2
  | (Ast_c.ForExp(None,_),Ast_c.ForExp(None,_)) -> true
  | _ -> false

and baseType_semantic_equality
    (typ1 : Ast_c.baseType)
    (typ2 : Ast_c.baseType) : bool =
  (** No info in those nodes, only constructors *)
  typ1 = typ2

and fullType_semantic_equality
    ((_,(typ1,_)) : Ast_c.fullType)
    ((_,(typ2,_)) : Ast_c.fullType) : bool =
  match (typ1,typ2) with
  | (Ast_c.NoType,Ast_c.NoType) -> true
  | (Ast_c.BaseType(typ1'),Ast_c.BaseType(typ2')) ->
    baseType_semantic_equality typ1' typ2'
  | (Ast_c.Pointer(typ1'),Ast_c.Pointer(typ2'))
  | (Ast_c.Array(None,typ1'),Ast_c.Array(None,typ2'))->
    fullType_semantic_equality typ1' typ2'
  | (Ast_c.Array(Some(expr1),typ1'),Ast_c.Array(Some(expr2),typ2')) ->
    (expression_semantic_equality expr1 expr2) &&
    (fullType_semantic_equality typ1' typ2')
  | (Ast_c.Decimal(expr11,Some(expr12)),Ast_c.Decimal(expr21,Some(expr22))) ->
    (expression_semantic_equality expr11 expr21) &&
    (expression_semantic_equality expr12 expr22)
  | (Ast_c.Decimal(expr1,None),Ast_c.Decimal(expr2,None)) ->
    expression_semantic_equality expr1 expr2
  (** TODO: ensure equality for all struct and union types*)
  | (Ast_c.Enum(Some(str1),_),Ast_c.Enum(Some(str2),_))
  | (Ast_c.StructUnion(_,Some(str1),_),Ast_c.StructUnion(_,Some(str2),_))
  | (Ast_c.EnumName(str1),Ast_c.EnumName(str2))
  | (Ast_c.StructUnionName(_,str1),Ast_c.StructUnionName(_,str2)) ->
    str1 = str2
  | (Ast_c.Enum(None,_),Ast_c.Enum(None,_))
  | (Ast_c.StructUnion(_,None,_),Ast_c.StructUnion(_,None,_)) ->
    true
  | (Ast_c.TypeName(name1,Some(typ1')),Ast_c.TypeName(name2,Some(typ2'))) ->
    (name_semantic_equality name1 name2) &&
    (fullType_semantic_equality typ1' typ2')
  | (Ast_c.TypeName(name1,None),Ast_c.TypeName(name2,None)) ->
    name_semantic_equality name1 name2
  | (Ast_c.ParenType(typ1'),Ast_c.ParenType(typ2')) ->
    fullType_semantic_equality typ1' typ2'
  | (Ast_c.TypeOfExpr(expr1),Ast_c.TypeOfExpr(expr2)) ->
    expression_semantic_equality expr1 expr2
  | (Ast_c.TypeOfType(typ1'),Ast_c.TypeOfType(typ2')) ->
    fullType_semantic_equality typ1' typ2'
  | _ -> false


and exprStatement_semantic_equality
    (exprstmt1 : Ast_c.exprStatement)
    (exprstmt2 : Ast_c.exprStatement) : bool =
  match (exprstmt1,exprstmt2) with
  | (Some(expr1),Some(expr2)) ->
    expression_semantic_equality expr1 expr2
  | (None,None) -> true
  | _ -> false

and argument_semantic_equality
    (arg1 : Ast_c.argument)
    (arg2 : Ast_c.argument) : bool =
  match (arg1,arg2) with
  | (Common.Left(expr1),Common.Left(expr2)) ->
    expression_semantic_equality expr1 expr2
  | (Common.Right(warg1),Common.Right(warg2)) ->
    begin match (warg1,warg2) with
      | (Ast_c.ArgType(param1),Ast_c.ArgType(param2)) ->
        begin match (param1.Ast_c.p_namei,param2.Ast_c.p_namei) with
          | (Some(name1),Some(name2)) ->
            name_semantic_equality name1 name2
          | (None,None) ->
            fullType_semantic_equality param1.Ast_c.p_type param2.Ast_c.p_type
          | _ -> false
        end
      | (Ast_c.ArgAction(_),Ast_c.ArgAction(_)) -> true
      | _ -> false
    end
  | _ -> false

and includ_semantic_equality
    (inc1 : Ast_c.includ)
    (inc2 : Ast_c.includ) : bool =
  match (inc1.Ast_c.i_include,inc2.Ast_c.i_include) with
  | ((Ast_c.Local(inclist1),_),(Ast_c.Local(inclist2),_))
  | ((Ast_c.NonLocal(inclist1),_),(Ast_c.NonLocal(inclist2),_)) ->
    ((List.length inclist1) = (List.length inclist2)) &&
    (List.fold_left2 (fun result str1 str2-> result && (str1=str2))
       true inclist1 inclist2)
  | ((Ast_c.Weird(str1),_),(Ast_c.Weird(str2),_)) ->
    str1 = str2
  | _ -> false

and node_semantic_equality
    (((node1,_),_) : Control_flow_c.node)
    (((node2,_),_) : Control_flow_c.node) : bool =
  match (node1,node2) with
  | (Control_flow_c.TopNode,Control_flow_c.TopNode)
  | (Control_flow_c.EndNode,Control_flow_c.EndNode)
  | (Control_flow_c.ExprStatement(_,(None,_)),
     Control_flow_c.ExprStatement(_,(None,_)))
  | (Control_flow_c.Else(_),Control_flow_c.Else(_))
  | (Control_flow_c.DoHeader(_,_),Control_flow_c.DoHeader(_,_))
  | (Control_flow_c.EndStatement(_),Control_flow_c.EndStatement(_))
  | (Control_flow_c.Return(_,_),Control_flow_c.Return(_,_))
  | (Control_flow_c.IfdefIteHeader(_),Control_flow_c.IfdefIteHeader(_))
  | (Control_flow_c.DefineDoWhileZeroHeader(_),
     Control_flow_c.DefineDoWhileZeroHeader(_))
  | (Control_flow_c.DefineTodo,Control_flow_c.DefineTodo)
  | (Control_flow_c.Default(_,_),Control_flow_c.Default(_,_))
  | (Control_flow_c.Continue(_,_),Control_flow_c.Continue(_,_))
  | (Control_flow_c.Break(_,_,_),Control_flow_c.Break(_,_,_))
  | (Control_flow_c.Enter,Control_flow_c.Enter)
  | (Control_flow_c.Exit,Control_flow_c.Exit)
  | (Control_flow_c.Fake,Control_flow_c.Fake)
  | (Control_flow_c.TrueNode(_),Control_flow_c.TrueNode(_))
  | (Control_flow_c.FalseNode,Control_flow_c.FalseNode)
  | (Control_flow_c.InLoopNode,Control_flow_c.InLoopNode)
  | (Control_flow_c.FallThroughNode,Control_flow_c.FallThroughNode)
  | (Control_flow_c.LoopFallThroughNode,Control_flow_c.LoopFallThroughNode)
  | (Control_flow_c.ErrorExit,Control_flow_c.ErrorExit)
    ->
    true

  (**
     [spinfer] is not designed to handle the next constructions so they are
     assumed equal.
  *)
  | (Control_flow_c.IfdefHeader(_),Control_flow_c.IfdefHeader(_))
  | (Control_flow_c.IfdefElse(_),Control_flow_c.IfdefElse(_))
  | (Control_flow_c.IfdefEndif(_),Control_flow_c.IfdefEndif(_))
  | (Control_flow_c.Asm(_,_),Control_flow_c.Asm(_,_))
  | (Control_flow_c.MacroStmt(_,_),Control_flow_c.MacroStmt(_,_))
  | (Control_flow_c.Exec(_,_),Control_flow_c.Exec(_,_))
  | (Control_flow_c.AfterNode(_),Control_flow_c.AfterNode(_))
    ->
    true

  | (Control_flow_c.FunHeader(def1),Control_flow_c.FunHeader(def2))
    ->
    definition_semantic_equality def1 def2

  | (Control_flow_c.Decl(decl1),Control_flow_c.Decl(decl2))
    ->
    declaration_semantic_equality decl1 decl2

  | (Control_flow_c.SeqStart(_,n1,_),Control_flow_c.SeqStart(_,n2,_))
  | (Control_flow_c.SeqEnd(n1,_),Control_flow_c.SeqEnd(n2,_))
  | (Control_flow_c.CaseNode(n1),Control_flow_c.CaseNode(n2))
    ->
    (n1 = n2)

  | (Control_flow_c.ExprStatement(_,(Some(expr1),_)),
     Control_flow_c.ExprStatement(_,(Some(expr2),_)))
  | (Control_flow_c.IfHeader(_,(expr1,_)),Control_flow_c.IfHeader(_,(expr2,_)))
  | (Control_flow_c.WhileHeader(_,(expr1,_)),
     Control_flow_c.WhileHeader(_,(expr2,_)))
  | (Control_flow_c.DoWhileTail(expr1,_),Control_flow_c.DoWhileTail(expr2,_))
  | (Control_flow_c.SwitchHeader(_,(expr1,_)),
     Control_flow_c.SwitchHeader(_,(expr2,_)))
  | (Control_flow_c.ReturnExpr(_,(expr1,_)),
     Control_flow_c.ReturnExpr(_,(expr2,_)))
  | (Control_flow_c.DefineExpr(expr1),Control_flow_c.DefineExpr(expr2))
  | (Control_flow_c.Case(_,(expr1,_)),Control_flow_c.Case(_,(expr2,_)))
    ->
    expression_semantic_equality expr1 expr2

  | (Control_flow_c.ForHeader(_,((declOrExpr1,(exprstmt11,_),(exprstmt12,_)),_)),
     Control_flow_c.ForHeader(_,((declOrExpr2,(exprstmt21,_),(exprstmt22,_)),_)))
    ->
    (declOrExpr_semantic_equality declOrExpr1 declOrExpr2) &&
    (exprStatement_semantic_equality exprstmt11 exprstmt21) &&
    (exprStatement_semantic_equality exprstmt12 exprstmt22)

  | (Control_flow_c.MacroIterHeader(_,((name1,arglist1),_)),
     Control_flow_c.MacroIterHeader(_,((name2,arglist2),_)))
  | (Control_flow_c.MacroTop(name1,arglist1,_),
     Control_flow_c.MacroTop(name2,arglist2,_))
    ->
    (name1 = name2) &&
    (List.length arglist1 = List.length arglist2) &&
    (List.fold_left2
       (fun result (arg1,_) (arg2,_) ->
          result && argument_semantic_equality arg1 arg2)
       true arglist1 arglist2)

  | (Control_flow_c.DefineHeader((name1,_),_),
     Control_flow_c.DefineHeader((name2,_),_))
  | (Control_flow_c.PragmaHeader((name1,_),_),
     Control_flow_c.PragmaHeader((name2,_),_))
    ->
    name1 = name2

  | (Control_flow_c.DefineType(typ1),Control_flow_c.DefineType(typ2))
    ->
    fullType_semantic_equality typ1 typ2

  | (Control_flow_c.Include(inc1),Control_flow_c.Include(inc2))
    ->
    includ_semantic_equality inc1 inc2

  | (Control_flow_c.CaseRange(_,((expr11,expr12),_)),
     Control_flow_c.CaseRange(_,((expr21,expr22),_)))
    ->
    (expression_semantic_equality expr11 expr21) &&
    (expression_semantic_equality expr12 expr22)

  | (Control_flow_c.Label(_,name1,_),Control_flow_c.Label(_,name2,_))
  | (Control_flow_c.Goto(_,name1,_),Control_flow_c.Goto(_,name2,_))
    ->
    name_semantic_equality name1 name2
  | _ -> false

(** {2 Printing functions} *)
(**
   Various printing functions for Coccinelle's objects. The functions raise
   [CFG.NotInC] exception when given to print non-C constructions.
*)

(**
   Raised when a C++ declaration is encountered. The parsing library parse C
   and C++ so some C++ might be present.
*)
exception NotInC of string

let stringify_arithOp (op : Ast_c.arithOp) : string =
  match op with
  | Ast_c.Plus -> "+"
  | Ast_c.Minus -> "-"
  | Ast_c.Mul -> "*"
  | Ast_c.Div -> "/"
  | Ast_c.Mod -> "%"
  | Ast_c.DecLeft -> "<<"
  | Ast_c.DecRight -> ">>"
  | Ast_c.And -> "&"
  | Ast_c.Or -> "|"
  | Ast_c.Xor -> "^"
  | Ast_c.Min
  | Ast_c.Max -> raise (NotInC ">? or <?")

let stringify_fixOp (op : Ast_c.fixOp) : string =
  match op with
  | Ast_c.Dec -> "--"
  | Ast_c.Inc -> "++"

let stringify_unaryOp (op : Ast_c.unaryOp) : string =
  match op with
  | Ast_c.GetRef -> "&"
  | Ast_c.GetRefLabel -> "&&"
  | Ast_c.DeRef -> "*"
  | Ast_c.UnPlus -> "+"
  | Ast_c.UnMinus -> "-"
  | Ast_c.Tilde -> "~"
  | Ast_c.Not -> "!"

let stringify_logicalOp (op : Ast_c.logicalOp) : string =
  match op with
  | Ast_c.Inf -> "<"
  | Ast_c.Sup -> ">"
  | Ast_c.InfEq -> "<="
  | Ast_c.SupEq -> ">="
  | Ast_c.Eq -> "=="
  | Ast_c.NotEq -> "!="
  | Ast_c.AndLog -> "&&"
  | Ast_c.OrLog -> "||"

let stringify_name (name : Ast_c.name) : string =
  begin match name with
    | Ast_c.RegularName((name',_)) -> name'
    | _ -> raise (NotInC "Invalid name for identifier.")
  end

let rec stringify_typeC ((typ,_) : Ast_c.typeC) : string =
  match typ with
  | Ast_c.BaseType(typ') ->
    begin match typ' with
      | Ast_c.Void -> "void"
      | Ast_c.IntType(typ'') ->
        begin match typ'' with
          | Ast_c.CChar -> "char"
          | Ast_c.Si(sign,base) ->
            let strsign = begin match sign with
              | Ast_c.Signed -> "signed"
              | Ast_c.UnSigned -> "unsigned"
            end in
            let strbase = begin match base with
              | Ast_c.CChar2 -> "char"
              | Ast_c.CShort -> "short"
              | Ast_c.CInt -> "int"
              | Ast_c.CLong -> "long"
              | Ast_c.CLongLong -> "long long"
            end in
            strsign^" "^strbase
        end
      | Ast_c.FloatType(flo) ->
        begin match flo with
          | Ast_c.CFloat -> "float"
          | Ast_c.CDouble -> "double"
          | Ast_c.CLongDouble -> "long double"
        end
      | Ast_c.SizeType -> "size"
      | Ast_c.SSizeType -> "/SSizeType/"
      | Ast_c.PtrDiffType -> "/PtrDiffType/"
    end
  | Ast_c.Pointer(typ') ->
    let strtyp' = stringify_fullType typ' in
    "*"^strtyp'
  | Ast_c.Array(n',typ') ->
    let strn = begin match n' with
      | Some(n) -> stringify_expression n
      | None -> ""
    end in
    let strtyp = stringify_fullType typ' in
    strtyp^"["^strn^"]"
  | Ast_c.Decimal(e1,e2') ->
    let stre1 = stringify_expression e1 in
    begin match e2' with
      | Some(e2) ->
        let stre2 = stringify_expression e2 in
        "/Decimal("^stre1^","^stre2^")/"
      | None -> "/Decimal("^stre1^")"
    end
  | Ast_c.Enum(str',_)
  | Ast_c.StructUnion(_,str',_) ->
    begin match str' with
      | Some(str) -> str
      | None -> "/Struct or Enum/"
    end
  | Ast_c.EnumName(str)
  | Ast_c.StructUnionName(_,str) ->
    str
  | Ast_c.TypeName(name,_) ->
    stringify_name name
  | Ast_c.ParenType(typ') ->
    let strtyp = stringify_fullType typ' in
    "("^strtyp^")"
  | Ast_c.TypeOfExpr(expr) ->
    let strexpr = stringify_expression expr in
    "typeof("^strexpr^")"
  | Ast_c.TypeOfType(typ') ->
    let strtyp = stringify_fullType typ' in
    "typeof("^strtyp^")"
  | _ -> "/Type/"


and stringify_fullType (typ : Ast_c.fullType) : string =
  match typ with
  | (_,typeC) -> stringify_typeC typeC

and stringify_declaration (decl' : Ast_c.declaration) : string =
  match decl' with
  | Ast_c.DeclList((declist,_)) ->
    let stringify_onedecl (decl : Ast_c.onedecl) =
      let strnameandinit = begin match decl.Ast_c.v_namei with
        | Some(name',Ast_c.NoInit) -> stringify_name name'
        | Some(name',Ast_c.ValInit(_,(init,_))) ->
          let strname = stringify_name name' in
          let strinit = begin match init with
            | Ast_c.InitExpr(expr) -> stringify_expression expr
            | Ast_c.InitList(_) -> "/List of initializations/"
            | _ -> raise (NotInC "Weird variable initialization.")
          end in
          strname^" = "^strinit
        | _ -> "/Unnamed declaration/"
      end in
      let strtyp = stringify_fullType decl.Ast_c.v_type in
      strtyp^" "^strnameandinit
    in
    let rec stringify_decllist decllist =
      begin match decllist with
        | [] -> ""
        | [(decl,_)] -> (stringify_onedecl decl)^";"
        | (decl,_)::rest ->
          (stringify_onedecl decl)^
          ";\n"^(stringify_decllist rest)
      end in
    stringify_decllist declist
  | Ast_c.MacroDecl((_,name,arglist,_),_) ->
    let strargs = stringify_arguments arglist in
    name^"("^strargs^")"
  | Ast_c.MacroDeclInit((_,name,args,(init,_)),_) ->
    let strinit = begin match init with
      | Ast_c.InitExpr(expr) -> stringify_expression expr
      | Ast_c.InitList(_) -> "/List of initializations/"
      | _ -> raise (NotInC "Weird variable initialization.")
    end in
    let strargs = stringify_arguments args in
    "#define "^name^"("^strargs^") = "^strinit

and stringify_compound stmtlist =
  begin match stmtlist with
    | [] -> ""
    | [(Ast_c.StmtElem(stmt2))] ->
      let strstmt2 = stringify_statement stmt2 in
      strstmt2
    | (Ast_c.StmtElem(stmt2))::rest ->
      let strstmt2 = stringify_statement stmt2 in
      let strrest = stringify_compound rest in
      strstmt2^"\n"^strrest
    | _ -> raise (NotInC "C++ directive")
  end

and stringify_statement ((stmt,_) : Ast_c.statement) : string =
  match stmt with
  | Ast_c.Labeled(stmt') ->
    begin match stmt' with
      | Ast_c.Label(name,stmt2) ->
        let strstmt2 = stringify_statement stmt2 in
        let strname = stringify_name name in
        strname^": "^strstmt2
      | Ast_c.Case(e,stmt2) ->
        let stre = stringify_expression e in
        let strstmt2 = stringify_statement stmt2 in
        "case "^stre^":\n"^strstmt2
      | Ast_c.CaseRange(e1,e2,stmt2) ->
        let stre1 = stringify_expression e1 in
        let stre2 = stringify_expression e2 in
        let strstmt2 = stringify_statement stmt2 in
        "case "^stre1^" ... "^stre2^":\n"^strstmt2
      | Ast_c.Default(stmt2) ->
        let strstmt2 = stringify_statement stmt2 in
        "default: "^strstmt2
    end
  | Ast_c.Compound(stmtlist) ->
    stringify_compound stmtlist
  | Ast_c.ExprStatement(expr') ->
    begin match expr' with
      | Some expr ->
        let strexpr = stringify_expression expr in
        strexpr^";"
      | None -> ""
    end
  | Ast_c.Selection(select) ->
    begin match select with
      | Ast_c.If(c,s1,s2) ->
        let strc = stringify_expression c in
        let strs1 = stringify_statement s1 in
        let strs2 = stringify_statement s2 in
        "if ("^strc^")\n"^strs1^strs2
      | Ast_c.Switch(e,s) ->
        let stre = stringify_expression e in
        let strs = stringify_statement s in
        "switch ("^stre^")\n"^strs
      | _ -> raise (NotInC "C++ ifdef")
    end
  | Ast_c.Iteration(it) ->
    begin match it with
      | Ast_c.While(e,s) ->
        let stre = stringify_expression e in
        let strs = stringify_statement s in
        "while ("^stre^")\n"^strs
      | Ast_c.DoWhile(s,e) ->
        let stre = stringify_expression e in
        let strs = stringify_statement s in
        "do\n"^strs^"while ("^stre^")"
      | Ast_c.For(init,ending,incr,s) ->
        let strinit = begin match init with
          | Ast_c.ForDecl(decl) -> stringify_declaration decl
          | Ast_c.ForExp((Some expr ,_)) -> stringify_expression expr
          | Ast_c.ForExp((None,_)) -> ""
        end in
        let strincr = begin match incr with
          | (Some(expr),_) -> stringify_expression expr
          | (None,_) -> ""
        end in
        let strending = begin match ending with
          | (Some(expr),_) -> stringify_expression expr
          | (None,_) -> ""
        end in
        let strs = stringify_statement s in
        "for ("^strinit^";"^strending^";"^strincr^")\n"^strs
      | _ ->  raise (NotInC "C++ macro iteration")
    end
  | Ast_c.Jump(jump) ->
    begin match jump with
      | Ast_c.Goto(name) ->
        let strname = stringify_name name in
        "goto: "^strname
      |  Ast_c.Continue -> "continue;"
      |  Ast_c.Break -> "break;"
      |  Ast_c.Return -> "return;"
      |  Ast_c.ReturnExpr(expr) ->
        let strexpr = stringify_expression expr in
        "return "^strexpr^";"
      |  Ast_c.GotoComputed(expr) ->
        let strexpr = stringify_expression expr in
        "goto: "^strexpr^";"
    end
  |  Ast_c.Decl(decl) -> stringify_declaration decl
  |  Ast_c.Asm(_) -> raise (NotInC "Assembly code")
  |  Ast_c.NestedFunc(def) -> assert(false)
  | _ -> raise (NotInC "Macro statement or exec.")

and stringify_arguments (args : Ast_c.argument Ast_c.wrap2 list) =
  match args with
  | [] -> ""
  | [((Common.Left(argexpr)),_)] -> stringify_expression argexpr
  | ((Common.Left(argexpr)),_)::rest ->
    (stringify_expression argexpr)^", "^(stringify_arguments rest)
  | _ -> raise (NotInC "Weird function argument.")

and stringify_expression (expr : Ast_c.expression) : string =
  match expr with
  | ((expr',_),_) ->
    begin match expr' with
      | Ast_c.Ident(name) -> stringify_name name
      | Ast_c.Constant(const) ->
        begin match const with
          | Ast_c.Char(str,_)
          | Ast_c.String(str,_) -> "'"^str^"'"
          | Ast_c.MultiString(_) -> "/Multistring/"
          | Ast_c.Int(str,_)
          | Ast_c.Float(str,_) -> str
          | Ast_c.DecimalConst(str1,str2,str3) -> str1^"."^str2^"."^str3
        end
      | Ast_c.StringConstant(_,str,_) -> "'"^str^"'"
      | Ast_c.FunCall(expr,args) ->
        (stringify_expression expr)^"("^(stringify_arguments args)^")"
      | Ast_c.CondExpr(e1,e2',e3) ->
        let str1 = stringify_expression e1 in
        let str3 = stringify_expression e3 in
        begin match e2' with
          | Some(e2) ->
            let str2 = stringify_expression e2 in
            str1^" ? "^str2^" : "^str3
          | None ->
            str1^" ? "^str1^" : "^str3
        end
      | Ast_c.Sequence(e1,e2) ->
        let str1 = stringify_expression e1 in
        let str2 = stringify_expression e2 in
        str1^","^str2
      | Ast_c.Assignment(x,op',e) ->
        let strx = stringify_expression x in
        let stre = stringify_expression e in
        let assignPrefix = begin match op' with
          | (Ast_c.SimpleAssign,_) -> ":"
          | (Ast_c.OpAssign(arith),_) -> stringify_arithOp arith
        end in
        strx^" "^assignPrefix^"= "^stre

      | Ast_c.Postfix(e,op) ->
        let stre = stringify_expression e in
        let strop = stringify_fixOp op in
        stre^strop
      | Ast_c.Infix(e,op) ->
        let stre = stringify_expression e in
        let strop = stringify_fixOp op in
        strop^stre
      | Ast_c.Unary(e,op) ->
        let stre = stringify_expression e in
        let strop = stringify_unaryOp op in
        strop^stre
      | Ast_c.Binary(e1,op,e2) ->
        let stre1 = stringify_expression e1 in
        let stre2 = stringify_expression e2 in
        let strop = begin match op with
          | (Ast_c.Arith(op'),_) -> stringify_arithOp op'
          | (Ast_c.Logical(op'),_) -> stringify_logicalOp op'
        end in
        stre1^" "^strop^" "^stre2
      | Ast_c.ArrayAccess(t,i) ->
        let strt = stringify_expression t in
        let stri = stringify_expression i in
        strt^"["^stri^"]"
      | Ast_c.RecordAccess(e,name) ->
        let stre = stringify_expression e in
        let strname = stringify_name name in
        stre^"."^strname
      | Ast_c.RecordPtAccess(e,name) ->
        let stre = stringify_expression e in
        let strname = stringify_name name in
        stre^"->"^strname
      | Ast_c.SizeOfExpr(e) ->
        let stre = stringify_expression e in
        "sizeof("^stre^")"
      | Ast_c.SizeOfType(typ) ->
        let strtyp = stringify_fullType typ in
        "sizeof("^strtyp^")"
      | Ast_c.Cast(typ,e) ->
        let stre = stringify_expression e in
        let strtyp = stringify_fullType typ in
        "("^strtyp^")"^stre
      | Ast_c.StatementExpr(_,_) ->
        raise (NotInC "Statement inside an expression.")
      | Ast_c.Constructor(_,_) ->
        raise (NotInC "Constructor.")
      | Ast_c.ParenExpr(e) ->
        let stre = stringify_expression e in
        "("^stre^")"
      | Ast_c.New(_,_) ->
        raise (NotInC "new keyword")
      | Ast_c.Delete(_) ->
        raise (NotInC "delete keyword")
      | Ast_c.Defined(_) ->
        raise (NotInC "defined operator")
    end

let rec stringify_attributes (attrs : Ast_c.attribute list) : string =
  match attrs with
  | [] -> ""
  | (Ast_c.Attribute(attr),_)::rest -> attr^" "^(stringify_attributes rest)

(**
   As functionType contains both the return type and the parameters, the output
   is not one contiguous string to allow C-Style printing. The output is
   (return type, parameters).
*)
let stringify_functionType (functype : Ast_c.functionType) : (string*string) =
  let (typ, (args,_)) = functype in
  let strtyp = stringify_fullType typ in
  let rec stringify_args args =
    match args with
    | [] -> ""
    | [(paramtyp,_)] ->
      let strname = begin match paramtyp.Ast_c.p_namei with
        | Some(name) -> stringify_name name
        | None -> ""
      end in
      let strtyp = stringify_fullType paramtyp.Ast_c.p_type  in
      strtyp^" "^strname
    | (paramtyp,_)::rest ->
      let strname = begin match paramtyp.Ast_c.p_namei with
        | Some(name) -> stringify_name name
        | None -> ""
      end in
      let strtyp = stringify_fullType paramtyp.Ast_c.p_type  in
      strtyp^" "^strname^","^(stringify_args rest)
  in
  let strargs = stringify_args args in
  (strtyp,"("^strargs^")")

let stringify_definition ((def,_) : Ast_c.definition) : string =
  let strname = stringify_name def.Ast_c.f_name in
  let strattrs = stringify_attributes def.Ast_c.f_attr in
  let (strtyp,strargs) = stringify_functionType def.Ast_c.f_type in
  strattrs^" "^strtyp^" "^strname^strargs

(**
   Encapsulates the Coccinelle unparse function.
*)
let stringify_CFG_to_file
    (flow_graph : Parse_c.program2)
    (filename : string) =
  Unparse_c.pp_program_default flow_graph filename

let stringify_nodeinfo (info : Control_flow_c.nodeinfo) : string =
  let rec stringify_int_list l = match l with
    | [] -> ""
    | [i] -> Printf.sprintf "%d" i
    | i::m -> Printf.sprintf "%d,%s" i (stringify_int_list m)
  in
  Printf.sprintf "Labels : [%s] \nBCLabels : [%s]"
    (stringify_int_list info.Control_flow_c.bclabels)
    (stringify_int_list info.Control_flow_c.labels)

let stringify_CFG_node2 (node2 : Control_flow_c.node2 ) : string =
  match node2 with
  | Control_flow_c.TopNode -> "Top"
  | Control_flow_c.EndNode -> "Bottom"

  | Control_flow_c.FunHeader(def) -> stringify_definition def
  | Control_flow_c.Decl(decl) -> stringify_declaration decl

  | Control_flow_c.SeqStart(_,d,_) -> Printf.sprintf "{ (%d)" d
  | Control_flow_c.SeqEnd(d,_) -> Printf.sprintf "} (%d)" d

  | Control_flow_c.ExprStatement(state,(expr',_)) ->
    begin match expr' with
      | Some(expr) -> (stringify_expression expr)
      | None -> (stringify_statement state)
    end

  | Control_flow_c.IfHeader(state,(expr,_)) ->
    "if ("^(stringify_expression expr)^")"
  | Control_flow_c.Else(_) -> "else"
  | Control_flow_c.WhileHeader(stmt,(expr,_)) ->
    "while ("^(stringify_expression expr)^")"
  | Control_flow_c.DoHeader(_,_) ->
    "do"
  | Control_flow_c.DoWhileTail(expr,_) ->
    "while ("^(stringify_expression expr)^")"
  | Control_flow_c.ForHeader(_,((init,(ending,_),(incr,_)),_)) ->
    let strinit = begin match init with
      | Ast_c.ForDecl(decl) -> stringify_declaration decl
      | Ast_c.ForExp((Some expr ,_)) -> stringify_expression expr
      | Ast_c.ForExp((None,_)) -> ""
    end in
    let strincr = begin match incr with
      | Some(expr) -> stringify_expression expr
      | None -> ""
    end in
    let strending = begin match ending with
      | Some(expr) -> stringify_expression expr
      | None -> ""
    end in
    "for ("^strinit^";"^strending^";"^strincr^")"
  | Control_flow_c.SwitchHeader(_,(expr,_)) ->
    "switch ("^(stringify_expression expr)^")"

  | Control_flow_c.EndStatement(_) -> "end"

  | Control_flow_c.Return(stmt,_) ->
    stringify_statement stmt
  | Control_flow_c.ReturnExpr(stmt,_) ->
    stringify_statement stmt

  | Control_flow_c.DefineHeader((name,_),_) ->
    "#define "^name^" = "
  | Control_flow_c.DefineExpr(expr) ->
    stringify_expression expr
  | Control_flow_c.DefineType(typ) ->
    stringify_fullType typ
  | Control_flow_c.DefineDoWhileZeroHeader(_) -> "/DefineDoWhileZeroHeader/"
  | Control_flow_c.DefineTodo -> "/DefineTodo/"

  | Control_flow_c.Include(_) -> "#include /Something/"
  | Control_flow_c.PragmaHeader(_,_) -> "#pragma"

  | Control_flow_c.MacroTop(_,_,_) -> "#macro"

  | Control_flow_c.Case(stmt,(expr,_)) ->
    let strexpr = stringify_expression expr in
    "case: "^strexpr
  | Control_flow_c.Default(_,_) ->
    "default :"
  | Control_flow_c.Continue(_,_) ->
    "continue;"
  | Control_flow_c.Break(_,_,_) ->
    "break;"
  | Control_flow_c.CaseRange(_,((e1,e2),_)) ->
    let stre1 = stringify_expression e1 in
    let stre2 = stringify_expression e2 in
    "case "^stre1^" ... "^stre2^":"
  | Control_flow_c.Label(_,name,_) ->
    let strname = stringify_name name in
    strname^":"
  | Control_flow_c.Goto(_,name,_) ->
    let strname = stringify_name name in
    "goto: "^strname^";"

  | Control_flow_c.MacroStmt(stmt,_) ->
    stringify_statement stmt

  | Control_flow_c.Enter -> "/Enter/"
  | Control_flow_c.Exit -> "/Exit/"
  | Control_flow_c.Fake -> "/Fake/"
  | Control_flow_c.CaseNode(i) -> Printf.sprintf "/CaseNode(%d)/" i

  | Control_flow_c.TrueNode(boolref) -> Printf.sprintf "/TrueNode(%b)/" !boolref
  | Control_flow_c.FalseNode -> "/FalseNode/"
  | Control_flow_c.InLoopNode -> "/InLoopNode/"
  | Control_flow_c.AfterNode(after) -> begin match after with
      | Control_flow_c.RetAfterNode -> "/RetAfterNode/"
      | Control_flow_c.GotoAfterNode -> "/GotoAfterNode/"
      | Control_flow_c.BreakAfterNode -> "/BreakAfterNode/"
      | Control_flow_c.ContAfterNode -> "/ContAfterNode/"
      | Control_flow_c.SWBreakAfterNode -> "/SWBreakAfterNode/"
      | Control_flow_c.NormalAfterNode -> "/NormalAfterNode/"
    end
  | Control_flow_c.FallThroughNode -> "/FallThroughNode/"
  | Control_flow_c.LoopFallThroughNode -> "/LoopFallThroughNode/"
  | Control_flow_c.ErrorExit -> "/ErrorExit/"

  | _ -> raise (NotInC "Unknown control flow node.")

let stringify_CFG_node1 (node1 : Control_flow_c.node1) : string =
  match node1 with
  | (node2, nodeinfo) -> stringify_CFG_node2 node2

(**
     Printer for the main node object.
*)
let stringify_CFG_node (node : Control_flow_c.node) : string =
  try
    match node with
    | (node1, str) -> stringify_CFG_node1 node1
  with
  | NotInC(error) -> Printf.sprintf "/Unknown node : %s/" error
