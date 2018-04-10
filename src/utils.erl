-module(utils).
-export([replace/3,stringToFunName/1,stringToCoreArgs/1,fundef_lookup/2,
	fundef_rename/1,toErlang/1,toCore/1,
	variant_any/2,loadFile/1,is_smaller/2,replace_all/2,is_value/1,is_var/1,is_pattern/1]).

-include("erlmix.hrl").

%%--------------------------------------------------------------------
%% @doc Replaces a variable Var by SubExp (subexpression) in SuperExp
%% (expression)
%% @end
%%--------------------------------------------------------------------
replace(Var, SubExp, SuperExp) ->
  VarName = cerl:var_name(Var),
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case cerl:var_name(Exp) of
            VarName -> SubExp;
            _Other -> Exp
          end;
        _Other -> Exp
      end
    end, SuperExp).

%%--------------------------------------------------------------------
%% @doc Converts a string String into a Core Erlang function name
%% @end
%%--------------------------------------------------------------------
stringToFunName(String) ->
  FunParts = string:tokens(String, "/"),
  Name = list_to_atom(lists:nth(1,FunParts)),
  Arity = list_to_integer(lists:nth(2,FunParts)),
  cerl:c_var({Name,Arity}).

%%--------------------------------------------------------------------
%% @doc Parses a string Str that represents a list of arguments
%% and transforms these arguments to their equivalent in Core Erlang
%% @end
%%--------------------------------------------------------------------
stringToCoreArgs([]) ->
  [];
stringToCoreArgs(Str) ->
  StrDot = Str ++ ".",
  {ok, ParsedStr, _} = erl_scan:string(StrDot),
  {ok, Exprs} = erl_parse:parse_exprs(ParsedStr),
  CoreExprs = [toCore(Expr) || Expr <- Exprs],
  CoreExprs.

%%--------------------------------------------------------------------
%% @doc Searches a function definition in FunDefs with name FunName
%% @end
%%--------------------------------------------------------------------
fundef_lookup(FunName, FunDefs) ->
  {_, FunDef} = lists:keyfind(FunName, 1, FunDefs),
  FunDef.

%%--------------------------------------------------------------------
%% @doc Renames all the variables in function definition FunDef
%% @end
%%--------------------------------------------------------------------
fundef_rename(FunDef) ->
  FunVars = cerl:fun_vars(FunDef),
  FunBody = cerl:fun_body(FunDef),
  RenamedVars = pars_rename(FunVars),
  {RenamedExp, _} =
    cerl_trees:mapfold(fun (Exp, Acc) ->
                          case cerl:type(Exp) of
                            var ->
                              case cerl:var_name(Exp) of
                                {_FunName, _FunArity} ->
                                  NewExp = Exp,
                                  NewAcc = Acc;
                              _OtherName ->
                                case lists:keyfind(Exp, 1, Acc) of
                                  false ->
                                    NewExp = fresh_var(),
                                    NewAcc = [{Exp,NewExp}] ++ Acc;
                                  {Exp, NewVar} ->
                                    NewExp = NewVar,
                                    NewAcc = Acc
                                end
                              end;
                            _Other ->
                              NewExp = Exp,
                              NewAcc = Acc
                          end,
                          {NewExp, NewAcc}
                        end,
                        RenamedVars,
                        FunBody),
  NewFunDef = cerl:c_fun([NewVar || {_, NewVar} <- RenamedVars], RenamedExp),
  NewFunDef.

pars_rename(Vars) ->
  [{Var, fresh_var()} || Var <- Vars].

fresh_var() ->
  VarNum = ets:lookup_element(info,freshvar,2),
  ets:insert(info,{freshvar,VarNum + 1}),
  build_var(VarNum).


%%--------------------------------------------------------------------
%% @doc Builds a variable from a given number Num
%% @end
%%--------------------------------------------------------------------
build_var(Num) ->
  NumAtom = list_to_atom("y_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

%%--------------------------------------------------------------------
%% @doc Transforms an Erlang expression Expr to its equivalent in
%% Core Erlang
%% @end
%%--------------------------------------------------------------------
toCore(Expr) ->
  case Expr of
    {var, _, Atom} ->        %%case added for erlmix
      cerl:c_var(Atom);
    {atom, _, Atom} ->
      cerl:c_atom(Atom);
    {integer, _, Int} ->
      cerl:c_int(Int);
    {float, _, Float} ->
      cerl:c_float(Float);
    {string, _, String} ->
      cerl:c_string(String);
    {tuple, _, TupleEs} ->
      cerl:c_tuple_skel([toCore(E) || E <- TupleEs]);
    {cons, _, Head, Tail} ->
      cerl:c_cons_skel(toCore(Head), toCore(Tail));
    {nil, _} ->
      cerl:c_nil()
  end.

toErlang(Expr) ->
  LitExpr =
    case cerl:is_literal(Expr) of
      true -> Expr;
      false -> cerl:fold_literal(Expr)
    end,
  cerl:concrete(LitExpr).

%%--------------------------------------------------------------------
%% @doc Checks if Arg1 is a variant of some expression list Arg2
%% @end
%%--------------------------------------------------------------------

variant_any(_,[]) -> false;
variant_any(E1,[{_,E2}|R]) ->
  case variant_apply(E1,E2) of
    true -> true;
    false -> variant_any(E1,R)
  end.

variant_apply({c_apply,[],Fun1,Args1},{c_apply,[],Fun2,Args2}) ->
  case Fun1==Fun2 of
    true -> variant_list(Args1,Args2);
    false -> false
  end.

variant(Exp1,Exp2) ->
  case {cerl:type(Exp1),cerl:type(Exp2)} of
    {var,var} -> true;
    {literal,literal} -> Exp1==Exp2;
    {nil,nil} -> true;
    {cons,cons} -> variant(cerl:cons_hd(Exp1),cerl:cons_hd(Exp2)) 
                   and variant(cerl:cons_tl(Exp1),cerl:cons_tl(Exp2));
    {values,values} -> variant_list(cerl:values_es(Exp1),cerl:values_es(Exp2));
    {tuple,tuple} -> variant_list(cerl:tuple_es(Exp1),cerl:tuple_es(Exp2));
    _ -> false
  end.

variant_list([],[]) -> true;
variant_list([E1|R1],[E2|R2]) ->
  case variant(E1,E2) of 
    true -> variant_list(R1,R2);
    false -> false
  end.

%%--------------------------------------------------------------------
%% @doc Loads a file into Core form and inserts it into ETS info 
%% @end
%%--------------------------------------------------------------------

loadFile(File) ->
  case compile:file(File, [to_core,binary]) of
    {ok, _, CoreForms} ->
      NoAttsCoreForms = cerl:update_c_module(CoreForms,
                                             cerl:module_name(CoreForms),
                                             cerl:module_exports(CoreForms),
                                             [],
                                             cerl:module_defs(CoreForms)),
      Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end,
      CleanCoreForms = cerl_trees:map(Stripper, NoAttsCoreForms),
      FunDefs = cerl:module_defs(CleanCoreForms),
      ets:insert(info,{funs,FunDefs});
    _Other ->
      ?LOG("Error: Could not compile file " ++ File)
  end.

%%--------------------------------------------------------------------
%% @doc Checks whether an expression (an application) is smaller
%% than another application with the same operator (if any) in a list
%% (a simple well-founded ordering)
%% @end
%%--------------------------------------------------------------------

is_smaller(Exp,List) ->
  ApplyOp = cerl:apply_op(Exp),
  Args = cerl:apply_args(Exp),
  ListOpArgs = select_args_same_op(List,ApplyOp),
  case ListOpArgs of
    [] -> true;
    L -> is_smaller_some_args(Args,L)
  end.

select_args_same_op([],_) -> [];
select_args_same_op([Exp|R],ApplyOp) ->
  ExpOp = cerl:apply_op(Exp),
  case ExpOp==ApplyOp of
    true -> [cerl:apply_args(Exp)]; %%|select_args_same_op(R,ApplyOp)];  %%actually, the last one is enough
    false -> select_args_same_op(R,ApplyOp)
  end.

is_smaller_some_args(_,[]) -> false;
is_smaller_some_args(Args1,[Args2|R]) ->
  case is_smaller_args(Args1,Args2) of
    true -> true;
    false -> is_smaller_some_args(Args1,R)
  end.

is_smaller_args([],[]) -> false;
is_smaller_args([A1|R1],[A2|R2]) ->
  case cerl:is_literal(A1) and cerl:is_literal(A2) of
    true -> case cerl:concrete(A1) < cerl:concrete(A2) of
              true -> true;
              false -> is_smaller_args(R1,R2)
            end;
    false -> is_smaller_args(R1,R2)
  end.

%%--------------------------------------------------------------------
%% @doc A typical substitution application
%% @end
%%--------------------------------------------------------------------

replace_all([],Exp) -> Exp;
replace_all([{Var,Val}|R],Exp) -> replace_all(R,utils:replace(Var,Val,Exp)).


%%--------------------------------------------------------------------
%% @doc Checks if an expression is a value (a literal)
%% @end
%%--------------------------------------------------------------------

%%is_value is only true for literals... is it ok?
is_value([]) -> true;
is_value(Exp) when is_list(Exp) ->
  lists:all(fun is_value/1, Exp);
is_value(Exp) -> 
  case cerl:type(Exp) of
    literal -> true;
    _Other -> false
  end.

%%--------------------------------------------------------------------
%% @doc Checks if an expression is a variable
%% @end
%%--------------------------------------------------------------------
is_var(Exp) -> 
  case cerl:type(Exp) of
    var -> true;
    _ -> false
  end.

%%--------------------------------------------------------------------
%% @doc Checks if an expression is a pattern
%% @end
%%--------------------------------------------------------------------
is_pattern([]) -> true;
is_pattern(Exp) when is_list(Exp) ->
  lists:all(fun is_pattern/1, Exp);
is_pattern(Exp) -> 
  case cerl:type(Exp) of
    var -> true;
    literal -> true;
    nil -> true;
    cons -> is_pattern(cerl:cons_hd(Exp)) and is_pattern(cerl:cons_tl(Exp));
    values -> is_pattern(cerl:values_es(Exp));
    tuple -> is_pattern(cerl:tuple_es(Exp));
    _Other -> false
  end.

