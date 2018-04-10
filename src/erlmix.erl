-module(erlmix).
-export([mix/4]).

-include("erlmix.hrl").

%%--------------------------------------------------------------------
%% @doc mix takes a file name, a string with the main function,
%% a string with the arguments, and a list [{pid_name,string_messages}]
%% where pid_name is an atom and string_messages is a string with
%% the messages for the process pid_name
%% @end
%%--------------------------------------------------------------------
mix(File,FunStr,ArgsStr,Msgs) ->
  ets:new(info,[set,public,named_table]),
  ets:insert(info,{funs,[]}),
  ets:insert(info,{freshvar,1}),
  utils:loadFile(File),
  Msgs2 = translate_msgs(Msgs),
  Fun = utils:stringToFunName(FunStr),
  Args = utils:stringToCoreArgs(ArgsStr),
  {_, FunArity} = cerl:var_name(Fun),
  case FunArity == length(Args) of
    true ->
      Exp = cerl:c_apply(Fun, Args),
      mixproc([{main,Exp}],[],Msgs2);
    false ->
      error
  end,
  ets:delete(info).

%%--------------------------------------------------------------------
%% @doc Translates messages to Core form
%% @end
%%--------------------------------------------------------------------
translate_msgs([]) -> [];
translate_msgs([{Pid,Msgs}|R]) ->
  [{Pid,utils:stringToCoreArgs(Msgs)}|translate_msgs(R)].

%%--------------------------------------------------------------------
%% @doc Main loop of the partial evaluator. Takes a list of pairs
%% {Proc,Exp} to be partially evaluated, a list of pairs already
%% partially evaluated and the list of messages. It follows a
%% standard partial evaluation procedure.
%% @end
%%--------------------------------------------------------------------
mixproc([],_,_) -> [];
mixproc([{Proc,Exp}|R],Seen,Msgs) ->
  Ms = select_msgs(Proc,Msgs),
	NewExp = peval(Ms,Exp,[]), 
  io:fwrite("~n~n"++core_pp:format(Exp)++" -> "++core_pp:format(NewExp)++"~n"),
  Succ = successors(Proc,NewExp),
  NewCalls = add_calls(R++Succ,[{Proc,Exp}|Seen]),
	[{Exp,NewExp} | mixproc(NewCalls,[{Proc,Exp}|Seen],Msgs)].


%%--------------------------------------------------------------------
%% @doc Selects the messages to a given process
%% @end
%%--------------------------------------------------------------------
select_msgs(Pid,[]) -> io:format("Error: no messages for pid: ~p\n",[Pid]),[];
select_msgs(Pid1,[{Pid2,Ms}|R]) ->
  case Pid1==Pid2 of
    true -> Ms;
    false -> select_msgs(Pid1,R)
  end.

%%--------------------------------------------------------------------
%% @doc Adds the new calls which are not a variant of another call
%% @end
%%--------------------------------------------------------------------
add_calls([],_) -> [];
add_calls([{Proc,E}|R],Seen) ->
  case utils:variant_any(E,Seen) of
     true -> add_calls(R,Seen);
     false -> [{Proc,E}|add_calls(R,Seen)]
  end.  


%%--------------------------------------------------------------------
%% @doc Partial evaluation semantics
%% @end
%%--------------------------------------------------------------------
peval(Ms,Exp,Ancestors) ->
  case utils:is_pattern(Exp) of
    true -> Exp;
    false -> 
    case cerl:type(Exp) of
    var -> Exp;
    cons ->
      ConsHdExp = cerl:cons_hd(Exp),
      ConsTlExp = cerl:cons_tl(Exp),
      NewHd = peval(Ms,ConsHdExp,Ancestors),
      NewTl = peval(Ms,ConsTlExp,Ancestors),
      cerl:c_cons_skel(NewHd,NewTl);
    values ->
      NewValuesEs = peval_list(Ms,cerl:values_es(Exp),Ancestors),
      cerl:c_values(NewValuesEs);
    tuple ->
      NewTupleEs = peval_list(Ms,cerl:tuple_es(Exp),Ancestors),
      cerl:c_tuple_skel(NewTupleEs);
    apply -> 
      ApplyOp = cerl:apply_op(Exp),
      Args = peval_list(Ms,cerl:apply_args(Exp),Ancestors),
      {c_var,[],{FunAtom,Arity}} = ApplyOp,
      FunString = atom_to_list(FunAtom),
      case lists:prefix("no_",FunString) of
        true -> %% do not unfold
            FunOp = list_to_atom(lists:subtract(FunString,"no_")),
            NewFunOp = {c_var,[],{FunOp,Arity}},
            NewExp = cerl:update_c_apply(Exp,NewFunOp,Args),
            NewExp;
        false -> %% unfold only if smaller than last ancestor
            Exp2 = cerl:update_c_apply(Exp,ApplyOp,Args),
            case utils:is_smaller(Exp2,Ancestors) of
              true -> 
                  FunDefs = ets:lookup_element(info,funs,2),
                  FunDef = utils:fundef_lookup(ApplyOp, FunDefs),
                  NewFunDef = utils:fundef_rename(FunDef),
                  FunBody = cerl:fun_body(NewFunDef),
                  FunArgs = cerl:fun_vars(NewFunDef),
                  % standard zip is used here (pretty-printer forces it)
                  NewEnv = lists:zip(FunArgs,Args),
                  NewFunBody = utils:replace_all(NewEnv,FunBody),
                  peval(Ms,NewFunBody,[Exp2|Ancestors]);
              false -> Exp2
            end
        end;
    'case' ->
      CaseArg = peval(Ms,cerl:case_arg(Exp),Ancestors),
      case utils:is_pattern(CaseArg) and not(utils:is_var(CaseArg)) of
        true -> 
          CaseClauses = peval_guards(Ms,cerl:case_clauses(Exp),Ancestors),
          CaseArgs =
            case cerl:type(CaseArg) of
              values -> cerl:values_es(CaseArg);
              _ -> [CaseArg]
          end,
          case cerl_clauses:reduce(CaseClauses,CaseArgs) of
            {true,{Clause,Bindings}} -> 
                    peval(Ms,utils:replace_all(Bindings,cerl:clause_body(Clause)),Ancestors);
            {false,[]} ->
              io:fwrite("Error: No matching clause~n") 
          end;
        false ->
          NewCaseClauses = peval_clauses(Ms,cerl:case_clauses(Exp),Ancestors),
          cerl:update_c_case(Exp,CaseArg,NewCaseClauses)
      end;
    'let' ->
          LetArg = peval(Ms,cerl:let_arg(Exp),Ancestors),
          case utils:is_pattern(LetArg) of
          true -> 
              LetVars2 = cerl:let_vars(Exp),
              LetEnv2 = case cerl:let_arity(Exp) of
                            1 -> lists:zip(LetVars2,[LetArg]);
                            _ ->
                                FlatLetArg =
                                  case cerl:type(LetArg) of
                                      values ->
                                          cerl:values_es(LetArg);
                                      _ -> LetArg
                                  end,
                                lists:zip(LetVars2,FlatLetArg)
                          end,
              peval(Ms,utils:replace_all(LetEnv2,cerl:let_body(Exp)),Ancestors);
          false ->
              cerl:update_c_let(Exp,cerl:let_vars(Exp),LetArg,peval(Ms,cerl:let_body(Exp),Ancestors))
          end;
    call ->
      CallArgs = peval_list(Ms,cerl:call_args(Exp),Ancestors),
      CallModule = peval(Ms,cerl:call_module(Exp),Ancestors),
      CallName = peval(Ms,cerl:call_name(Exp),Ancestors),
      % here we assume CallModule and CallName have always a value
      case utils:is_value(CallArgs) of
          true -> case {CallModule, CallName} of
                    {{c_literal,_,'erlang'},{c_literal,_,'spawn'}} -> 
                       cerl:update_c_call(Exp,CallModule,CallName,CallArgs);
                    {{c_literal,_,'erlang'},{c_literal, _, 'self'}} ->
                       cerl:update_c_call(Exp,CallModule,CallName,CallArgs);
                    {{c_literal,_,'erlang'},{c_literal, _, '!'}} ->
                      cerl:update_c_call(Exp,CallModule,CallName,CallArgs);
                    {{c_literal,_,'timer'},{c_literal,_,'sleep'}} ->
                      cerl:update_c_call(Exp,CallModule,CallName,CallArgs);
                    _ ->
                      ConcModule = cerl:concrete(CallModule),
                      ConcName = cerl:concrete(CallName),
                      ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
                      ConcExp = apply(ConcModule, ConcName, ConcArgs),
                      StrExp = lists:flatten(io_lib:format("~p", ([ConcExp]))) ++ ".",
                      {ok, ParsedExp, _} = erl_scan:string(StrExp),
                      {ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
                      hd([utils:toCore(Expr) || Expr <- TypedExp])
                    end;
          false -> cerl:update_c_call(Exp,CallModule,CallName,CallArgs)
        end;
    seq ->
      SeqArg = peval(Ms,cerl:seq_arg(Exp),Ancestors),
      case utils:is_pattern(SeqArg) of
        true ->
          peval(Ms,cerl:seq_body(Exp),Ancestors);
        false ->
          cerl:update_c_seq(Exp,SeqArg,peval(Ms,cerl:seq_body(Exp),Ancestors))
      end;
    'receive' ->
       RecClauses = cerl:receive_clauses(Exp), 
       RecTimeout = cerl:receive_timeout(Exp), 
       RecAction = cerl:receive_action(Exp), 
       NewClauses = rec_new_clauses(Ms,RecClauses),
       cerl:update_c_receive(Exp,peval_clauses(Ms,NewClauses++RecClauses,Ancestors),RecTimeout,RecAction);

    _Other -> Exp  %% give up...
    end
  end.

peval_list(Ms,Exps,Ancestors) ->
  lists:map(fun(X) -> peval(Ms,X,Ancestors) end,Exps).  

peval_clauses(Ms,Exps,Ancestors) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) -> {c_clause,L,Pats,peval(Ms,Guard,Ancestors),peval(Ms,Exp,Ancestors)} end,Exps).  

peval_guards(Ms,Exps,Ancestors) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) -> {c_clause,L,Pats,peval(Ms,Guard,Ancestors),Exp} end,Exps).  

%%--------------------------------------------------------------------
%% @doc Builds specialized clauses for receive statements
%% @end
%%--------------------------------------------------------------------
rec_new_clauses([],_) -> [];
rec_new_clauses([M|R],RecClauses) ->
       %% we assume all messages are patterns and non-variable
       MM = case cerl:type(M) of
              values -> cerl:values_es(M);
              _ -> [M]
       end,
       NewClauses = case cerl_clauses:reduce(RecClauses,MM) of
                      {true,{Clause,Bindings}} -> [utils:replace_all(Bindings,Clause)];
                      {false,[]} -> []
                    end,
       NewClauses ++ rec_new_clauses(R,RecClauses).

%%--------------------------------------------------------------------
%% @doc Extracts the calls from residual code
%% @end
%%--------------------------------------------------------------------
successors(Pid,Exp) ->
  case utils:is_pattern(Exp) of
    true -> [];
    false -> 
    case cerl:type(Exp) of
    var -> [];
    cons ->
      successors(Pid,cerl:cons_hd(Exp))++successors(Pid,cerl:cons_tl(Exp));
    values ->
      lists:flatmap(fun(X) -> successors(Pid,X) end, cerl:values_es(Exp));
    tuple ->
      lists:flatmap(fun(X) -> successors(Pid,X) end, cerl:tuple_es(Exp));
    apply -> 
      ApplyOp = cerl:apply_op(Exp),
      case ApplyOp of
        {c_var,[],{myspawn,4}} ->  
            [Proc,_,{c_literal,[],Fun},Args] = cerl:apply_args(Exp),
            Args2 = cerl:list_elements(Args),
            Arity = length(Args2),
            % io:format("myspawn: ~p\n",[cerl:c_apply({c_var,[],{Fun,Arity}}, Args2)]),
            [{Proc,cerl:c_apply({c_var,[],{Fun,Arity}}, Args2)}];
        _ -> [{Pid,Exp}]
      end;
    'case' ->
      successors(Pid,cerl:case_arg(Exp))++successors_clauses(Pid,cerl:case_clauses(Exp));
    'let' ->
      successors(Pid,cerl:let_arg(Exp))++successors(Pid,cerl:let_body(Exp));
    call ->
      [];
    seq ->
      successors(Pid,cerl:seq_arg(Exp))++successors(Pid,cerl:seq_body(Exp));
    'receive' ->
      successors_clauses(Pid,cerl:receive_clauses(Exp));
    _Other -> []  %% give up...
    end
  end.

successors_clauses(Pid,Clauses) ->
  lists:flatmap(fun({c_clause,_,_,_,Exp}) -> successors(Pid,Exp) end,Clauses).  


