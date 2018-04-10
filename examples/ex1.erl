-module(ex).
-compile(export_all).

main(N,L) -> S=self(),register(main,S),no_init(N,L), no_server().

server() -> receive
               {C,L,N} -> C ! {ack, no_rep(L,N)}, no_server();
               _Y -> error
            after 
              100 -> io:fwrite("Server stopped~n")
            end.

init(N,L) -> case N of
                  0 -> done;
                  M when M>0 -> myspawn(list_to_atom("client" ++ integer_to_list(M)),ex,client,[M,L]), 
                                init(M-1,L)
             end.

client(N,L) -> main ! {list_to_atom("client" ++ integer_to_list(N)),L,N}, 
               receive
                  {ack,Res} -> io:format("Res: ~p~n",[Res]), ok
              end.

rep(L,N) -> case L of
               [] -> [];
               [H|R] -> [aux(H,N)|rep(R,N)]
            end.

aux(X,N) -> case N of 
              0 -> [];
              M when M>0 -> [X|aux(X,M-1)]
            end.

%% peval stuff
myspawn(Name,Mod,Fun,Args) -> C = spawn(Mod,Fun,Args), register(Name,C).
no_init(N,L) -> init(N,L).
no_server() -> server().
no_rep(L,N) -> rep(L,N).

  % Pid=spawn(Mod,Fun,Args), 
  % register(Name,Pid). 

% bench() ->
%   N = 6,
%   L = [a,b,c],
%   T1 = erlang:system_time(millisecond),
%        main(N,L),
%   T2 = erlang:system_time(millisecond),
%   T2-T1.
