-module(ex1).
-export([main/2,client/2]).

main(N,L) -> S=self(),register(main,S),no_init(N,L), no_server().

server() -> receive
               {C,L,N} -> C ! {ack, no_rep(L,N)}, no_server();
               _Y -> error
            after 
              100 -> io:fwrite("Server stopped~n")
            end.

init(N,L) -> case N of
                  0 -> done;
                  M when M>0 -> myspawn(list_to_atom("client" ++ integer_to_list(M)),ex1,client,[M,L]), 
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

%% peval stuff: my spawn is used to fix a name for every spawned process
myspawn(Name,Mod,Fun,Args) -> C = spawn(Mod,Fun,Args), register(Name,C).
%% functions prefixed with no_ are not unfolded
no_init(N,L) -> init(N,L).
no_server() -> server().
no_rep(L,N) -> rep(L,N).

