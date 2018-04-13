-module(ex2).
-export([main/1,funpong/1]).

main(N) -> S=self(),register(main,S), myspawn(pong,ex2,funpong,[N]), no_funping(). 

funping() -> receive
				{N} -> io:format("pong got message ~p\n",[{pong,{N}}]), pong ! {N-1}, funping()
			 after 
			 	100 -> ok
			 end.


funpong(N) ->
	case N of
		0 -> done;
		M when M>0 -> main ! {M},
			  		  receive
			  		  	{K} -> io:format("ping got message ~p\n",[{ping,{K}}]), funpong(K)
			  		  end
	end.


%% peval stuff: my spawn is used to fix a name for every spawned process
myspawn(Name,Mod,Fun,Args) -> C = spawn(Mod,Fun,Args), register(Name,C).
%% functions prefixed with no_ are not unfolded
no_funping() -> funping().

