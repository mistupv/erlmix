Go to the src folder, compile the files:

$erl
> c(utils).
> c(erlmix).

You can then try, e.g., these calls:

> erlmix:mix("../examples/ex1.erl","main/2","2,L",[{main,"{client1,L,1},{client2,L,2}"}]).

and

> erlmix:mix("../examples/ex2.erl","main/1","3",[{main,"{2}"},{pong,"{2}"}]).

The output is a pretty printing form of Core Erlang.