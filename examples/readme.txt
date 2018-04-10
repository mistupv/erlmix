Go to the src folder, compile the files:

$erl
> c(utils).
> c(erlmix).

and try this call:

> erlmix:mix("../examples/ex1.erl","main/2","2,L",[{main,"{client1,L,1},{client2,L,2}"}]).