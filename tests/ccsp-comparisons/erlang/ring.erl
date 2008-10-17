-module(ring).
-export([main/1, ring_element/2]).

-define(ELEMENTS, 256).

next_token(0) -> 0;
next_token(Token) -> Token + 1.

ring_element(0, _) -> 
   done;
ring_element(_, Next) ->
   receive
      Token ->
         Next ! next_token(Token),
         ring_element(Token, Next)
   end.

ring_inner(0, Token, Next) ->
   io:fwrite("end~n"),
   io:fwrite("~b~n", [Token]),
   Next ! 0,
   receive
   	_ -> done
   end;
ring_inner(Cycles, Token, Next) ->
   Next ! Token + 1,
   receive
      NewToken ->
        ring_inner(Cycles - 1, NewToken, Next)
   end.

ring(1, Cycles, Next) ->
   Next ! 1,
   receive
      Token ->
        io:fwrite("start~n"),
        ring_inner(Cycles, Token, Next)
   end;
ring(N, Cycles, Last) ->
   Next = spawn(ring, ring_element, [1, Last]),
   ring(N - 1, Cycles, Next).

ring_root(Cycles) ->
   ring(?ELEMENTS, Cycles, self()).

main([Arg]) ->
   Cycles = list_to_integer(Arg),
   ring_root(Cycles),
   erlang:halt().

