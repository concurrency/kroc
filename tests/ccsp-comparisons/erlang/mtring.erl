-module(mtring).
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

pass_tokens(0, _) -> done;
pass_tokens(Tokens, Next) ->
  receive
     Token ->
        Next ! (Token + 1),
        pass_tokens(Tokens - 1, Next)
  end.

recv_tokens(0, Sum) -> Sum;
recv_tokens(Tokens, Sum) ->
   receive
      Token -> recv_tokens(Tokens - 1, Sum + Token)
   end.

send_tokens(0, _) -> done;
send_tokens(Tokens, Next) ->
   Next ! Tokens,
   send_tokens(Tokens - 1, Next).

ring_inner(0, Tokens, Next) ->
   Sum = recv_tokens(Tokens, 0),
   io:fwrite("end~n"),
   io:fwrite("~b~n", [Sum]),
   Next ! 0,
   receive
      _ -> done
   end;
ring_inner(Cycles, Tokens, Next) ->
   pass_tokens(Tokens, Next),
   ring_inner(Cycles - 1, Tokens, Next).

ring(1, Cycles, Tokens, Next) ->
   Next ! 1,
   receive
      _ ->
         io:fwrite("start~n"),
         send_tokens(Tokens, Next),
         ring_inner(Cycles, Tokens, Next)
   end;
ring(N, Cycles, Tokens, Last) ->
   Next = spawn(ring, ring_element, [1, Last]),
   ring(N - 1, Cycles, Tokens, Next).

ring_root(Cycles, Tokens) ->
   ring(?ELEMENTS, Cycles, Tokens, self()).

main([A1|[A2|_]]) ->
   Cycles = list_to_integer(A1),
   Tokens = list_to_integer(A2),
   ring_root(Cycles, Tokens),
   erlang:halt().

