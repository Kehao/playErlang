-module(u).
-compile(nowarn_unused_function).
-compile(export_all).
%%YA生成随机字符串
-define(SAFE_CHARS, {$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
    $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
    $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
    $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
    $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $_}).

rngchars(0) ->
  "";
rngchars(N) ->
  [rngchar() | rngchars(N - 1)].

rngchar() ->
  rngchar(crypto:rand_uniform(0, tuple_size(?SAFE_CHARS))).

rngchar(C) ->
  element(1 + C, ?SAFE_CHARS).


%%产生GUID
random_token() ->
  Term = term_to_binary({node(), make_ref()}),
  Digest = erlang:md5(Term),
  binary_to_hex(Digest).

binary_to_hex(Bin) when is_binary(Bin) ->
  [oct_to_hex(N) || <<N:4>> <= Bin].

oct_to_hex(0) -> $0;
oct_to_hex(1) -> $1;
oct_to_hex(2) -> $2;
oct_to_hex(3) -> $3;
oct_to_hex(4) -> $4;
oct_to_hex(5) -> $5;
oct_to_hex(6) -> $6;
oct_to_hex(7) -> $7;
oct_to_hex(8) -> $8;
oct_to_hex(9) -> $9;
oct_to_hex(10) -> $a;
oct_to_hex(11) -> $b;
oct_to_hex(12) -> $c;
oct_to_hex(13) -> $d;
oct_to_hex(14) -> $e;
oct_to_hex(15) -> $f.


%%向上取整
%% @spec int_ceil(F::float()) -> integer()
%% @doc  Return the ceiling of F as an integer. The ceiling is defined as
%%       F when F == trunc(F);
%%       trunc(F) when F &lt; 0;
%%       trunc(F) + 1 when F &gt; 0.
int_ceil(X) ->
  T = trunc(X),
  case (X - T) of
    Pos when Pos > 0 -> T + 1;
    _ -> T
  end.

%%整形幂运算
%% @spec int_pow(X::integer(), N::integer()) -> Y::integer()
%% @doc  Moderately efficient way to exponentiate integers.
%%       int_pow(10, 2) = 100.
int_pow(_X, 0) ->
  1;
int_pow(X, N) when N > 0 ->
  int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
  R * X;
int_pow(X, N, R) ->
  int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

%%判断是否支持IPV6
ipv6_supported() ->
  case (catch inet:getaddr("localhost", inet6)) of
    {ok, _Addr} ->
      true;
    {error, _} ->
      false
  end.

trace(X) -> spawn(fun() -> io:format("~p~n",[X]) end).
trace(X,Y) -> spawn(fun() -> io:format("~p: ~p~n",[X,Y]) end).
traceBinary(X) -> spawn(fun() -> io:format("~p~n",[b2h(X)]) end).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

b2h(Bin) -> lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
h2b(String) -> << << (erlang:list_to_integer([Char], 16)):4/integer >> || Char <- String >>.
t(String)-> h2b(String).
txt(Bin) -> [X || <<X>> <= Bin,X > 32, X < 127, X =/= 45].
b2s(Bin) ->    b2s1(binary_to_list(Bin),[]).
b2s1([],Str) ->    lists:reverse(Str);
b2s1([H|T],Str) -> 
  case H > 32 andalso H < 127 andalso H =/= 45 of      
    true -> b2s1(T,[H|Str]);
    false -> b2s1(T,[46,46|Str])
  end.

pmap(F, L,Parent) -> [receive {Pid, Res} -> Res end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

timer(Time,Fun) -> spawn(fun() -> receive after Time -> ?MODULE:Fun() end end).

signSubtract(A,B) ->   
  case A<0 of       
    true -> (erlang:abs(A)-erlang:abs(B))*-1;
    false -> (erlang:abs(A)-erlang:abs(B))   
  end.

signSubtract1(A,B) ->   
  case A<0 of       
    true -> (erlang:abs(A)-B)*-1;
    _ -> (erlang:abs(A)-B)   
  end.

%%     u:floor(-4.7).
%% Output: -5
floor(X) when X < 0 ->   
  T = trunc(X),   
  case (X - T) =:= 0 of       
    true -> T;
    false -> T - 1   
  end;
floor(X) ->    trunc(X).

%%添加协议头
addLen(Bin) ->   
  Len=erlang:size(Bin)+2,   
  <<Len:16,Bin/binary>>.
