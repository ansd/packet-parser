-module(packet).

-export([client/0, server4/0, server0/0]).

-define(DURATION, 30_000).
-define(PORT, 4001).
-define(MSG_BYTES, 5_000).

-record(parser,
        {
         data ::
         undefined |
         %% this is only if the binary is smaller than 4 bytes
         binary() |
         {RemainingBytes :: non_neg_integer(), iodata()},
         counter
        }).

client() ->
    {ok, Sock} = gen_tcp:connect("localhost", ?PORT, [binary, {packet, 0}]),
    io:format("Starting client~n", []),
    client_loop(Sock, 0).

client_loop(Sock, Counter) ->
    case gen_tcp:send(Sock, <<?MSG_BYTES:32, 1:(8*?MSG_BYTES)>>) of
        ok ->
            case Counter rem 10_000 of
                0 ->
                    io:format("client: sent ~p messages~n", [Counter]);
                _ ->
                    ok
            end,
            client_loop(Sock, Counter + 1);
        {error, Reason} ->
            io:format("client got ~p with ~w sent messages~n", [Reason, Counter])
    end.

server4() ->
    {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 4}]),
    io:format("server: listening...~n", []),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("server: accepted~n", []),
    _TimerRef = erlang:send_after(?DURATION, self(), stop),
    server4_loop(Sock, 0).

server4_loop(Sock, Counter) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
            % case Counter rem 10_000 of
            %     0 ->
            %         io:format("server: received ~p messages~n", [Counter]);
            %     _ ->
            %         ok
            % end,
            ?MSG_BYTES = byte_size(Data),
            server4_loop(Sock, Counter + 1);
        Other ->
            io:format("server: got ~p, received ~w messages~n", [Other, Counter]),
            ok = gen_tcp:close(Sock)
    end.

server0() ->
    {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 0}]),
    io:format("server: listening...~n", []),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("server: accepted~n", []),
    _TimerRef = erlang:send_after(?DURATION, self(), stop),
    server0_loop(Sock, #parser{counter = 0}).

server0_loop(Sock, #parser{counter = Counter} = State0) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
            % case Counter rem 10_000 of
            %     0 ->
            %         io:format("server: received ~p messages~n", [Counter]);
            %     _ ->
            %         ok
            % end,
            State = incoming_data(Data, State0),
            server0_loop(Sock, State);
        Other ->
            io:format("server: got ~p, received ~w messages~n", [Other, Counter]),
            ok = gen_tcp:close(Sock)
    end.

incoming_data(<<>>, State) ->
    State;
incoming_data(<<Size:32, Frame:Size/binary, Rem/binary>>,
              #parser{data = undefined,
                      counter = C} = State) ->
    ?MSG_BYTES = byte_size(Frame),
    incoming_data(Rem,
                  State#parser{data = undefined,
                               counter = C + 1});
incoming_data(<<Size:32, Rem/binary>>,
              #parser{data = undefined} = State) ->
    %% not enough data to complete message, stash and await more data
    State#parser{data = {Size - byte_size(Rem), Rem}};
incoming_data(Data,
              #parser{data = undefined} = State)
  when byte_size(Data) < 4 ->
    %% not enough data to even know the size required
    %% just stash binary and hit last clause next
    State#parser{data = Data};
incoming_data(Data,
              #parser{data = {Size, Partial},
                      counter = C} = State) ->
    case Data of
        <<Part:Size/binary, Rem/binary>> ->
            %% enough data, assemble frame into binary, expensive!
            Frame0 = append_data(Partial, Part),
            Frame = iolist_to_binary(Frame0),
            ?MSG_BYTES = byte_size(Frame),
            incoming_data(Rem,
                          State#parser{data = undefined,
                                       counter = C + 1});
        Rem ->
            %% still not enough data, stash further
            State#parser{data = {Size - byte_size(Rem),
                                 append_data(Partial, Rem)}}
    end;
incoming_data(Data,
              #parser{data = Partial} = State)
  when is_binary(Partial) ->
    incoming_data(<<Partial/binary, Data/binary>>,
                  State#parser{data = undefined}).

append_data(Prev, Data) when is_binary(Prev) ->
    [Prev, Data];
append_data(Prev, Data) when is_list(Prev) ->
    Prev ++ [Data].
