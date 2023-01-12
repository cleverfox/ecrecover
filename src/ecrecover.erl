-module(ecrecover).

%% https://github.com/mbrix/libsecp256k1

%% API
-export([recover/2, recover/3]).

%% NIF
-export([load/0]).
-on_load(load/0).

%%=============================================================================
%% NIF API
%% GoodMsg1 = <<71,23,50,133,168,215,52,30,94,151,47,198,119,40,99,132,248,2,248,239,66,165,236,95,3,187,250,37,76,176,31,173>>.
%% GoodSig1_v = <<27,101,10,207,157,63,95,10,44,121,151,118,161,37,67,85,213,244,6,23,98,162,55,57,106,153,160,224,227,252,43,205,103,41,81,74,13,172,178,230,35,172,74,189,21,124,177,129,99,255,148,34,128,219,77,92,170,214,109,223,148,27,161,46,3>>.
%% ecrecover:recover(<<71,23,50,133,168,215,52,30,94,151,47,198,119,40,99,132,248,2,248,239,66,165,236,95,3,187,250,37,76,176,31,173>>, <<27,101,10,207,157,63,95,10,44,121,151,118,161,37,67,85,213,244,6,23,98,162,55,57,106,153,160,224,227,252,43,205,103,41,81,74,13,172,178,230,35,172,74,189,21,124,177,129,99,255,148,34,128,219,77,92,170,214,109,223,148,27,161,46,3>>).
%% 

load() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    PrivDir = filename:join(AppDir, "priv"),
    SoName = filename:join(PrivDir, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

%%=============================================================================
%% External API

recover(Hash, <<V, Sig:64/binary>>) when V == 27; V == 28 ->
    RecId = V - 27,
    case recover(Hash, Sig, RecId) of
        {ok, <<4, XY:64/binary>>} ->
            <<_:12/bytes, ShortPub:20/bytes>> = keccak256(XY),
            <<0:96, ShortPub/binary>>;
        {error, _} ->
            <<0:256>>
        end;
recover(_Hash, _VSig) ->
    <<0:256>>.

-spec recover(<<_:(32*8)>>, <<_:(65*8)>>, integer()) -> <<_:(32*8)>>.
recover(_Hash, _Sig, _RecId) ->
    not_loaded(?LINE).

%%=============================================================================
%% Internal Functions

keccak256(Bin) ->
    sha3:hash(256, Bin).
