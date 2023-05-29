%%%=============================================================================
%%% @copyright (C) 2023, Aeternity Foundation
%%% @doc
%%%   Unit tests for ecrecover
%%% @end
%%%=============================================================================
-module(ecrecover_tests).

-include_lib("eunit/include/eunit.hrl").

%% MSG and SIG from aecontract_SUITE, in turn taken from 
%%  https://github.com/aeternity/parity-ethereum/blob/master/ethcore/builtin/src/lib.rs#L656
%% GoodHexSig1 = "47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad000000000000000000000000000000000000000000000000000000000000001b650acf9d3f5f0a2c799776a1254355d5f4061762a237396a99a0e0e3fc2bcd6729514a0dacb2e623ac4abd157cb18163ff942280db4d5caad66ddf941ba12e03",
%% <<GoodMsg1:32/binary, _:31/binary, GoodSig1_v:65/binary>> = aeu_hex:hex_to_bin(GoodHexSig1)
-define(MSG, <<71,23,50,133,168,215,52,30,94,151,47,198,119,40,99,132,248,2,248,239,66,165,236,95,3,187,250,37,76,176,31,173>>).
-define(SIG, <<27,101,10,207,157,63,95,10,44,121,151,118,161,37,67,85,213,244,6,23,98,162,55,57,106,153,160,224,227,252,43,205,103,41,81,74,13,172,178,230,35,172,74,189,21,124,177,129,99,255,148,34,128,219,77,92,170,214,109,223,148,27,161,46,3>>).
-define(EXPECTED_PUBKEY, <<0,0,0,0,0,0,0,0,0,0,0,0,192,139,85,66,209,119,172,102,134,148,105,32,64,151,65,70,58,21,221,219>>).


recover_test() ->
    PubKey = ecrecover:recover(?MSG, ?SIG),
    ?assertEqual(?EXPECTED_PUBKEY, PubKey).