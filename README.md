# ecrecover
FFI (NIF) export of Ethereum's ecrecover for use from Erlang based on
https://github.com/bitcoin-core/secp256k1.git


### Build
Execute:
```
./rebar3 compile
```


## Erlang integration

The shared library uses NIF. Use the Erlang file `src/ecrecover.erl` to use this:

```
./rebar3 shell
Erlang/OTP 25 [erts-13.1.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit] [dtrace]
Eshell V13.1.3  (abort with ^G)

1> Decoded = binary:decode_hex(<<"47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad000000000000000000000000000000000000000000000000000000000000001b650acf9d3f5f0a2c799776a1254355d5f4061762a237396a99a0e0e3fc2bcd6729514a0dacb2e623ac4abd157cb18163ff942280db4d5caad66ddf941ba12e03">>).
2> List = binary:bin_to_list(Decoded).
3> Hash = binary:list_to_bin(lists:sublist(List, 1, 32)).
4> Sig = binary:list_to_bin(lists:sublist(List, 64, 65)).
5> Input = <<Hash/binary, 0:(8*31), Sig/binary>>.
6> binary:bin_to_list(Input) == binary:bin_to_list(Decoded).
7> Result = ecrecover:recover(Hash, Sig).
8> Expected = binary:decode_hex(<<"000000000000000000000000c08b5542d177ac6686946920409741463a15dddb">>).
9> Result == Expected. %% check result
```
