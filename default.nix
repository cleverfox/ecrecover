let
  stable = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/860b56be91fb874d48e23a950815969a7b832fbc.tar.gz"
  ){}; # 21.11
in {
  aeternityEnv = stable.stdenv.mkDerivation {
    name = "ecrecover";
    buildInputs = [
      ## base
      stable.stdenv
      ## erlang
      stable.erlangR23 # OTP 23.3.4.4
    ];
  };
}
