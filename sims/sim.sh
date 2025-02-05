cd ~
cd chipyard/sims/verilator/
make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/st
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/minver > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/crc32 > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/sglib-combined > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/slre > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/huffbench > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/matmult-int > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/nbody > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/nettle-aes > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/nsichneu > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/picojpeg > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/ud > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/wikisort > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/aha-mont64 > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/cubic > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/edn > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/nettle-sha256 > /dev/null 2>&1 &
nohup make SUB_PROJECT=chipyard CONFIG=RocketConfig run-binary BINARY=../../software/embench/build/qrduino > /dev/null 2>&1 &
