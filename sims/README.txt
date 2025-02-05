set.sh
가상환경 설정하는 bash
실행방법: source set.sh

sim.sh
시뮬레이션 1개 컴파일 후 나머지 17개 백그라운드로 일괄 실행하는 bash
실행방법: source sim.sh
주의: ssh 접속 이후 set.sh 한번 실행하고 sim.sh 실행해야 작동함

sim.sh에서 st를 먼저 실행하는 이유: st가 가장 빨리 시뮬레이션 끝남
가장 시뮬레이션이 늦게 끝나는 것은 edn이니 참고
