import os

# 벤치마크 이름 모음
file_names = [
    "minver",
    "crc32",
    "sglib-combined",
    "slre",
    "huffbench",
    "matmult-int",
    "nbody",
    "nettle-aes",
    "nsichneu",
    "picojpeg",
    "st",
    "ud",
    "wikisort",
    "aha-mont64",
    "cubic",
    "edn",
    "nettle-sha256",
    "qrduino"
]

# *** 자신 폴더 이름에 맞게 바꾸기
# output 폴더 경로
folder_path = "/data/URP_24_winter/t2021311863/chipyard/sims/verilator/output/chipyard.harness.TestHarness.RocketConfig/"

# misprediction count 저장용 리스트
misprediction_counts = []

for fname in file_names:
    full_path = os.path.join(folder_path, f"{fname}.out")

    # 실제 파일이 존재하는지 먼저 확인
    if not os.path.exists(full_path):
        print(f"[WARN] {full_path} 파일이 존재하지 않습니다.")
        continue

    last_access_count = None
    last_misprediction_count = None

    # 파일 열어서 한 줄씩 읽기
    with open(full_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()

            # *** PASSED *** 라인이 등장하면
            # 이후에는 misprediction, access 카운트를 세지 않고 중단
            if "*** PASSED ***" in line:
                break

            # "misprediction count = ???" 파싱
            if line.startswith("misprediction count ="):
                # '=' 기준으로 split 후 뒤쪽을 strip
                parts = line.split("=")
                if len(parts) == 2:
                    last_misprediction_count = parts[1].strip()

            # "access count = ???" 파싱
            elif line.startswith("access count ="):
                parts = line.split("=")
                if len(parts) == 2:
                    last_access_count = parts[1].strip()

    # 최종 결과 출력
    print(f"[{fname}.out]\n access count = {last_access_count}\t\t"
          f"misprediction count = {last_misprediction_count}")

    # misprediction count를 리스트에 저장
    misprediction_counts.append((fname, last_misprediction_count))

# 모든 파일의 misprediction count 출력 (엑셀에 쓰는 용도)
print("\nSummary of misprediction counts:")
for fname, count in misprediction_counts:
    print(count)
