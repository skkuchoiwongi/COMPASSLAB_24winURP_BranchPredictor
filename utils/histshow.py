import os
import matplotlib.pyplot as plt
import numpy as np

# 파일 리스트 (18개 벤치마크)
file_names = [
    "minver", "crc32", "sglib-combined", "slre", "huffbench",
    "matmult-int", "nbody", "nettle-aes", "nsichneu", "picojpeg",
    "st", "ud", "wikisort", "aha-mont64", "cubic", "edn",
    "nettle-sha256", "qrduino"
]

# 랜덤 컬러 지정 (각 벤치마크마다 다른 색상 사용)
colors = plt.cm.get_cmap("tab20", len(file_names))

# *** 자신 폴더 이름에 맞게 바꾸기
# output 폴더 경로
folder_path = "/data/URP_24_winter/t2021311863/chipyard/sims/verilator/output/chipyard.harness.TestHarness.RocketConfig/"

misprediction_data = {}

for fname in file_names:
    full_path = os.path.join(folder_path, f"{fname}.out")
    if not os.path.exists(full_path):
        continue

    last_access_count = None
    misprediction_positions = []

    with open(full_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if "*** PASSED ***" in line:
                break

            # "access count = ???" 파싱
            if line.startswith("access count ="):
                parts = line.split("=")
                if len(parts) == 2:
                    last_access_count = int(parts[1].strip())

            # "misprediction count = ???" 파싱
            elif line.startswith("misprediction count ="):
                parts = line.split("=")
                if len(parts) == 2:
                    misprediction_positions.append(last_access_count)

    if last_access_count and misprediction_positions:
        misprediction_data[fname] = {
            "total_access_count": last_access_count,
            "mispredictions": misprediction_positions
        }

# 그래프 그리기 (점 형식)
plt.figure(figsize=(16, 9), dpi=250)

for idx, (fname, data) in enumerate(misprediction_data.items()):
    total_count = data["total_access_count"]
    mispredictions = data["mispredictions"]

    # 상대적 위치 (0~100% 스케일로 변환)
    relative_positions = np.array(mispredictions) / total_count * 100

    # 점 형식으로 플롯
    plt.scatter(relative_positions, [idx] * len(relative_positions),
                color=colors(idx), alpha=0.8, label=fname, s=25)

plt.xlabel("Execution Progress (%)")
plt.ylabel("Benchmarks")
plt.yticks(range(len(file_names)), file_names)
plt.title("Misprediction Distribution Over Execution")
plt.grid(True, linestyle="--", alpha=0.5)

# 그래프 저장
plt.savefig("misprediction_distribution.png", dpi=300, bbox_inches="tight")

plt.show()
