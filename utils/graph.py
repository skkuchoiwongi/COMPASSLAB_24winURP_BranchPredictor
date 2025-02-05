import matplotlib.pyplot as plt
import numpy as np

# Data
categories = ["Overall", "Corr-High", "Corr-Med", "Corr-Low"]

# 베이스라인으로 쓸 MPKI 정보
baseline_label = "Gshare"
baseline = [
31.204,
18.315,
27.469,
60.113
]

# 첫번째 비교군
comp0_label = "Perceptron"
comp0 = [
15.139,
12.842,
8.288,
51.085
]

# 두번째 비교군
comp1_label = "TAGE"
comp1 = [
14.842,
10.621,
11.028,
33.105
]

# Calculate MPKI improvement compared to baseline
comp0_improvement = [((g - t) / g) * 100 for g, t in zip(baseline, comp0)]
comp1_improvement = [((g - p) / g) * 100 for g, p in zip(baseline, comp1)]

# Plot setup
x = np.arange(len(categories))
bar_width = 0.2

fig, axs = plt.subplots(1, 2, figsize=(14, 6), gridspec_kw={'width_ratios': [2, 3]})

# Left graph: Absolute MPKI values
axs[0].bar(x - bar_width, baseline, bar_width, label=baseline_label, color="gray")
axs[0].bar(x, comp0, bar_width, label=comp0_label, color="red")
axs[0].bar(x + bar_width, comp1, bar_width, label=comp1_label, color="orange")

axs[0].set_title("Absolute MPKI Values (Lower is the better)")
axs[0].set_ylabel("MPKI")
axs[0].set_xticks(x)
axs[0].set_xticklabels(categories)
axs[0].legend()
axs[0].grid(axis="y", linestyle="--", alpha=0.7)

# Right graph: Improvement percentages
axs[1].bar(x - bar_width / 2, comp0_improvement, bar_width, label=comp0_label+" Improvement", color="red")
axs[1].bar(x + bar_width / 2, comp1_improvement, bar_width, label=comp1_label+" Improvement", color="orange")

axs[1].set_title("Improvement Over "+baseline_label)
axs[1].set_ylabel("Improvement (%)")
axs[1].set_xticks(x)
axs[1].set_xticklabels(categories)
axs[1].axhline(0, color="black", linewidth=1, linestyle="--", label="Baseline ("+baseline_label+")")
axs[1].legend()
axs[1].grid(axis="y", linestyle="--", alpha=0.7)

# Adjust spacing and show plot
plt.tight_layout()
plt.savefig('../Graphs/myfigure.png', dpi=250)
plt.show()
