import re
import csv
import numpy as np
import functools
from matplotlib import pyplot as plt
import seaborn as sns
import pandas as pd

def get_minimization_times(minimization_contents):
    return list(map(float,re.findall(r"Minimization took: (\d+\.\d+) ms", minimization_contents)))
def get_modelcheck_times(contents):
    return list(map(float,re.findall(r"Absolute time spent model checking: (\d+\.\d+) ms", contents)))
def get_soundness(contents):
    return list(map(lambda x: x=="YES",re.findall(r"(YES|NO)", contents)))
def parse(contents):
    return list(zip(get_soundness(contents),get_minimization_times(contents), get_modelcheck_times(contents)))

def parse_files(filepaths):
    files_contents = list(map(lambda path: open(path).read(), filepaths))
    minimization_times = (functools.reduce(lambda x,y: x+y, map(lambda contents: np.array(get_minimization_times(contents)), files_contents))/len(filepaths))*1000
    modelcheck_times = (functools.reduce(lambda x,y: x+y, map(lambda contents: np.array(get_modelcheck_times(contents)), files_contents))/len(filepaths))*1000
    soundness = get_soundness(files_contents[0])
    return soundness, minimization_times.tolist(), modelcheck_times.tolist()
    # return list(zip(soundness, minimization_times, modelcheck_times))



def parse_graphs_stats(contents):
    file_names = re.findall(r"Graph file name: ([^\n]+)\n", contents)
    amount_of_nodes = list(map(int,re.findall(r"Amount of nodes: (\d+)", contents)))
    amount_of_edges = list(map(int,re.findall(r"Amount of edges: (\d+)", contents)))
    amount_of_backedges = list(map(int,re.findall(r"Amount of backedges: (\d+)", contents)))
    has_overlapping_cycles = list(map(lambda x: x=="yes",re.findall(r"Are there overlapping cycles: (no|yes)", contents)))
    amount_of_positions = list(map(int,re.findall(r"Amount of positions: (\d+)", contents)))
    proof_width = list(map(int,re.findall(r"Proof width: (\d+)", contents)))

    return list(zip(amount_of_nodes, amount_of_edges, amount_of_backedges, has_overlapping_cycles, amount_of_positions, proof_width, file_names))

def generate_tables(parsed_minimization_unzipped_SH, parsed_no_minimization_unzipped_SH, parsed_minimization_unzipped_OR, parsed_no_minimization_unzipped_OR, parsed_minimization_unzipped_FWK, parsed_minimization_unzipped_VLA, parsed_no_minimization_unzipped_VLA, parsed_minimization_unzipped_SLA):
    parsed_minimization_SH = list(zip(parsed_minimization_unzipped_SH[0],parsed_minimization_unzipped_SH[1],parsed_minimization_unzipped_SH[2]))
    parsed_no_minimization_SH = list(zip(parsed_no_minimization_unzipped_SH[0],parsed_no_minimization_unzipped_SH[1],parsed_no_minimization_unzipped_SH[2]))
    parsed_minimization_OR = list(zip(parsed_minimization_unzipped_OR[0],parsed_minimization_unzipped_OR[1],parsed_minimization_unzipped_OR[2]))
    parsed_no_minimization_OR = list(zip(parsed_no_minimization_unzipped_OR[0],parsed_no_minimization_unzipped_OR[1],parsed_no_minimization_unzipped_OR[2]))
    parsed_minimization_FWK = list(zip(parsed_minimization_unzipped_FWK[0],parsed_minimization_unzipped_FWK[1],parsed_minimization_unzipped_FWK[2]))
    parsed_minimization_VLA = list(zip(parsed_minimization_unzipped_VLA[0],parsed_minimization_unzipped_VLA[1],parsed_minimization_unzipped_VLA[2]))
    parsed_no_minimization_VLA = list(zip(parsed_no_minimization_unzipped_VLA[0],parsed_no_minimization_unzipped_VLA[1],parsed_no_minimization_unzipped_VLA[2]))
    parsed_minimization_SLA = list(zip(parsed_minimization_unzipped_SLA[0],parsed_minimization_unzipped_SLA[1],parsed_minimization_unzipped_SLA[2]))

    minimized_graphs_stats_contents = open("./graphs_stats.minimized").read()
    minimized_parsed_stats = parse_graphs_stats(minimized_graphs_stats_contents)
    non_minimized_graphs_stats_contents = open("./graphs_stats.unminimized").read()
    non_minimized_parsed_stats = parse_graphs_stats(non_minimized_graphs_stats_contents)

    table_minimized = [(*SH_tuple, OR_tuple[2], FWK_tuple[2], VLA_tuple[2], SLA_tuple[2], *graph_stats) for SH_tuple, OR_tuple, FWK_tuple, VLA_tuple, SLA_tuple, graph_stats in zip(parsed_minimization_SH, parsed_minimization_OR, parsed_minimization_FWK, parsed_minimization_VLA, parsed_minimization_SLA, minimized_parsed_stats)]
    table_nonminimized = [(*SH_tuple, OR_tuple[2], VLA_tuple[2], *graph_stats) for SH_tuple, OR_tuple, VLA_tuple, graph_stats in zip(parsed_no_minimization_SH, parsed_no_minimization_OR, parsed_no_minimization_VLA, non_minimized_parsed_stats)]

    return table_minimized, table_nonminimized

def output_to_csv(table_minimized, table_nonminimized):
    # table = [(*SH_tuple_min, OR_tuple_min[2], VLA_tuple_min[2], SH_tuple_no_min[2], OR_tuple_no_min[2], VLA_tuple_no_min[2],*graph_stats) for SH_tuple_min, SH_tuple_no_min, OR_tuple_min, OR_tuple_no_min, VLA_tuple_min, VLA_tuple_no_min, graph_stats in zip(parsed_minimization_SH, parsed_no_minimization_SH, parsed_minimization_OR, parsed_no_minimization_OR, parsed_minimization_VLA, parsed_no_minimization_VLA, parsed_stats)]
    # csv_file = open("minimized.csv", mode='w')
    csv_file = open("minimized.opt.csv", mode='w')
    writer = csv.writer(csv_file)

    writer.writerow(["is sound", "minimization time (ms)", "minimized model check time CY (ms)", "minimized model check time OR (ms)", "minimized model check time FWK (ms)", "minimized model check time VLA (ms)", "minimized model check time SLA (ms)", "amount of nodes", "amount of edges", "amount of backedges", "has overlapping cycles", "amount of positions", "graph width", "filename"])
    for row in table_minimized:
        writer.writerow(row)

    csv_file.close()

    # csv_file = open("non-minimized.csv", mode='w')
    csv_file = open("non-minimized.opt.csv", mode='w')
    writer = csv.writer(csv_file)

    writer.writerow(["is sound", "minimization time (ms)", "non-minimized model check time CY (ms)", "non-minimized model check time OR (ms)", "non-minimized model check time VLA (ms)", "amount of nodes", "amount of edges", "amount of backedges", "has overlapping cycles", "amount of positions", "graph_width", "filename"])
    for row in table_nonminimized:
        writer.writerow(row)

    csv_file.close()


# minimization_filepaths_SH = [f"./logs/results.minimization.CY.{i}" for i in range(1,6)]
# no_minimization_filepaths_SH = [f"./logs/results.no_minimization.CY.{i}" for i in range(1,6)]
# minimization_filepaths_OR = [f"./logs/results.minimization.OR.{i}" for i in range(1,6)]
# no_minimization_filepaths_OR = [f"./logs/results.no_minimization.OR.{i}" for i in range(1,6)]
# minimization_filepaths_FWK = [f"./logs/results.minimization.FWK.{i}" for i in range(1,6)]
# no_minimization_filepaths_FWK = [f"./logs/results.no_minimization.FWK.{i}" for i in range(1,6)]
# minimization_filepaths_VLA = [f"./logs/results.minimization.VLA.{i}" for i in range(1,3)]
# no_minimization_filepaths_VLA = [f"./logs/results.no_minimization.VLA.{i}" for i in range(1,3)]
# minimization_filepaths_SLA = [f"./logs/results.minimization.SLA.{i}" for i in range(1,3)]
# no_minimization_filepaths_SLA = [f"./logs/results.no_minimization.SLA.{i}" for i in range(1,3)]

# parsed_minimization_unzipped_SH = parse_files(minimization_filepaths_SH)
# parsed_no_minimization_unzipped_SH = parse_files(no_minimization_filepaths_SH)
# parsed_minimization_unzipped_SH_DU = parse_files(minimization_filepaths_SH_DU)
# parsed_no_minimization_unzipped_SH_DU = parse_files(no_minimization_filepaths_SH_DU)

# parsed_minimization_unzipped_OR = parse_files(minimization_filepaths_OR)
# parsed_no_minimization_unzipped_OR = parse_files(no_minimization_filepaths_OR)
# parsed_minimization_unzipped_FWK = parse_files(minimization_filepaths_FWK)
# parsed_minimization_unzipped_VLA = parse_files(minimization_filepaths_VLA)
# parsed_no_minimization_unzipped_VLA = parse_files(no_minimization_filepaths_VLA)
# parsed_minimization_unzipped_SLA = parse_files(minimization_filepaths_SLA)


# table_minimized, table_nonminimized = generate_tables(parsed_minimization_unzipped_SH_DU, parsed_no_minimization_unzipped_SH_DU, parsed_minimization_unzipped_OR, parsed_no_minimization_unzipped_OR, parsed_minimization_unzipped_FWK, parsed_minimization_unzipped_VLA, parsed_no_minimization_unzipped_VLA, parsed_minimization_unzipped_SLA)
# output_to_csv(table_minimized, table_nonminimized)

# table_minimized, table_nonminimized = generate_DU_comparison_tables(parsed_minimization_unzipped_SH, parsed_no_minimization_unzipped_SH, parsed_minimization_unzipped_SH_DU, parsed_no_minimization_unzipped_SH_DU)
# output_comparison_tables_to_csv(table_minimized, table_nonminimized)

def get_graphs_stats(csv_filepath):
    return pd.read_csv(csv_filepath)

def plot_mean_with_interquartile_range_by(frame, group_by_col, values_col, show_legend=True):
    means = frame.groupby(group_by_col)[values_col].mean()
    stats = frame.groupby(group_by_col)[values_col].describe()
    quartiles1 = stats["25%"]
    quartiles3 = stats["75%"]
    mins = frame.groupby(group_by_col)[values_col].min()
    maxs = frame.groupby(group_by_col)[values_col].max()
    ax = sns.lineplot(x=stats.index, y=means, label=values_col)
    ax.fill_between(stats.index, quartiles1, quartiles3, alpha=0.3)
    # ax.set_yscale("log")
    if show_legend:
        ax.legend()
    ax.set_ylabel("duration (milliseconds)")

def get_method_runtimes(runs_filepaths_prefixes, amount_of_runs, method_description):
    filepaths_sl = [f"{runs_filepaths_prefixes}.sl.{i}" for i in range(1,amount_of_runs+1)]
    filepaths_fo = [f"{runs_filepaths_prefixes}.fo.{i}" for i in range(1,amount_of_runs+1)]
    
    parsed_sl = parse_files(filepaths_sl)
    parses_fo = parse_files(filepaths_fo)

    return pd.DataFrame({
        method_description: parses_fo[2]+parsed_sl[2]
    })


def compare_SH_with_and_without_TM(frame):
    plot_mean_with_interquartile_range_by(frame, "edges", "VLA")
    # plot_mean_with_interquartile_range_by(frame, "edges", "SLA")
    plot_mean_with_interquartile_range_by(frame, "edges", "FWK")
    plot_mean_with_interquartile_range_by(frame, "edges", "OR")
    plot_mean_with_interquartile_range_by(frame, "edges", "CY")
    # plot_mean_with_interquartile_range_by(frame, "edges", "CY with TM")
    # plot_mean_with_interquartile_range_by(frame, "amount of backedges", "CY with TM")
    # plot_mean_with_interquartile_range_by(frame, "amount of backedges", "VLA")
    # plot_mean_with_interquartile_range_by(frame, "amount of backedges", "SLA", show_legend=False)
    # plot_mean_with_interquartile_range_by(frame, "amount of backedges", "FWK")
    # plot_mean_with_interquartile_range_by(frame, "amount of backedges", "OR")
    # plot_mean_with_interquartile_range_by(frame, "amount of backedges", "CY")
    plt.show()
    
    frame = frame[(frame["FC answers"]!="don't know")|(frame["DU answers"]!="don't know")|(frame["TM answers"]!="don't know")]
    print(sum(frame["OR"]))
    print(sum(frame["CY"]))
    print(sum(frame["CY with TM"]))
    print(len(frame.index))
    # plot_mean_with_interquartile_range_by(frame, "edges", "OR")
    # plot_mean_with_interquartile_range_by(frame, "edges", "CY")
    # plot_mean_with_interquartile_range_by(frame, "edges", "CY with TM")
    # plt.show()

def plot_runtimes_and_overheads(frame, methods, palette):
    interesting_cols = ["edges","nodes","buds"]
    runtimes_frame = frame[methods+interesting_cols]
    runtimes_frame = pd.melt(runtimes_frame, var_name="method", value_name="duration (ms)", id_vars=["edges","nodes","buds"])
    runtimes_frame = runtimes_frame[runtimes_frame["duration (ms)"]>0]
    overhead_frame = frame.copy()[methods+interesting_cols].drop(columns=["CY"])
    overhead_perc_frame = frame.copy()[methods+interesting_cols].drop(columns=["CY"])
    for m in methods:
        if m=="CY":
            continue
        overhead_frame[m] = frame[m] - frame["CY"]
        overhead_perc_frame[m] = 100*overhead_frame[m]/frame["CY"]

    overhead_frame = pd.melt(overhead_frame, var_name="method", value_name="overhead (ms)", id_vars=["edges","nodes","buds"])
    overhead_perc_frame = pd.melt(overhead_perc_frame, var_name="method", value_name="overhead %", id_vars=["edges","nodes","buds"])
    sns.lineplot(data=runtimes_frame, x="edges", y="duration (ms)", hue="method", style="method", palette=palette, errorbar=("pi",50))
    plt.yscale("log")
    plt.show()

    sns.lineplot(data=overhead_frame, x="edges", y="overhead (ms)", hue="method", style="method", palette=palette, errorbar=("pi",50))
    plt.yscale("log")
    plt.show()

    gfg = sns.lineplot(data=overhead_perc_frame, x="edges", y="overhead %", hue="method", style="method", palette=palette, errorbar=("pi",50))
    plt.yscale("log")
    plt.show()

def get_non_minimized_frame():
    VLA_times = get_method_runtimes("./logs/non-minimized/results.deleteme.VLA", 3, "VLA")
    FWK_times = get_method_runtimes("./logs/non-minimized/results.FWK", 3, "FWK")
    OR_times = get_method_runtimes("./logs/non-minimized/results.deleteme.OR", 5, "OR")
    SH_times = get_method_runtimes("./logs/non-minimized/results.deleteme.SH", 5, "CY")

    stats_frame = get_graphs_stats("./stats.csv")

    frame = pd.concat([stats_frame, VLA_times,FWK_times, OR_times, SH_times], axis=1)
    frame = frame[frame["VLA"]>0]
    return frame

def get_minimized_frame():
    VLA_times = get_method_runtimes("./logs/minimized/results.VLA", 5, "VLA")
    SLA_times = get_method_runtimes("./logs/minimized/results.SLA", 5, "SLA")
    FWK_times = get_method_runtimes("./logs/minimized/results.FWK", 5, "FWK")
    OR_times = get_method_runtimes("./logs/minimized/results.OR", 5, "OR")
    SH_times = get_method_runtimes("./logs/minimized/results.SH", 5, "CY")
    
    stats_frame = get_graphs_stats("./stats.minimized.csv")
    frame = pd.concat([stats_frame, VLA_times, SLA_times, FWK_times, OR_times, SH_times], axis=1)
    frame = frame[frame["VLA"]>0]
    return frame

def plot_CY_performance():
    frame_non_minimized = get_non_minimized_frame()
    methods_non_minimized = ["VLA","FWK", "OR","CY"]
    palette_non_minimized = sns.color_palette(["#233D4D","#FCCA46","#A1C181","#619B8A"])
    plot_runtimes_and_overheads(frame_non_minimized, methods_non_minimized, palette_non_minimized)

    # frame_minimized = get_minimized_frame()
    # methods_minimized = ["VLA","SLA","FWK","OR","CY"]
    # palette_minimized = sns.color_palette(["#233D4D","#19C444", "#FCCA46", "#A1C181","#619B8A"])
    # plot_runtimes_and_overheads(frame_minimized, methods_minimized, palette_minimized)


# print(sum(frame["VLA"]))
# print(sum(frame["SLA"]))
# print(sum(frame["FWK"]))
# print(sum(frame["OR"]))
# print(sum(frame["CY"]))
# print(sum(frame["CY with TM"]))

# compare_SH_with_and_without_TM(frame)
plot_CY_performance()