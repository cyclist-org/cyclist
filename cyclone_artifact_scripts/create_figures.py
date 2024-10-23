import re
import numpy as np
from itertools import groupby
from matplotlib import pyplot as plt
import matplotlib as mplib
import seaborn as sns
import pandas as pd
from matplotlib.lines import Line2D
import csv

figures_dirpath= "/home/figures"



def print_statistics(test_suite, graph_name, width, buds, edges, nodes, amount_of_backedges, amounts_of_trace_manifold_graph_edges, amounts_of_trace_manifold_graph_nodes, has_overlapping_cycles, is_in_cycle_normal_form, FC_durations, FC_answers, DU_durations, DU_answers, TM_durations, TM_answers, OR_durations, OR_answers):
    amount_of_graphs = sum(1 for answer in OR_answers if answer=="yes" or answer=="no")
    amount_of_graphs_in_fo = sum(1 for answer,suite in zip(OR_answers,test_suite) if (answer=="yes" or answer=="no") and suite=="fo")
    amount_of_graphs_in_sl = sum(1 for answer,suite in zip(OR_answers,test_suite) if (answer=="yes" or answer=="no") and suite=="sl")
    FC_OR_answers_with_test_suite = zip(FC_answers, OR_answers, test_suite)

    satisfies_OR_amount = sum(1 for answer in OR_answers if answer=="yes")
    satisfies_OR_amount_in_fo = sum(1 for answer,suite in zip(OR_answers,test_suite) if answer=="yes" and suite=="fo")
    satisfies_OR_amount_in_sl = sum(1 for answer,suite in zip(OR_answers,test_suite) if answer=="yes" and suite=="sl")

    not_satisfies_OR_amount = sum(1 for answer in OR_answers if answer=="no")
    not_satisfies_OR_amount_in_fo = sum(1 for answer,suite in zip(OR_answers,test_suite) if answer=="no" and suite=="fo")
    not_satisfies_OR_amount_in_sl = sum(1 for answer,suite in zip(OR_answers,test_suite) if answer=="no" and suite=="sl")

    satisfies_FC_amount = sum(1 for answer in FC_answers if answer=="no")
    satisfies_FC_and_unsound = sum(1 for FC_answer,OR_answer in zip(FC_answers,OR_answers) if FC_answer=="no" and OR_answer=="no")
    satisfies_FC_of_unsound = 100 * satisfies_FC_and_unsound / not_satisfies_OR_amount
    satisfies_FC_amount_of_all_graphs = 100 * satisfies_FC_amount / amount_of_graphs

    satisfies_FC_amount_in_fo = sum(1 for answer,suite in zip(FC_answers,test_suite) if answer=="no" and suite=="fo")
    satisfies_FC_of_all_in_fo = 100 * satisfies_FC_amount_in_fo / amount_of_graphs_in_fo
    satisfies_FC_of_unsound_in_fo = 100 * satisfies_FC_amount_in_fo / not_satisfies_OR_amount_in_fo

    satisfies_FC_amount_in_sl = sum(1 for answer,suite in zip(FC_answers,test_suite) if answer=="no" and suite=="sl")
    satisfies_FC_of_all_in_sl = 100 * satisfies_FC_amount_in_sl / amount_of_graphs_in_sl
    satisfies_FC_of_unsound_in_sl = 100 * satisfies_FC_amount_in_sl / not_satisfies_OR_amount_in_sl

    DU_yes_amount = sum(1 for answer in DU_answers if answer=="yes")
    # DU_yes_and_sound = sum(1 for DU_answer,OR_answer in zip(DU_answers,OR_answers) if DU_answer=="yes" and OR_answer=="yes")
    DU_yes_of_sound = 100 * DU_yes_amount / satisfies_OR_amount

    DU_yes_amount_in_fo = sum(1 for answer,suite in zip(DU_answers,test_suite) if answer=="yes" and suite=="fo")
    # DU_yes_and_sound_in_fo = sum(1 for DU_answer,OR_answer,suite in zip(DU_answers,OR_answers,test_suite) if DU_answer=="yes" and OR_answer=="yes" and suite=="fo")
    DU_yes_of_sound_in_fo = 100 * DU_yes_amount_in_fo / satisfies_OR_amount_in_fo

    DU_yes_amount_in_sl = sum(1 for answer,suite in zip(DU_answers,test_suite) if answer=="yes" and suite=="sl")
    # DU_yes_and_sound_in_sl = sum(1 for DU_answer,OR_answer,suite in zip(DU_answers,OR_answers,test_suite) if DU_answer=="yes" and OR_answer=="yes" and suite=="sl")
    DU_yes_of_sound_in_sl = 100 * DU_yes_amount_in_sl / satisfies_OR_amount_in_sl

    DU_no_amount = sum(1 for answer in DU_answers if answer=="no")
    # DU_no_and_unsound = sum(1 for DU_answer,OR_answer in zip(DU_answers,OR_answers) if DU_answer=="no" and OR_answer=="no")
    DU_no_of_unsound = 100 * DU_no_amount / not_satisfies_OR_amount

    DU_no_amount_in_fo = sum(1 for answer,suite in zip(DU_answers,test_suite) if answer=="no" and suite=="fo")
    # DU_no_and_unsound_in_fo = sum(1 for DU_answer,OR_answer,suite in zip(DU_answers,OR_answers,test_suite) if DU_answer=="no" and OR_answer=="no" and suite=="fo")
    DU_no_of_unsound_in_fo = 100 * DU_no_amount_in_fo / not_satisfies_OR_amount_in_fo

    DU_no_amount_in_sl = sum(1 for answer,suite in zip(DU_answers,test_suite) if answer=="no" and suite=="sl")
    # DU_no_and_unsound_in_sl = sum(1 for DU_answer,OR_answer,suite in zip(DU_answers,OR_answers,test_suite) if DU_answer=="no" and OR_answer=="no" and suite=="sl")
    DU_no_of_unsound_in_sl = 100 * DU_no_amount_in_sl / not_satisfies_OR_amount_in_sl

    amount_of_unicycles_graphs = sum(1 for has in has_overlapping_cycles if has=="no")
    rate_of_unicycles_graphs = 100 * amount_of_unicycles_graphs / amount_of_graphs
    amount_of_unicycles_graphs_in_fo = sum(1 for has,suite in zip(has_overlapping_cycles,test_suite) if has=="no" and suite=="fo")
    rate_of_unicycles_graphs_in_fo = 100 * amount_of_unicycles_graphs_in_fo / amount_of_graphs_in_fo
    amount_of_unicycles_graphs_in_sl = sum(1 for has,suite in zip(has_overlapping_cycles,test_suite) if has=="no" and suite=="sl")
    rate_of_unicycles_graphs_in_sl = 100 * amount_of_unicycles_graphs_in_sl / amount_of_graphs_in_sl
    DU_true_answers_amount = DU_yes_amount+DU_no_amount
    DU_true_answers_of_all_graphs = 100 * DU_true_answers_amount / amount_of_graphs
    DU_true_answers_of_unicycles_graphs = 100 * DU_true_answers_amount / amount_of_unicycles_graphs

    amount_in_CNF_and_sound = sum(1 for is_CNF,OR_answer in zip(is_in_cycle_normal_form,OR_answers) if is_CNF=="yes" and OR_answer=="yes")
    rate_of_CNF_in_sound = 100 * amount_in_CNF_and_sound / satisfies_OR_amount
    satisfies_TM_amount = sum(1 for answer in TM_answers if answer=="yes")
    satisfies_TM_amount_of_sound = 100 * satisfies_TM_amount / satisfies_OR_amount
    satisfies_TM_amount_of_all_graphs = 100 * satisfies_TM_amount / amount_of_graphs


    print(f"Amount of graphs {amount_of_graphs}")
    print(f"Amount of sound graphs {satisfies_OR_amount}")
    print(f"Amount of unsound graphs {not_satisfies_OR_amount}")
    print()
    print(f"Amount of graphs in the fo suite {amount_of_graphs_in_fo}")
    print(f"Amount of sound graphs in the fo suite {satisfies_OR_amount_in_fo}")
    print(f"Amount of unsound graphs in the fo suite {not_satisfies_OR_amount_in_fo}")

    print(f"Amount of graphs in the sl suite {amount_of_graphs_in_sl}")
    print(f"Amount of sound graphs in the sl suite {satisfies_OR_amount_in_sl}")
    print(f"Amount of unsound graphs in the sl suite {not_satisfies_OR_amount_in_sl}")

    print()

    print(f"Amount of graphs that satisfies FC {satisfies_FC_amount}")
    print(f"Coverage of FC among all graphs {satisfies_FC_amount_of_all_graphs}")
    print(f"Coverage of FC among unsound graphs {satisfies_FC_of_unsound}")
    print()
    print(f"Amount of FC in fo {satisfies_FC_amount_in_fo}")
    print(f"Coverage of FC among all graphs in fo {satisfies_FC_of_all_in_fo}")
    print(f"Coverage of FC among unsound graphs in fo {satisfies_FC_of_unsound_in_fo}")

    print(f"Amount of FC in sl {satisfies_FC_amount_in_sl}")
    print(f"Coverage of FC among all graphs in sl {satisfies_FC_of_all_in_sl}")
    print(f"Coverage of FC among unsound graphs in sl {satisfies_FC_of_unsound_in_sl}")

    print()
    print()

    print(f"Amount of DU true answer {DU_true_answers_amount}")
    print(f"Coverage of DU among all graphs {DU_true_answers_of_all_graphs}")
    print(f"Amount of unicycles graphs {amount_of_unicycles_graphs}")
    print(f"Ratio of unicycles graphs {rate_of_unicycles_graphs}")
    print(f"Amount of unicycles graphs in fo {amount_of_unicycles_graphs_in_fo}")
    print(f"Ratio of unicycles graphs in fo {rate_of_unicycles_graphs_in_fo}")
    print(f"Amount of unicycles graphs in sl {amount_of_unicycles_graphs_in_sl}")
    print(f"Ratio of unicycles graphs in sl {rate_of_unicycles_graphs_in_sl}")
    print()
    print(f"Amount of DU yes {DU_yes_amount}")
    print(f"Coverage of DU yes among sound graphs {DU_yes_of_sound}")
    print(f"Amount of DU yes in fo {DU_yes_amount_in_fo}")
    print(f"Coverage of DU yes among sound graphs in fo {DU_yes_of_sound_in_fo}")
    print(f"Amount of DU yes in sl {DU_yes_amount_in_sl}")
    print(f"Coverage of DU yes among sound graphs in sl {DU_yes_of_sound_in_sl}")

    print(f"Amount of DU no {DU_no_amount}")
    print(f"Coverage of DU no among unsound graphs {DU_no_of_unsound}")
    print(f"Amount of DU no in fo {DU_no_amount_in_fo}")
    print(f"Coverage of DU no among unsound graphs in fo {DU_no_of_unsound_in_fo}")
    print(f"Amount of DU no in sl {DU_no_amount_in_sl}")
    print(f"Coverage of DU no among unsound graphs in sl {DU_no_of_unsound_in_sl}")

    print()
    print()

    print(f"Amount of sound graphs in cycle normal form {amount_in_CNF_and_sound}")
    print(f"Ratio of graphs in cycle normal form among sound graphs {rate_of_CNF_in_sound}")
    print(f"Amount of TM yes {satisfies_TM_amount}")
    print(f"Coverage of TM among sound graphs {satisfies_TM_amount_of_sound}")
    print(f"Coverage of TM among cycle normal form and sound {100*satisfies_TM_amount/amount_in_CNF_and_sound}")
    print(f"Coverage of TM among all graphs {satisfies_TM_amount_of_all_graphs}")

def create_FC_figures(frame):
    frame=frame.copy().rename(columns={"edges":"Edges", "nodes":"Nodes"})
    ax = sns.lineplot(data=frame, x="Nodes", y="FC duration microseconds", errorbar=None)
    sns.lineplot(data=frame, x="Edges", y="FC duration microseconds", errorbar=None,linestyle="dotted", color=sns.color_palette()[0])

    handles, labels = ax.get_legend_handles_labels()

    legend_labels = ['By Nodes',  'By Edges']
    handles = [
        Line2D([0], [0], color=sns.color_palette()[0], lw=2, linestyle='-'),
        Line2D([0], [0], color=sns.color_palette()[0], lw=2, linestyle=':')
    ]

    # Create a single combined legend
    plt.legend(handles=handles, labels=legend_labels, loc='upper left')
    plt.ylabel("Microseconds")
    plt.xlabel("Nodes/Edges")
    savefig_to("figure5.png")

def create_DU_figures(frame):
    frame=frame.copy().rename(columns={"nodes":"Nodes", "edges":"Edges", "width":"Width","amount of backedges":"Backedges"})
    frame_only_unicycles_graphs = frame[frame["has overlapping cycles"]=="no"]
    frame_only_not_unicycles_graphs = frame[frame["has overlapping cycles"]=="yes"]

    color_palette = sns.color_palette()

    ax = sns.lineplot(data=frame, x="Nodes", y="DU duration microseconds", errorbar=None)
    sns.lineplot(data=frame, x="Edges", y="DU duration microseconds", errorbar=None, linestyle="dotted", color=color_palette[0])
    handles, labels = ax.get_legend_handles_labels()

    legend_labels = ['By Nodes',  'By Edges']
    handles = [
        Line2D([0], [0], color=sns.color_palette()[0], lw=2, linestyle='-'),
        Line2D([0], [0], color=sns.color_palette()[0], lw=2, linestyle=':')
    ]

    # Create a single combined legend
    plt.legend(handles=handles, labels=legend_labels, loc='upper left')
    plt.ylabel("Microseconds")
    plt.xlabel("Nodes/Edges")
    savefig_to("figure7a.png")

    sns.lineplot(data=frame, x="Width", y="DU duration microseconds", errorbar=None)
    plt.ylabel("Microseconds")
    savefig_to("figure7b.png")

    sns.lineplot(data=frame, x="Backedges", y="DU duration microseconds", errorbar=None)
    plt.ylabel("Microseconds")
    savefig_to("figure7c.png")


def plot_mean_with_interquartile_range_by(frame, group_by_col, values_col, plot_interquirtile_range=True, show_legend=True, linestyle=None):
    means = frame.groupby(group_by_col)[values_col].mean()
    stats = frame.groupby(group_by_col)[values_col].describe()
    quartiles1 = stats["25%"]
    quartiles3 = stats["75%"]
    mins = frame.groupby(group_by_col)[values_col].min()
    maxs = frame.groupby(group_by_col)[values_col].max()
    sums = frame.groupby(group_by_col)[values_col].sum()

    if linestyle is not None:
        ax = sns.lineplot(x=stats.index, y=means, label=values_col, linestyle=linestyle)
    else:
        ax = sns.lineplot(x=stats.index, y=means, label=values_col)
    if plot_interquirtile_range:
        ax.fill_between(stats.index, quartiles1, quartiles3, alpha=0.3)


    if show_legend:
        ax.legend()
    ax.set_ylabel(values_col)

def savefig_to(path_inside_figures_dir):
    plt.savefig(f"{figures_dirpath}/{path_inside_figures_dir}")
    plt.cla()
    plt.clf()

def print_database_stats(frame):
    print(f"max nodes: {frame['nodes'].max()}")
    print(f"max edges: {frame['edges'].max()}")
    print(f"max width: {frame['width'].max()}")
    print(f"max buds: {frame['buds'].max()}")
    print(f"amount in CNF {sum(frame['is in cycle normal form']=='yes')}")
    print(f"amount in CNF and sound {sum((frame['is in cycle normal form']=='yes') & (frame['OR answer']=='yes'))}")

def plot_database_stats(frame):
    new_frame=frame.copy().rename(columns={"nodes":"Nodes", "edges":"Edges","width":"Width", "buds":"Buds", "test suite":"Test suite"})
    new_frame=pd.melt(new_frame, var_name="Metric", value_name="Amount", id_vars=["Edges","Test suite"], value_vars=["Nodes", "Width", "Buds"])

    frame_copy = frame.copy()
    frame_copy=frame_copy.rename(columns={"nodes":"Nodes", "edges":"Edges","width":"Width", "buds":"Buds", "test suite":"Test suite"})
    frame_copy.loc[frame_copy["Test suite"]=="sl", "Test suite"]="SL"
    frame_copy.loc[frame_copy["Test suite"]=="fo", "Test suite"]="FO"

    ax = sns.kdeplot(frame_copy, x="Nodes", hue="Test suite", bw_adjust=2)
    sns.kdeplot(frame_copy, x="Edges", hue="Test suite", bw_adjust=2, linestyle=':')
    plt.xlabel("Nodes/Edges")

    handles, labels = ax.get_legend_handles_labels()

    legend_labels = ['FO Nodes',  'SL Nodes','FO Edges', 'SL Edges']
    handles = [
        Line2D([0], [0], color=sns.color_palette()[0], lw=2, linestyle='-'),  # FO Nodes
        Line2D([0], [0], color=sns.color_palette()[1], lw=2, linestyle='-'),  # SL Nodes
        Line2D([0], [0], color=sns.color_palette()[0], lw=2, linestyle=':'),  # FO Edges
        Line2D([0], [0], color=sns.color_palette()[1], lw=2, linestyle=':')   # SL Edges
    ]

    # Create a single combined legend
    plt.legend(handles=handles, labels=legend_labels, loc='upper right')
    # savefig_to("database/nodes_edges_per_test_suite.kde.png")
    savefig_to("figure3a.png")

    ax = sns.kdeplot(frame_copy, x="Width", hue="Test suite", bw_adjust=2)
    max_width = frame_copy["Width"].max()
    ax.set_xlim(0, max_width)
    ax.set_xticks(range(0,max_width, 4))
    # savefig_to("database/width_per_test_suite.kde.png")
    savefig_to("figure3b.png")

    ax = sns.kdeplot(frame_copy, x="Buds", hue="Test suite", bw_adjust=2)
    max_buds = frame_copy["Buds"].max()
    ax.set_xlim(0, max_buds)
    ax.set_xticks(range(0,max_buds, 4))
    # savefig_to("database/buds_per_test_suite.kde.png")
    savefig_to("figure3c.png")




def plot_coverages(frame):
    FC_true = len(frame[frame["FC answer"]!="don't know"])
    not_FC_true_DU_true = len(frame[(frame["FC answer"]=="don't know")&(frame["DU answer"]!="don't know")])
    not_FC_nor_DU_true = len(frame[(frame["FC answer"]=="don't know")&(frame["DU answer"]=="don't know")])
    sum_of_amounts = FC_true+not_FC_true_DU_true+not_FC_nor_DU_true
    print(f"amount of graphs {len(frame)}")
    print(f"sum of three amounts {sum_of_amounts}")
    plt.pie([FC_true, not_FC_true_DU_true, not_FC_nor_DU_true], labels=["FC", "DU", "OR"], autopct='%.0f%%', textprops={'fontsize': 16})
    savefig_to("figure8a.png")

def plot_methods_comparison(frame):
    frame["FC"]=frame["FC duration microseconds"]
    frame["DU"]=frame["DU duration microseconds"]
    frame["OR"]=frame["OR duration microseconds"]
    plot_mean_with_interquartile_range_by(frame,"edges", "FC")
    plot_mean_with_interquartile_range_by(frame,"edges", "DU",linestyle="--")
    plot_mean_with_interquartile_range_by(frame,"edges", "OR",linestyle=":")
    plt.ylabel("Microseconds")
    plt.xlabel("Edges")
    savefig_to("figure8b.png")

def plot_complete_methods_comparison(frame, methods, palette):
    frame = frame.rename(columns={"edges":"Edges"})
    interesting_cols = ["Edges"]
    runtimes_frame = frame[methods+interesting_cols]
    runtimes_frame = pd.melt(runtimes_frame, var_name="Method", value_name="Runtime (microseconds)", id_vars=["Edges"])
    runtimes_frame = runtimes_frame[runtimes_frame["Runtime (microseconds)"]>0]
    overhead_frame = frame.copy()[methods+interesting_cols].drop(columns=["CY"])
    overhead_perc_frame = frame.copy()[methods+interesting_cols].drop(columns=["CY"])
    for m in methods:
        if m=="CY":
            continue
        overhead_frame[m] = frame[m] - frame["CY"]
        overhead_perc_frame[m] = 100*overhead_frame[m]/frame["CY"]

    overhead_frame = pd.melt(overhead_frame, var_name="Method", value_name="Overhead (microseconds)", id_vars=["Edges"])
    overhead_perc_frame = pd.melt(overhead_perc_frame, var_name="Method", value_name="Overhead %", id_vars=["Edges"])
    sns.lineplot(data=runtimes_frame, x="Edges", y="Runtime (microseconds)", hue="Method", style="Method", palette=palette, errorbar=("pi",50))
    plt.legend(loc="lower right",ncol=2)
    plt.yscale("log")
    savefig_to("figure9a.png")

    sns.lineplot(data=overhead_frame, x="Edges", y="Overhead (microseconds)", hue="Method", style="Method", palette=palette[0:-1], errorbar=("pi",50))
    plt.legend(loc="lower right",ncol=2)
    plt.yscale("log")
    savefig_to("figure9b.png")

    gfg = sns.lineplot(data=overhead_perc_frame, x="Edges", y="Overhead %", hue="Method", style="Method", palette=palette[0:-1], errorbar=("pi",50))
    plt.legend(loc="upper right",ncol=2)
    plt.yscale("log")
    savefig_to("figure9c.png")

def get_dataframe_from_csv(stats_filepath):
    # dtype={"width": np.double,"buds": np.double,"edges": np.double,"nodes": np.double,"amount of backedges": np.double,"amount of trace manifold graph edges": np.double,"amount of trace manifold graph nodes": np.double,"size of structural connectivity relation": np.double,"amount of positions in all cycles": np.double,"amount of SCCs": np.double,"FC duration microseconds": np.double,"DU duration microseconds": np.double,"TM duration microseconds": np.double,"OR duration microseconds": np.double}
    frame = pd.read_csv(stats_filepath)
    return frame

stats_filepath = "/home/stats.csv"
frame = get_dataframe_from_csv(stats_filepath)


plt.rc('font', size=16)
plt.rcParams.update({'figure.autolayout': True})

plot_database_stats(frame)
create_FC_figures(frame)
create_DU_figures(frame)
plot_coverages(frame)
plot_methods_comparison(frame)

evaluation_frame = get_dataframe_from_csv("/home/evaluation.csv")

# methods = ["VLA", "SLA", "FWK", "OR", "CY"]
# palette = sns.color_palette(["#233D4D","#19C444","#FCCA46","#6924B3","#8C1858"])
methods = ["VLA", "FWK", "OR", "CY"]
palette = sns.color_palette(["#233D4D","#FCCA46","#6924B3","#8C1858"])

plot_complete_methods_comparison(evaluation_frame, methods, palette)