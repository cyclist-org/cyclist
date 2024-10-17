import re
import numpy as np
from itertools import groupby
from matplotlib import pyplot as plt
import matplotlib as mplib
import seaborn as sns
import pandas as pd
from matplotlib.lines import Line2D

figures_dirpath= "/Users/matanshaked/M.Sc research/Thesis/results/figures"

def parse_runtimes(contents):
    parsed_FC_runtimes = re.findall(r"flat cycles answer: (yes|no|don't know) took (\d+)", contents)
    parsed_DU_runtimes = re.findall(r"descending unicycles answer: (yes|no|don't know) took (\d+)", contents)
    parsed_TM_runtimes = re.findall(r"trace manifold answer: (yes|no|don't know) took (\d+)", contents)
    parsed_OR_runtimes = re.findall(r"order reduced answer: (yes|no) took (\d+)", contents)
    is_cycle_normal_form = re.findall(r"Is in cycle normal form\? (yes|no)", contents)
    has_overlapping_cycles = re.findall(r"Has overlapping cycles\? (yes|no)", contents)
    amount_of_backedges = re.findall(r"Amount of backedges: (\d+)", contents)
    graph_width = re.findall(r"Proof width: (\d+)", contents)
    filename = re.findall(r"Graph file name: (.+)", contents)
    amounts_of_buds = re.findall(r"Parsing Buds: Done! , Bud size: (\d+)", contents)
    amounts_of_edges = re.findall(r"Parsing Edges: Done! , Edge size: (\d+)", contents)
    amounts_of_nodes = re.findall(r"Parsing Nodes: Done! , Node size: (\d+)", contents)
    amounts_of_trace_manifold_graph_edges = re.findall(r"Amount of edges in trace manifold graph: (\d+)", contents)
    amounts_of_trace_manifold_graph_nodes = re.findall(r"Amount of nodes in trace manifold graph: (\d+)", contents)
    size_of_structural_connectivity_relation = re.findall(r"size of structural connectivity relation: (\d+)", contents)
    
    amounts_of_SCCs = re.findall(r"Amount of SCCs: (\d+)", contents)
    amounts_of_positions_in_all_cycles = re.findall(r"Amount of positions in all cycles: (\d+)", contents)
    
    [FC_answer, FC_durations] = np.transpose(parsed_FC_runtimes) 
    [DU_answer, DU_durations] = np.transpose(parsed_DU_runtimes)
    [TM_answer, TM_durations] = np.transpose(parsed_TM_runtimes)
    [OR_answer, OR_durations] = np.transpose(parsed_OR_runtimes)
    # print(len(FC_answer))
    # print(len(FC_durations))
    # print(len(filename))
    return np.array(filename), \
    np.array(list(map(int,graph_width))), \
    np.array(list(map(int,amounts_of_buds))), \
    np.array(list(map(int,amounts_of_edges))), \
    np.array(list(map(int,amounts_of_nodes))),\
    np.array(is_cycle_normal_form),\
    np.array(has_overlapping_cycles),\
    np.array(list(map(int,amount_of_backedges))),\
    np.array(list(map(int,amounts_of_trace_manifold_graph_edges))),\
    np.array(list(map(int,amounts_of_trace_manifold_graph_nodes))),\
    np.array(list(map(int,size_of_structural_connectivity_relation))),\
    np.array(list(map(int,amounts_of_positions_in_all_cycles))),\
    np.array(list(map(int,amounts_of_SCCs))),\
    np.array(list(map(int,FC_durations))),\
    np.array(FC_answer),\
    np.array(list(map(int,DU_durations))),\
    np.array(DU_answer),\
    np.array(list(map(int,TM_durations))),\
    np.array(TM_answer),\
    np.array(list(map(int,OR_durations))),\
    np.array(OR_answer)
    # runtimes = list(zip(amounts_of_edges, amounts_of_nodes, FC_durations, DU_durations, OR_durations))
    # runtimes = sorted(runtimes, key=lambda x: x[0])

    # return runtimes

def mean_over(values, x_axis):
    d = {}
    for x_value, value in sorted(zip(x_axis, values), key= lambda x:x[0]):
        if not x_value in d:
            d[x_value] = [0, 0]
        d[x_value][0] += 1
        d[x_value][1] += value
    
    result_x_axis = []
    meaned_values = []
    for key, val in d.items():
        values_amount, values_sum = val
        result_x_axis.append(key)
        meaned_values.append(values_sum/values_amount)
        
    return result_x_axis, meaned_values
        


def graph_runtimes(x_axis, y_axes, labels, x_axis_description, y_scale="linear"):
    for i, y_axis in enumerate(y_axes,0):
        if len(labels)==0:
            plt.plot(x_axis, y_axis)
        else: 
            plt.plot(x_axis, y_axis, label=labels[i])
    
    plt.xlabel(x_axis_description)
    plt.ylabel("microseconds")
    if len(labels)>0:
        plt.legend()
    plt.yscale(y_scale)
    # plt.savefig(f"z.{x_axis}.png", dpi=300)
    plt.show()


def calc_mean(runtimes):
    graph_name=runtimes[0][0]
    width=runtimes[0][1]
    buds=runtimes[0][2]
    edges=runtimes[0][3]
    nodes=runtimes[0][4]
    is_in_cycle_normal_form=runtimes[0][5]
    has_overlapping_cycles=runtimes[0][6]
    amount_of_backedges=runtimes[0][7]
    amounts_of_trace_manifold_graph_edges=runtimes[0][8]
    amounts_of_trace_manifold_graph_nodes=runtimes[0][9]
    size_of_structural_connectivity_relation=runtimes[0][10]
    amounts_of_positions_in_all_cycles=runtimes[0][11]
    amounts_of_SCCs=runtimes[0][12]
    FC_answers=runtimes[0][14]
    DU_answers=runtimes[0][16]
    TM_answers=runtimes[0][18]
    OR_answers=runtimes[0][20]
    mean_FC=np.zeros(len(runtimes[0][0]))
    mean_DU=np.zeros(len(runtimes[0][0]))
    mean_TM=np.zeros(len(runtimes[0][0]))
    mean_OR=np.zeros(len(runtimes[0][0]))
    for i in range(len(runtimes)):
        _,_,_,_,_,_,_,_,_,_,_,_,_,FC,_,DU,_,TM,_,OR,_ = runtimes[i]
        mean_FC = mean_FC + FC
        mean_DU = mean_DU + DU
        mean_TM = mean_TM + TM
        mean_OR = mean_OR + OR
    return graph_name, width, buds, edges, nodes, amount_of_backedges, amounts_of_trace_manifold_graph_edges, amounts_of_trace_manifold_graph_nodes, size_of_structural_connectivity_relation, amounts_of_positions_in_all_cycles, amounts_of_SCCs, has_overlapping_cycles, is_in_cycle_normal_form, mean_FC/len(runtimes), FC_answers, mean_DU/len(runtimes), DU_answers, mean_TM/len(runtimes), TM_answers, mean_OR/len(runtimes), OR_answers

import csv
def output_to_csv(outfilepath, rows): 
    csv_file = open(outfilepath, mode='w')
    writer = csv.writer(csv_file)

    writer.writerow(["amount of nodes", "amount of edges", "proof width", "amount of buds", "amount of backedges", "amount of nodes in the trace manifold graph", "amount of edges in the trace manifold graph", "has overlapping cycles?", "is in cycle normal form?", "FC answer", "FC time (us)", "DU answer", "DU time (us)", "TM answer", "TM time (ms)", "OR answer", "OR time (us)", "graph file name", "test suite"])
    for row in rows:
        writer.writerow(row)

    csv_file.close()

def parse_files_multiruns(filespaths):
    files = [open(filepath, "r") for filepath in filespaths]
    files_contents = [file.read() for file in files]
    runtimes = [parse_runtimes(contents) for contents in files_contents]
    [file.close() for file in files]
    data = calc_mean(runtimes)
    return data

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

    print(f"Amount of DU true answers {DU_true_answers_amount}")
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

def compare_TM_before_and_after(): 
    filespaths_before = [f"./src/generic/test/deleteme.trace_manifold_before_refactor"]
    data_before = parse_files_multiruns(filespaths_before)
    data_before = ["sl"] + [x for x in data_before]
    filespaths_after = [f"./src/generic/test/deleteme.trace_manifold_after_refactor"]
    data_after = parse_files_multiruns(filespaths_after)
    data_after = ["sl"] + [x for x in data_after]

    just_cycle_normal_form = [(edges, TM_before, TM_after) for edges, TM_before, TM_after, is_CNF in zip(edges,TM_before_durations, TM_after_durations, is_in_cycle_normal_form) if is_CNF=="yes"]
    just_cycle_normal_form_edges, just_cycle_normal_form_TM_before_durations, just_cycle_normal_form_TM_after_durations = np.transpose(just_cycle_normal_form)

def get_graphs_with_different_answers(data_before, data_after):
    before_answers = data_before[16]
    print(before_answers)
    after_answers = data_after[16]

    before_yes_after_dont_know = [graph_name for graph_name, before_answer, after_answer in zip(data_before[1], before_answers, after_answers) if before_answer=="yes" and after_answer=="don't know"]
    before_dont_know_after_yes = [graph_name for graph_name, before_answer, after_answer in zip(data_before[1], before_answers, after_answers) if before_answer=="don't know" and after_answer=="yes"]

    print(f"before yes, after don't know: {before_yes_after_dont_know}")
    print(f"before don't know, after yes: {before_dont_know_after_yes}")

def create_FC_figures(frame):
    frame=frame.copy().rename(columns={"edges":"Edges", "nodes":"Nodes"})
    ax = sns.lineplot(data=frame, x="Nodes", y="FC durations", errorbar=None)
    sns.lineplot(data=frame, x="Edges", y="FC durations", errorbar=None,linestyle="dotted", color=sns.color_palette()[0])

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
    plt.show()

def create_DU_figures(frame):
    frame=frame.copy().rename(columns={"nodes":"Nodes", "edges":"Edges", "width":"Width","amount of backedges":"Backedges"})
    frame_only_unicycles_graphs = frame[frame["has overlapping cycles"]=="no"]
    frame_only_not_unicycles_graphs = frame[frame["has overlapping cycles"]=="yes"]
    
    color_palette = sns.color_palette()

    ax = sns.lineplot(data=frame, x="Nodes", y="DU durations", errorbar=None)
    sns.lineplot(data=frame, x="Edges", y="DU durations", errorbar=None, linestyle="dotted", color=color_palette[0])
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
    plt.show()
    
    sns.lineplot(data=frame, x="Width", y="DU durations", errorbar=None)
    plt.ylabel("Microseconds")
    plt.show()
    sns.lineplot(data=frame, x="Backedges", y="DU durations", errorbar=None)
    plt.ylabel("Microseconds")
    plt.show()

def create_DU_figures2(frame):
    frame_only_unicycles_graphs = frame[frame["has overlapping cycles"]=="no"]
    frame_only_not_unicycles_graphs = frame[frame["has overlapping cycles"]=="yes"]
    
    plot_mean_with_interquartile_range_by(frame, "nodes", "DU durations")
    plt.title("DU runtime by nodes")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by nodes.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_unicycles_graphs, "nodes", "DU durations")
    plt.title("DU runtime by nodes only unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by nodes only unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_not_unicycles_graphs, "nodes", "DU durations")
    plt.title("DU runtime by nodes only non-unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by nodes only non-unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()

    plot_mean_with_interquartile_range_by(frame, "edges", "DU durations")
    plt.title("DU runtime by edges")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by edges.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_unicycles_graphs, "edges", "DU durations")
    plt.title("DU runtime by edges only unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by edges only unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_not_unicycles_graphs, "edges", "DU durations")
    plt.title("DU runtime by edges only non-unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by edges only non-unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()

    plot_mean_with_interquartile_range_by(frame, "width", "DU durations")
    plt.title("DU runtime by width")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by width.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_unicycles_graphs, "width", "DU durations")
    plt.title("DU runtime by width only unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by width only unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_not_unicycles_graphs, "width", "DU durations")
    plt.title("DU runtime by width only non-unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by width only non-unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()

    plot_mean_with_interquartile_range_by(frame, "amount of positions in all cycles", "DU durations")
    plt.title("DU runtime by amount of positions in all cycles")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by amount of positions in all cycles.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_unicycles_graphs, "amount of positions in all cycles", "DU durations")
    plt.title("DU runtime by amount of positions in all cycles only unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by amount of positions in all cycles only unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()
    plot_mean_with_interquartile_range_by(frame_only_not_unicycles_graphs, "amount of positions in all cycles", "DU durations")
    plt.title("DU runtime by amount of positions in all cycles only non-unicycles graphs")
    plt.savefig("/Users/matanshaked/M.Sc research/Thesis/results/figures/DU/DU runtime by amount of positions in all cycles only non-unicycles graphs.png", dpi=300)
    plt.clf()
    plt.cla()

def create_TM_figures(frame):
    cycle_normal_form_frame = frame[frame["is in cycle normal form"] == "yes"]
    not_cycle_normal_form_frame = frame[frame["is in cycle normal form"] == "no"]

    plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "buds", "TM durations")
    plt.ylabel("microseconds")
    plt.show()
    plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "edges", "TM durations")
    plt.ylabel("microseconds")
    plt.show()
    plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "nodes", "TM durations")
    plt.ylabel("microseconds")
    plt.show()
    plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "width", "TM durations")
    plt.ylabel("microseconds")
    plt.show()
    # plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "size of structural connectivity relation", "TM durations")
    # plt.ylabel("microseconds")
    # plt.show()
    # plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "amount of trace manifold graph nodes", "TM durations")
    # plt.ylabel("microseconds")
    # plt.show()
    # plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "amount of trace manifold graph edges", "TM durations")
    # plt.ylabel("microseconds")
    # plt.show()
    
    plot_mean_with_interquartile_range_by(not_cycle_normal_form_frame, "nodes", "TM durations")
    plt.ylabel("microseconds")
    plt.show()
    plot_mean_with_interquartile_range_by(not_cycle_normal_form_frame, "edges", "TM durations")
    plt.ylabel("microseconds")
    plt.show()
    plot_mean_with_interquartile_range_by(not_cycle_normal_form_frame, "buds", "TM durations")
    plt.ylabel("microseconds")
    plt.show()



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
        # ax = sns.lineplot(x=stats.index, y=maxs, label=values_col, linestyle=linestyle)
    else:
        ax = sns.lineplot(x=stats.index, y=means, label=values_col)
        # ax = sns.lineplot(x=stats.index, y=maxs, label=values_col)
    # ax = sns.lineplot(x=stats.index, y=sums, label=values_col)
    if plot_interquirtile_range:
        ax.fill_between(stats.index, quartiles1, quartiles3, alpha=0.3)
        # ax.fill_between(stats.index, mins, maxs, alpha=0.1)


    if show_legend:
        ax.legend()
    ax.set_ylabel(values_col)
    # ax.set_ylabel("duration (microseconds)")
    # ax.set_ylabel("amount of backedges")
    # ax.fill_between(stats.index, mins, maxs, alpha=0.1)


# filespaths_fo = [f"./src/generic/test/run_stats.minimised.fo"]
filespaths_fo = [f"./src/generic/test/run_stats.fo.{i}" for i in range(1,6)]
data_fo = parse_files_multiruns(filespaths_fo)
# filespaths_sl = [f"./src/generic/test/run_stats.minimised.sl"]
filespaths_sl = [f"./src/generic/test/run_stats.sl.{i}" for i in range(1,6)]
data_sl = parse_files_multiruns(filespaths_sl)

data = [
    np.concatenate((data_fo[i], data_sl[i]))
    for i in range(len(data_fo))
]
data = [["fo" for i in range(len(data_fo[0]))]+["sl" for i in range(len(data_sl[0]))]] + data
# filespaths_before = [f"./src/generic/test/deleteme.trace_manifold_before_refactor"]
# data_before = parse_files_multiruns(filespaths_before)
# data_before = ["sl"] + [x for x in data_before]
# filespaths_after = [f"./src/generic/test/deleteme.trace_manifold_after_refactor"]
# data_after = parse_files_multiruns(filespaths_after)
# data = ["sl"] + [x for x in data_after]
# get_graphs_with_different_answers(data_before, data)

test_suite, graph_name, width, buds, edges, nodes, amount_of_backedges, amounts_of_trace_manifold_graph_edges, amounts_of_trace_manifold_graph_nodes, size_of_structural_connectivity_relation, amounts_of_positions_in_all_cycles, amount_of_SCCs, has_overlapping_cycles, is_in_cycle_normal_form, FC_durations, FC_answers, DU_durations, DU_answers, TM_durations, TM_answers, OR_durations, OR_answers = data
frame = pd.DataFrame({
    "test suite": test_suite,
    "graph name": graph_name,
    "width": width,
    "buds": buds,
    "edges": edges,
    "nodes": nodes,
    "amount of backedges": amount_of_backedges,
    "amount of trace manifold graph edges": amounts_of_trace_manifold_graph_edges,
    "amount of trace manifold graph nodes": amounts_of_trace_manifold_graph_nodes,
    "size of structural connectivity relation": size_of_structural_connectivity_relation,
    "amount of positions in all cycles": amounts_of_positions_in_all_cycles,
    "amount of SCCs": amount_of_SCCs,
    "has overlapping cycles": has_overlapping_cycles,
    "is in cycle normal form": is_in_cycle_normal_form,
    "FC durations": FC_durations,
    "FC answers": FC_answers,
    "DU durations": DU_durations,
    "DU answers": DU_answers,
    "TM durations": TM_durations,
    "TM answers": TM_answers,
    "OR durations": OR_durations,
    "OR answers": OR_answers
})


def plot_DU_and_amount_of_backedges_by_edges(frame):
    backedges_means = frame.groupby("edges")["amount of backedges"].mean()
    backedges_stats = frame.groupby("edges")["amount of backedges"].describe()
    DU_durations_means = frame.groupby("edges")["DU durations"].mean()
    DU_durations_stats = frame.groupby("edges")["DU durations"].describe()
    ax = sns.lineplot(x=DU_durations_stats.index, y=DU_durations_means, color="r", label="DU durations", legend=False)
    ax2 = plt.twinx()
    sns.lineplot(x=backedges_stats.index, y=backedges_means, color="b", label="amount of backedges", ax=ax2, legend=False)
    ax.figure.legend()
    plt.show()

def savefig_to(path_inside_figures_dir): 
    plt.savefig(f"{figures_dirpath}/{path_inside_figures_dir}")
    plt.cla()
    plt.clf()

def print_database_stats(frame):
    print(f"max nodes: {frame["nodes"].max()}")
    print(f"max edges: {frame["edges"].max()}")
    print(f"max width: {frame["width"].max()}")
    print(f"max buds: {frame["buds"].max()}")
    print(f"amount in CNF {sum(frame["is in cycle normal form"]=="yes")}")
    print(f"amount in CNF and sound {sum((frame["is in cycle normal form"]=="yes") & (frame["OR answers"]=="yes"))}")

def plot_database_stats(frame):
    # amount_sound = len(frame[frame["OR answers"]=="yes"])
    # amount_unsound = len(frame[frame["OR answers"]=="no"])
    # plt.pie([amount_sound, amount_unsound], labels=["satisfies infinite descent", "does not satisfy infinite descent"])
    # plt.show()

########################

    # sns.boxplot(data=frame, x="edges", y="amount of backedges", hue="test suite", orient="y", whis=(0,100))
    # sns.violinplot(data=frame, x="edges", y="amount of backedges", hue="test suite", split=True, inner="quart", orient="y")
    # savefig_to("database/backedges_by_edges_per_test_suite.violin.png")
    # sns.boxplot(data=frame, x="edges", y="buds", hue="test suite", orient="y", whis=(0,100))
    # sns.violinplot(data=frame, x="edges", y="buds", hue="test suite", split=True, inner="quart", orient="y")
    # savefig_to("database/buds_by_edges_per_test_suite.violin.png")

#########################

    
    # sns.lineplot(data=frame, x="edges", y="buds", hue="test suite")
    # savefig_to("database/buds_by_edges_per_test_suite.lineplot.png")
    
    # sns.lineplot(data=frame, x="edges", y="width", hue="test suite")
    # savefig_to("database/width_by_edges_per_test_suite.lineplot.png")
    
    # sns.lineplot(data=frame, x="edges", y="nodes", hue="test suite")
    # savefig_to("database/nodes_by_edges_per_test_suite.lineplot.png")
    
    new_frame=frame.copy().rename(columns={"nodes":"Nodes", "edges":"Edges","width":"Width", "buds":"Buds", "test suite":"Test suite"})
    new_frame=pd.melt(new_frame, var_name="Metric", value_name="Amount", id_vars=["Edges","Test suite"], value_vars=["Nodes", "Width", "Buds"])
    # sns.lineplot(data=new_frame, x="edges", y="Amount", hue="Metric", style="test suite", style_order=["sl","fo"], palette="icefire")
    sns.lineplot(data=new_frame[new_frame["Test suite"]=="sl"], x="Edges", y="Amount", hue="Metric", palette="icefire")
    savefig_to("database/metrics.sl.relations.lineplot.png")
    sns.lineplot(data=new_frame[new_frame["Test suite"]=="fo"], x="Edges", y="Amount", hue="Metric", palette="icefire")
    savefig_to("database/metrics.fo.relations.lineplot.png")

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
    savefig_to("database/nodes_edges_per_test_suite.kde.png")

    ax = sns.kdeplot(frame_copy, x="Width", hue="Test suite", bw_adjust=2)
    max_width = frame_copy["Width"].max()
    ax.set_xlim(0, max_width)
    ax.set_xticks(range(0,max_width, 4))
    savefig_to("database/width_per_test_suite.kde.png")
    
    ax = sns.kdeplot(frame_copy, x="Buds", hue="Test suite", bw_adjust=2)
    max_buds = frame_copy["Buds"].max()
    ax.set_xlim(0, max_buds)
    ax.set_xticks(range(0,max_buds, 4))
    savefig_to("database/buds_per_test_suite.kde.png")

    # sns.kdeplot(frame_copy, x="Nodes", hue="Test suite", bw_adjust=2)
    # savefig_to("database/nodes_per_test_suite.kde.png")
    # sns.kdeplot(frame_copy, x="Edges", hue="Test suite", bw_adjust=2)
    # savefig_to("database/edges_per_test_suite.kde.png")
    # sns.kdeplot(frame_copy, x="Width", hue="Test suite", bw_adjust=2)
    # savefig_to("database/width_per_test_suite.kde.png")
    # sns.kdeplot(frame_copy, x="Buds", hue="Test suite", bw_adjust=2)
    # savefig_to("database/buds_per_test_suite.kde.png")



def plot_coverages(frame):
    FC_true = len(frame[frame["FC answers"]!="don't know"])
    not_FC_true_DU_true = len(frame[(frame["FC answers"]=="don't know")&(frame["DU answers"]!="don't know")])
    not_FC_nor_DU_true = len(frame[(frame["FC answers"]=="don't know")&(frame["DU answers"]=="don't know")])
    sum_of_amounts = FC_true+not_FC_true_DU_true+not_FC_nor_DU_true
    print(f"amount of graphs {len(frame)}")
    print(f"sum of three amounts {sum_of_amounts}")
    plt.pie([FC_true, not_FC_true_DU_true, not_FC_nor_DU_true], labels=["FC", "DU", "OR"], autopct='%.0f%%', textprops={'fontsize': 16})
    plt.show()

def plot_methods_comparison(frame):

    overhead_frame_only_TM_yes = frame.copy()
    overhead_frame_only_TM_yes = overhead_frame_only_TM_yes[overhead_frame_only_TM_yes["TM answers"]=="yes"]
    overhead_frame_only_TM_yes["OR overhead over TM"] = (overhead_frame_only_TM_yes["OR durations"]-overhead_frame_only_TM_yes["TM durations"])
    overhead_frame_only_TM_yes["OR overhead over TM %"] = 100*(overhead_frame_only_TM_yes["OR durations"]-overhead_frame_only_TM_yes["TM durations"])/overhead_frame_only_TM_yes["TM durations"]
    print(f"min overhead {overhead_frame_only_TM_yes["OR overhead over TM %"].min()}")
    print(f"max overhead {overhead_frame_only_TM_yes["OR overhead over TM %"].max()}")
    plot_mean_with_interquartile_range_by(overhead_frame_only_TM_yes,"edges", "OR overhead over TM %")
    plt.show()

    frame["FC"]=frame["FC durations"]
    frame["DU"]=frame["DU durations"]
    frame["TM"]=frame["TM durations"]
    frame["OR"]=frame["OR durations"]
    plot_mean_with_interquartile_range_by(frame,"edges", "FC")
    plot_mean_with_interquartile_range_by(frame,"edges", "DU",linestyle="--")
    # plot_mean_with_interquartile_range_by(frame,"edges", "TM",linestyle="-.")
    plot_mean_with_interquartile_range_by(frame,"edges", "OR",linestyle=":")
    plt.ylabel("Microseconds")
    plt.xlabel("Edges")
    plt.show()
    # plot_mean_with_interquartile_range_by(frame,"edges", "FC durations")
    # plot_mean_with_interquartile_range_by(frame,"edges", "DU durations")
    # plot_mean_with_interquartile_range_by(frame,"edges", "OR durations")
    # plt.ylabel("microseconds")
    # plt.yscale("log")
    # plt.show()

    # plot_mean_with_interquartile_range_by(frame,"amount of backedges", "FC durations")
    # plot_mean_with_interquartile_range_by(frame,"amount of backedges", "DU durations")
    # plot_mean_with_interquartile_range_by(frame,"amount of backedges", "OR durations")
    # plt.ylabel("microseconds")
    # plt.show()

def print_TM_coverage_after_DU(frame):
    amount_TM_not_DU = len(frame[(frame["DU answers"]=="don't know")&(frame["TM answers"]=="yes")])
    print(f"amount TM not DU: {amount_TM_not_DU}")
    print(f"percent TM not DU among all graphs {100*amount_TM_not_DU/len(frame)}")
    amount_not_FC_nor_DU = len(frame[(frame["DU answers"]=="don't know")&(frame["FC answers"]=="don't know")])
    print(f"percent TM not DU among non covered graphs {100*amount_TM_not_DU/amount_not_FC_nor_DU}")


plt.rc('font', size=16)
plt.rcParams.update({'figure.autolayout': True})

print_database_stats(frame)
exit()

# frame.to_csv("./checkproof_benchmarks/stats.minimized.csv")
plot_methods_comparison(frame)
exit()

# create_FC_figures(frame)
# exit()

# create_DU_figures(frame)
# exit()

# print_database_stats(frame)
# exit()

# plot_database_stats(frame)
# exit()

# plot_coverages(frame)
# exit()
plot_methods_comparison(frame)
exit()

print_TM_coverage_after_DU(frame)
exit()

create_TM_figures(frame)
exit()


amount_sound_DU_dont_know_TM_yes = len(frame[(frame["DU answers"]=="don't know") & (frame["TM answers"]!="don't know")])
amount_sound_only_TM_knows = len(frame[(frame["FC answers"]=="don't know") & (frame["DU answers"]=="don't know") & (frame["TM answers"]!="don't know")])
print(amount_sound_DU_dont_know_TM_yes)
print(amount_sound_only_TM_knows)
plot_mean_with_interquartile_range_by(frame[frame["is in cycle normal form"]=="yes"], "edges", "TM durations")
plot_mean_with_interquartile_range_by(frame[frame["is in cycle normal form"]=="yes"], "edges", "DU durations")
plt.ylabel("microseconds")
plt.show()
exit()

amount_sound = len(frame[frame["OR answers"]=="yes"])
amount_unsound = len(frame[frame["OR answers"]=="no"])
amount_sound_sl = len(frame[(frame["OR answers"]=="yes") & (frame["test suite"]=="sl")])
amount_unsound_sl = len(frame[(frame["OR answers"]=="no") & (frame["test suite"]=="sl")])
amount_sound_fo = len(frame[(frame["OR answers"]=="yes") & (frame["test suite"]=="fo")])
amount_unsound_fo = len(frame[(frame["OR answers"]=="no") & (frame["test suite"]=="fo")])
plt.pie(
    [amount_sound_sl, amount_sound_fo, amount_unsound_sl, amount_unsound_fo],
    labels=["satisfies infinite descent SL","satisfies infinite descent FOL", "does not satisfy infinite descent SL", "does not satisfy infinite descent FOL"],
    colors=["#ff7f0e","#1f77b4","#df9f0e","#3f97b4"],
    hatch=["/","/",".","."]
)
plt.show()
exit()
frame.to_csv("./checkproof_benchmarks/stats.minimized.csv")

plot_methods_comparison(frame)
exit()


plot_methods_comparison(frame)
exit()
plot_coverages(frame)
exit()
print_database_stats(frame)
print_statistics(test_suite, graph_name, width, buds, edges, nodes, amount_of_backedges, amounts_of_trace_manifold_graph_edges, amounts_of_trace_manifold_graph_nodes, has_overlapping_cycles, is_in_cycle_normal_form, FC_durations, FC_answers, DU_durations, DU_answers, TM_durations, TM_answers, OR_durations, OR_answers)
exit()

plot_mean_with_interquartile_range_by(frame, "amount of backedges", "FC durations")
# plot_mean_with_interquartile_range_by(frame, "amount of backedges", "DU durations")
plot_mean_with_interquartile_range_by(frame, "amount of backedges", "TM durations")
# plot_mean_with_interquartile_range_by(frame, "amount of backedges", "OR durations")
plt.show()
exit()
plot_mean_with_interquartile_range_by(frame, "edges", "amount of backedges")
plt.show()
# plot_mean_with_interquartile_range_by(frame[(frame["has overlapping cycles"]=="no")], "edges", "DU durations")
# plt.show()
plot_mean_with_interquartile_range_by(frame, "edges", "DU durations")
plt.show()
exit()
# frame.to_csv("./checkproof_benchmarks/stats.csv")

# print(sum((frame["has overlapping cycles"]=="yes")))
# print(len(frame.index))
# print(sum((frame["has overlapping cycles"]=="yes")&(frame["is in cycle normal form"]=="yes")))
# print(sum((frame["has overlapping cycles"]=="no")&(frame["is in cycle normal form"]=="yes")))
# print(sum((frame["has overlapping cycles"]=="yes")&(frame["is in cycle normal form"]=="no")))
# print(sum((frame["has overlapping cycles"]=="no")&(frame["is in cycle normal form"]=="no")))


# create_DU_figures(frame)
# exit()

cycle_normal_form_frame = frame[frame["is in cycle normal form"] == "yes"]
no_overlapping_cycles_frame = frame[frame["has overlapping cycles"] == "no"]
has_overlapping_cycles_frame = frame[frame["has overlapping cycles"] == "yes"]
cycle_normal_form_and_no_overlapping_cycles_frame = cycle_normal_form_frame[cycle_normal_form_frame["has overlapping cycles"] == "no"]
cycle_normal_form_and_has_overlapping_cycles_frame = cycle_normal_form_frame[cycle_normal_form_frame["has overlapping cycles"] == "yes"]


frame_only_unicycles_graphs = frame[frame["has overlapping cycles"]=="no"]
frame_only_not_unicycles_graphs = frame[frame["has overlapping cycles"]=="yes"]

frame["sum of all methods"] = frame["FC durations"] + frame["DU durations"] + frame["TM durations"]

plot_mean_with_interquartile_range_by(frame, "edges", "sum of all methods")
plot_mean_with_interquartile_range_by(frame, "edges", "OR durations")
plt.show()



# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "width", "DU durations")
# plt.show()

# plot_mean_with_interquartile_range_by(cycle_normal_form_frame,"amount of positions in all cycles", "DU durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame,"amount of positions in all cycles", "TM durations")
# plt.show()
# plot_mean_with_interquartile_range_by(cycle_normal_form_and_no_overlapping_cycles_frame,"amount of positions in all cycles", "DU durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_and_no_overlapping_cycles_frame,"amount of positions in all cycles", "TM durations")
# plt.show()
# plot_mean_with_interquartile_range_by(cycle_normal_form_and_has_overlapping_cycles_frame,"amount of positions in all cycles", "DU durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_and_has_overlapping_cycles_frame,"amount of positions in all cycles", "TM durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_and_has_overlapping_cycles_frame,"amount of positions in all cycles", "OR durations")
# plt.show()

# cycle_normal_form_frame = frame[frame["is in cycle normal form"] == "yes"]
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "amount of trace manifold graph edges", "FC durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "amount of trace manifold graph edges", "DU durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "edges", "TM durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "edges", "OR durations")
# plt.show()

# cycle_normal_form_frame = frame[frame["is in cycle normal form"] == "yes"]
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "edges", "FC durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "edges", "DU durations")
# plot_mean_with_interquartile_range_by(cycle_normal_form_frame, "edges", "TM durations")
# plt.show()

# not_cycle_normal_form_frame = frame[frame["is in cycle normal form"] == "no"]
# plot_mean_with_interquartile_range_by(not_cycle_normal_form_frame, "edges", "FC durations")
# plot_mean_with_interquartile_range_by(not_cycle_normal_form_frame, "edges", "DU durations")
# plot_mean_with_interquartile_range_by(not_cycle_normal_form_frame, "edges", "TM durations")
# plt.show()


##############
# print_statistics(test_suite, graph_name, width, buds, edges, nodes, amount_of_backedges, amounts_of_trace_manifold_graph_edges, amounts_of_trace_manifold_graph_nodes, has_overlapping_cycles, is_in_cycle_normal_form, FC_durations, FC_answers, DU_durations, DU_answers, TM_durations, TM_answers, OR_durations, OR_answers)
##############

# output_to_csv("run.stats.csv", zip(nodes, edges, width, buds, amount_of_backedges, amounts_of_trace_manifold_graph_nodes, amounts_of_trace_manifold_graph_edges, has_overlapping_cycles, is_in_cycle_normal_form, FC_answers, FC_durations, DU_answers, DU_durations, TM_answers, TM_durations, OR_answers, OR_durations, graph_name, test_suite))