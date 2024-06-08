import re
import csv
import numpy as np
import functools
from matplotlib import pyplot as plt


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
    minimization_times = np.around(functools.reduce(lambda x,y: x+y, map(lambda contents: np.array(get_minimization_times(contents)), files_contents))/len(filepaths), decimals=4)
    modelcheck_times = np.around(functools.reduce(lambda x,y: x+y, map(lambda contents: np.array(get_modelcheck_times(contents)), files_contents))/len(filepaths), decimals=4)
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
    csv_file = open("minimized.csv", mode='w')
    writer = csv.writer(csv_file)

    writer.writerow(["is sound", "minimization time (ms)", "minimized model check time SH (ms)", "minimized model check time OR (ms)", "minimized model check time FWK (ms)", "minimized model check time VLA (ms)", "minimized model check time SLA (ms)", "amount of nodes", "amount of edges", "amount of backedges", "has overlapping cycles", "amount of positions", "graph width", "filename"])
    for row in table_minimized:
        writer.writerow(row)

    csv_file.close()

    csv_file = open("non-minimized.csv", mode='w')
    writer = csv.writer(csv_file)

    writer.writerow(["is sound", "minimization time (ms)", "non-minimized model check time SH (ms)", "non-minimized model check time OR (ms)", "non-minimized model check time VLA (ms)", "amount of nodes", "amount of edges", "amount of backedges", "has overlapping cycles", "amount of positions", "graph_width", "filename"])
    for row in table_nonminimized:
        writer.writerow(row)

    csv_file.close()

    # minimization_table = [(*SH_tuple, OR_tuple[2], *t2) for SH_tuple, OR_tuple,t2 in zip(parsed_minimization_SH, parsed_minimization_OR, parsed_stats)]
    # no_minimization_table = [(*SH_tuple, OR_tuple[2], *t2) for SH_tuple, OR_tuple,t2 in zip(parsed_no_minimization_SH, parsed_no_minimization_OR, parsed_stats)]

    # csv_file = open("minimization.csv", mode='w')
    # writer = csv.writer(csv_file)

    # writer.writerow(["is sound", "minimization time (ms)", "model check time with Sledgehammer (ms)", "model check time with Order Reduced (ms)", "amount of nodes", "amount of edges", "amount of backedges", "amount of positions", "filename"])
    # for row in minimization_table:
    #     writer.writerow(row)

    # csv_file.close()

    # csv_file = open("no_minimization.csv", mode='w')
    # writer = csv.writer(csv_file)

    # writer.writerow(["is sound", "minimization time (ms)", "model check time with Sledgehammer (ms)", "model check time with Order Reduced (ms)", "amount of nodes", "amount of edges", "amount of backedges", "amount of positions", "filename"])
    # for row in no_minimization_table:
    #     writer.writerow(row)

def to_numeric(table):
    return [
        (
            float(1 if tup[0] else 0),
            float(tup[1]),
            float(tup[2]),
            float(tup[3]),
            float(tup[4]),
            float(tup[5]),
            float(tup[6]),
            float(tup[7]),
            float(1 if tup[8] else 0),
            float(tup[9]),
            float(tup[10])
        )
        for tup in table
    ]

from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
def analyze_table(table):
    numric_table = to_numeric(table)
    matrix = np.array(numric_table)

    kmeans = KMeans(n_clusters=5)
    kmeans.fit(matrix)

    cluster_centers = kmeans.cluster_centers_

    # Get the labels assigned to each data point
    labels = kmeans.labels_

    # Count the number of points in each cluster
    unique, counts = np.unique(labels, return_counts=True)
    points_per_cluster = dict(zip(unique, counts))

    print("Number of points in each cluster:")
    for cluster, count in points_per_cluster.items():
        print(f"Cluster {cluster}: {count} points")
    print([
        [
            round(float(entry), 5) for entry in component
        ]
        for component in kmeans.cluster_centers_
    ])

    # pca = PCA(n_components=5)
    # print("calculating PCs...")
    # pca.fit(matrix)
    # print(pca.explained_variance_ratio_)
    # print([
    #     [
    #         round(float(entry), 5) for entry in component
    #     ]
    #     for component in pca.components_
    # ])
    

minimization_filepaths_SH = [f"./logs/results.minimization.SH.{i}" for i in range(1,6)]
no_minimization_filepaths_SH = [f"./logs/results.no_minimization.SH.{i}" for i in range(1,6)]
minimization_filepaths_OR = [f"./logs/results.minimization.OR.{i}" for i in range(1,6)]
no_minimization_filepaths_OR = [f"./logs/results.no_minimization.OR.{i}" for i in range(1,6)]
minimization_filepaths_FWK = [f"./logs/results.minimization.FWK.{i}" for i in range(1,6)]
# no_minimization_filepaths_FWK = [f"./logs/results.no_minimization.FWK.{i}" for i in range(1,6)]
minimization_filepaths_VLA = [f"./logs/results.minimization.VLA.{i}" for i in range(1,3)]
no_minimization_filepaths_VLA = [f"./logs/results.no_minimization.VLA.{i}" for i in range(1,3)]
minimization_filepaths_SLA = [f"./logs/results.minimization.SLA.{i}" for i in range(1,3)]
# no_minimization_filepaths_SLA = [f"./logs/results.no_minimization.SLA.{i}" for i in range(1,3)]

parsed_minimization_unzipped_SH = parse_files(minimization_filepaths_SH)
parsed_no_minimization_unzipped_SH = parse_files(no_minimization_filepaths_SH)
parsed_minimization_unzipped_OR = parse_files(minimization_filepaths_OR)
parsed_no_minimization_unzipped_OR = parse_files(no_minimization_filepaths_OR)
parsed_minimization_unzipped_FWK = parse_files(minimization_filepaths_FWK)
parsed_minimization_unzipped_VLA = parse_files(minimization_filepaths_VLA)
parsed_no_minimization_unzipped_VLA = parse_files(no_minimization_filepaths_VLA)
parsed_minimization_unzipped_SLA = parse_files(minimization_filepaths_SLA)


table_minimized, table_nonminimized = generate_tables(parsed_minimization_unzipped_SH, parsed_no_minimization_unzipped_SH, parsed_minimization_unzipped_OR, parsed_no_minimization_unzipped_OR, parsed_minimization_unzipped_FWK, parsed_minimization_unzipped_VLA, parsed_no_minimization_unzipped_VLA, parsed_minimization_unzipped_SLA)
# analyze_table(table_minimized)
# analyze_table(table_nonminimized)
output_to_csv(table_minimized, table_nonminimized)

