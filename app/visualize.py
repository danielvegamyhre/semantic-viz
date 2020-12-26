#!/usr/bin/python3

import os
import sys
import ast
import argparse
import networkx
import matplotlib.pyplot as plt


def toLower(dictData):
    output = {}
    for key, vals in dictData.items():
        k = key.lower()
        output[k] = []
        for val in vals:
            v = val.lower()
            output[k].append(v)
    return output

def main(filename, word1, word2):
    with open(filename, 'r') as fp:
        data = fp.read()

    cleanData = data.split()[1]
    listData = ast.literal_eval(cleanData)
    dictData = dict(listData)
    adjacencyList = toLower(dictData)


    if word1 not in adjacencyList:
        sys.stderr.write("{} not found in semantic graph (you can open app/adjacency_list.txt to see what words exist in the graph)\n".format(word2))
        return -1
    if word2 not in adjacencyList:
        sys.stderr.write("{} not found in semantic graph (you can open app/adjacency_list.txt to see what words exist in the graph)\n".format(word2))
        return -1

    graph = networkx.Graph(adjacencyList)

    for key, vals in adjacencyList.items():
        for v in vals:
            graph.add_edge(key, v)

    pos = networkx.spring_layout(graph, k=0.1, iterations=20)
    networkx.draw(graph, pos, node_color='w', edge_color='k', with_labels=True)
    path = networkx.shortest_path(graph, source=word1, target=word2)
    path_edges = [_ for _ in zip(path, path[1:])]
    networkx.draw_networkx_nodes(graph, pos,nodelist=path,node_color='r')
    networkx.draw_networkx_edges(graph, pos,edgelist=path_edges, edge_color='r', width=5)
    plt.axis('equal')
    plt.show()
    return 0

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog='Graph Visualizer')
    parser.add_argument("filename", help="filename of adjacency list")
    parser.add_argument("word1", help="word 1 (source) for calculating semantic distance")
    parser.add_argument("word2", help="word 2 (target) for calculating semantic distance")
    args = parser.parse_args()
    main(args.filename, args.word1.lower(), args.word2.lower())