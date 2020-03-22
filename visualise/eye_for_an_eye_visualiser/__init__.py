"""Runs visualisation scripts"""

from os import listdir
import re
import numpy
from matplotlib import pyplot
import argparse

__version__ = "0.1.0"


def is_data_csv(fname):
    """Checks if a file is a data csv"""
    return re.search(r"^iter_(\d+).csv$", fname) is not None


def get_iter_num(fname):
    """Extracts the iteration number from the filename"""
    matches = re.match(r"^iter_(\d+).csv$", fname)

    if matches is None:
        return None

    return matches.group(1)


def list_data_files(name):
    """Lists all the data files in a named directory"""
    files = listdir(name)
    return sorted(filter(is_data_csv, files), key=get_iter_num)


def row_to_obj(row):
    """Converts a list of values from a CSV to a dictionary"""
    return {
        "name": row[0],
        "generosity": row[1],
        "selfishness": row[2],
        "score": row[3],
    }


def read_data_from_file(fpath):
    """Reads the data from a given file"""
    rows = numpy.loadtxt(
        fpath,
        dtype=[
            ("name", numpy.dtype("U10")),
            ("generosity", float),
            ("selfishness", float),
            ("score", float),
        ],
        delimiter=",",
        skiprows=1,
    )
    return [row_to_obj(row) for row in rows]


def load_data(path):
    """Loads all the data for all iterations"""
    return [
        (fname, read_data_from_file(path + "/" + fname))
        for fname in list_data_files(path)
    ]


def norm_factory(v_min, v_max):
    """Creates a normalisation function for scores"""
    v_range = v_max - v_min

    def norm(val):
        return (val - v_min) / v_range

    return norm


def plot_scatter(iteration, agents, output_dir):
    """Generates a plot for one iteration"""
    grouped_agents = {}

    def hash_agent(agent):
        return (
            str(agent["generosity"])
            + "|"
            + str(agent["selfishness"])
        )

    for agent in agents:
        key = hash_agent(agent)
        if key in grouped_agents:
            grouped_agents[key].append(agent)
        else:
            grouped_agents[key] = [agent]

    data = [
        (
            grouped[0]["generosity"],
            grouped[0]["selfishness"],
            numpy.mean(grouped[0]["score"]),
            len(grouped),
        )
        for grouped in grouped_agents.values()
    ]

    score_min = min([agent["score"] for agent in agents])
    score_max = max([agent["score"] for agent in agents])

    if score_max == score_min:
        colours = None
    else:
        score2norm = norm_factory(score_min, score_max)
        colours = [score2norm(datum[2]) * 255 for datum in data]

    count_min = min([datum[3] for datum in data])
    count_max = max([datum[3] for datum in data])

    if count_min == count_max:
        sizes = 50
    else:
        size2norm = norm_factory(count_min, count_max)
        sizes = [size2norm(datum[3]) * 300 + 50 for datum in data]

    x_values = [datum[0] for datum in data]
    y_values = [datum[1] for datum in data]

    pyplot.scatter(
        x_values, y_values, s=sizes, c=colours, vmin=score_min, vmax=score_max
    )

    pyplot.title("Agents for iteration " + str(iteration))
    pyplot.xlabel("generosity")
    pyplot.ylabel("selfishness")

    pyplot.colorbar()
    pyplot.savefig(output_dir + "/scatter_plot_" + str(iteration) + ".png")
    pyplot.close()

    return [data, colours, sizes]


def plot_2d_graphs(x_axis_key, iteration, agents, output_dir):
    """Generates plots of score and children per characteristic"""

    fig, ax_score = pyplot.subplots()

    x_data = [agent[x_axis_key] for agent in agents]

    ax_score.set_xlabel(x_axis_key)
    ax_score.set_ylabel("score", color="tab:red")
    ax_score.scatter(
        x_data,
        [agent["score"] for agent in agents],
        color="tab:red"
    )
    ax_score.tick_params(axis="y", labelcolor="tab:red")

    # TODO: fix plotting of reproductive success. This is complicated by
    # having to match up the x and y axes properly (don't assume they'll
    # match).

    # grouped_agents = {}
    # for agent in agents:
    #     key = agent[x_axis_key]
    #     if key in grouped_agents:
    #         grouped_agents[key].append(agent)
    #     else:
    #         grouped_agents[key] = [agent]

    # ax_repro = ax_score.twinx()
    # ax_repro.set_ylabel("num children", color="tab:blue")
    # ax_repro.plot(
    #     x_data,
    #     [len(group) for group in grouped_agents.values()],
    #     color="tab:blue"
    # )
    # ax_repro.tick_params(axis="y", labelcolor="tab:blue")

    pyplot.title(
        "Plotting score against "
        + x_axis_key
        + ": iteration " + str(iteration)
    )
    fig.tight_layout()
    pyplot.savefig(
        output_dir
        + "/plot_dimension_"
        + x_axis_key + "_"
        + str(iteration) + ".png"
    )
    pyplot.close()


def main():
    """Main function for running this script"""
    parser = argparse.ArgumentParser(description="Plot some data")
    parser.add_argument("path", type=str)
    args = parser.parse_args()
    iterations = load_data(args.path)
    print("Looking in " + args.path)
    print("Found " + str(len(iterations)) + " iterations, visualising them...")
    print("Output files in " + args.path)
    for (fname, agents) in iterations:
        i = get_iter_num(fname)
        print("- plotting " + str(i))
        plot_scatter(i, agents, args.path)
        plot_2d_graphs("selfishness", i, agents, args.path)
        plot_2d_graphs("generosity", i, agents, args.path)


if __name__ == "__main__":
    main()
