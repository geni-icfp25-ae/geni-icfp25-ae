{pkgs}:
pkgs.writers.writePython3Bin "plot" { 
    libraries = with pkgs.python3Packages; [ 
        matplotlib
        pandas
    ];
} ''
import argparse
import datetime
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt

"""
Example:
python plot.py \
    --csv dice.csv genfer.csv genfernoss.csv \
    --label dice genfer genfernoss \
    --yscale log
"""


def column(df, key, idx):
    return df[key] if key else df.iloc[:, idx]


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("--csv", type=str, nargs="+", required=True)
    parser.add_argument("--label", type=str, nargs="+", required=True)
    parser.add_argument("--color", type=str, nargs="*")
    parser.add_argument("--title", type=str)
    parser.add_argument("--savefig", type=str)
    parser.add_argument("--ylim", type=float, nargs="*")
    parser.add_argument("--xlim", type=float, nargs="*")
    parser.add_argument("--x", type=str)
    parser.add_argument("--y", type=str)
    parser.add_argument(
        "--yscale",
        type=str,
        choices=['linear', 'log', 'symlog', 'logit'],
        default="linear"
    )
    parser.add_argument(
        "--xscale",
        type=str,
        choices=['linear', 'log', 'symlog', 'logit'],
        default="linear"
    )

    args = parser.parse_args()
    assert len(args.csv) == len(args.label)
    assert args.color is None or len(args.color) == len(args.label)
    assert args.ylim is None or len(args.ylim) == 2
    assert args.xlim is None or len(args.xlim) == 2

    xs = [column(pd.read_csv(file), args.x, 8) for file in args.csv]
    ys = [column(pd.read_csv(file), args.y, 1) for file in args.csv]

    plt.title(args.title)
    plt.xlabel(args.x if args.x else "size")
    plt.ylabel(args.y if args.x else "Time (s)")

    colors = [None] * len(args.label) if args.color is None else args.color
    for x, y, label, color in zip(xs, ys, args.label, colors):
        plt.plot(x, y, label=label, color=color)

    plt.legend()
    if args.ylim is not None:
        plt.ylim(args.ylim[0], args.ylim[1])
    if args.xlim is not None:
        plt.xlim(args.xlim[0], args.xlim[1])
    plt.yscale(args.yscale)
    plt.xscale(args.xscale)

    plt.figtext(
        0.5, 0.2, str((args)),
        ha='center',
        va='top',
        fontsize=8,
        wrap=True
    )
    plt.subplots_adjust(bottom=0.3)

    timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    filename = Path(args.csv[0]).stem if len(args.csv) == 1 else "plot"
    output_filename = args.savefig if args.savefig \
        else f"{filename}_{timestamp}.png"
    plt.savefig(output_filename)
''