{pkgs}:

pkgs.writers.writePython3Bin "plot-csv" { libraries = [ pkgs.python3Packages.matplotlib ]; } ''
import pandas as pd
import matplotlib.pyplot as plt
import argparse
import datetime


def main():
    parser = argparse.ArgumentParser(description="Plot data from a CSV file.")
    parser.add_argument("file", type=str, help="Path to the CSV file.")
    parser.add_argument("--x", type=str, required=True)
    parser.add_argument("--y", type=str, required=True)
    parser.add_argument("--savefig", type=str)
    args = parser.parse_args()

    # Read CSV file
    df = pd.read_csv(args.file)

    # Check if columns exist
    if args.x not in df.columns or args.y not in df.columns:
        print("Error: One or both column names not found in CSV file.")
        return

    # Plot data
    plt.figure(figsize=(10, 5))
    plt.plot(df[args.x], df[args.y], marker='o', linestyle='-')
    plt.xlabel(args.x)
    plt.ylabel(args.y)
    plt.title(f'{args.y} vs {args.x}')
    plt.grid()

    output_filename = \
        args.savefig if args.savefig is not None else \
        "plot_" \
        + str(datetime.datetime.now().date()) \
        + '_' \
        + str(datetime.datetime.now().time()).replace(':', '.') \
        + ".png"

    plt.savefig(output_filename)


if __name__ == "__main__":
    main()
''
