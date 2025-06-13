#!/usr/bin/env python3

import pandas as pd
import argparse
import os

def main():
    parser = argparse.ArgumentParser(description="Aggregate CSVs and find max mean row per file")
    parser.add_argument("--inputs", nargs="+", required=True, help="List of CSV file paths to process")
    parser.add_argument("--output", type=str, default="max_mean_summary.csv", help="Path to output summary CSV file")

    args = parser.parse_args()
    summary = []

    for csv_file in args.inputs:
        try:
            df = pd.read_csv(csv_file)
            if 'mean' in df.columns:
                max_row = df.loc[df['mean'].idxmax()].copy()
                max_row['source_file'] = os.path.basename(csv_file)
                summary.append(max_row)
        except Exception as e:
            print(f"Error processing {csv_file}: {e}")

    summary_df = pd.DataFrame(summary)
    summary_df.to_csv(args.output, index=False)
    print(f"Summary written to: {args.output}")

if __name__ == "__main__":
    main()
