import argparse

def create_problog_program(args):
    lines = []
    
    for x in range(1, args.n+1):
        for y in range(1, args.m+1):
            if x == 1 and y == 1:
                lines.append(f"edge(n_{x}_{y}, n_{x}_{y}).")
            elif x == 1:
                lines.append(f"{args.p}::edge(n_{x}_{y-1}, n_{x}_{y}).")
            elif y == 1:
                lines.append(f"{args.p}::edge(n_{x-1}_{y}, n_{x}_{y}).")
            else:
                lines.append(f"{args.p}::edge(n_{x}_{y-1}, n_{x}_{y}).")
                lines.append(f"{args.p}::edge(n_{x-1}_{y}, n_{x}_{y}).")
                if args.mode == "2-flow":
                    pass
                elif args.mode == "3-flow":
                    lines.append(f"{args.p}::edge(n_{x-1}_{y-1}, n_{x}_{y}).")
                else:
                    raise ValueError(f"Unknown mode: {args.mode}")
    
    lines.append(r"path(X, Y) :- edge(X, Y).")
    lines.append(r"path(X, Y) :- edge(X, Z), Y \= Z, path(Z, Y).")
    lines.append(f"query(path(n_1_1, n_{args.n}_{args.m})).")
    return "\n".join(lines)

def main():
    parser = argparse.ArgumentParser(description='Generate probabilistic grid')
    parser.add_argument('--mode', type=str, choices=['3-flow', "2-flow"])
    parser.add_argument('--n', type=int, help='Number of rows')
    parser.add_argument('--m', type=int, help='Number of columns')
    parser.add_argument('--p', type=float, help='Probability of a cell being occupied')
    args = parser.parse_args()

    text_program = create_problog_program(args)
    print(text_program)

if __name__ == '__main__':
    main()
