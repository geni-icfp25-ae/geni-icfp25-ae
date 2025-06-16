import argparse

def print_flow_mvar(n, m, p):
    for x in range(1, n+1):
        print(f"# row {x}")
        for y in range(1, m+1):
            print(f"# cell {x} {y}")
            if x == 1 and y == 1:
                print(f"conn_1 := 1;")
            elif x == 1:
                print(f"if conn_{y-1} = 1 {{")
                print(f"    conn_{y} ~ Bernoulli({p});")
                print(f"}} else {{")
                print(f"    conn_{y} := 0;")
                print(f"}}")
            elif y == 1:
                print(f"if conn_1 = 1 {{")
                print(f"   new ~ Bernoulli({p});")
                print(f"}} else {{")
                print(f"   new := 0;")
                print(f"}}")
            else:
                print(f"if conn_{y} = 1 {{")
                print(f"    up ~ Bernoulli({p});")
                print(f"}} else {{")
                print(f"    up := 0;")
                print(f"}}")
                print(f"if conn_{y-1} = 1 {{")
                print(f"    diag ~ Bernoulli({p});")
                print(f"}} else {{")
                print(f"    diag := 0;")
                print(f"}}")
                print(f"if new = 1 {{")
                print(f"    left ~ Bernoulli({p});")
                print(f"}} else {{")
                print(f"    left := 0;")
                print(f"}}")
                print(f"conn_{y-1} := new;")
                print(f"if up = 1 or diag = 1 or left = 1 {{")
                print(f"    new := 1;")
                print(f"}} else {{")
                print(f"    new := 0;")
                print(f"}}")
                if y == m:
                    print(f"conn_{y} := new;")
            print("")
    print(f"return conn_{m}")

def print_flow(args):
    for x in range(1, args.n+1):
        print(f"# row {x}")
        for y in range(1, args.m+1):
            print(f"# cell {x} {y}")
            if x == 1 and y == 1:
                print(f"conn_1_1 := 1;")
            elif x == 1:
                print(f"if conn_1_{y-1} = 1 {{")
                print(f"    conn_1_{y} ~ Bernoulli({args.p});")
                print(f"}} else {{")
                print(f"    conn_1_{y} := 0;")
                print(f"}}")
            elif y == 1:
                print(f"if conn_{x-1}_1 = 1 {{")
                print(f"   conn_{x}_1  ~ Bernoulli({args.p});")
                print(f"}} else {{")
                print(f"   conn_{x}_1  := 0;")
                print(f"}}")
            else:
                print(f"if conn_{x-1}_{y} = 1 {{")
                print(f"    up ~ Bernoulli({args.p});")
                print(f"}} else {{")
                print(f"    up := 0;")
                print(f"}}")
                if args.mode == "2-flow":
                    pass
                elif args.mode == "3-flow":
                    print(f"if conn_{x-1}_{y-1} = 1 {{")
                    print(f"    diag ~ Bernoulli({args.p});")
                    print(f"}} else {{")
                    print(f"    diag := 0;")
                    print(f"}}")
                else:
                    raise ValueError(f"Unknown mode: {args.mode}")
                print(f"if conn_{x}_{y-1} = 1 {{")
                print(f"    left ~ Bernoulli({args.p});")
                print(f"}} else {{")
                print(f"    left := 0;")
                print(f"}}")
                if args.mode == "2-flow":
                    print(f"if up = 1 or left = 1 {{")
                elif args.mode == "3-flow":
                    print(f"if up = 1 or diag = 1 or left = 1 {{")
                else:
                    raise ValueError(f"Unknown mode: {args.mode}")
                print(f"    conn_{x}_{y} := 1;")
                print(f"}} else {{")
                print(f"     conn_{x}_{y} := 0;")
                print(f"}}")
            print("")
    print(f"return conn_{args.n}_{args.m}")

def main():
    parser = argparse.ArgumentParser(description='Generate probabilistic grid')
    parser.add_argument('--mode', type=str, choices=['3-flow', "2-flow", 'flow-mvar'])
    parser.add_argument('--n', type=int,   required=True, help='Number of rows')
    parser.add_argument('--m', type=int,   required=True, help='Number of columns')
    parser.add_argument('--p', type=float, required=True, help='Probability of a cell being occupied')
    args = parser.parse_args()

    if args.mode == '2-flow':
        print_flow(args)
    elif args.mode == '3-flow':
        print_flow(args)
    elif args.mode == 'flow-mvar':
        print_flow_mvar(args.n, args.m, args.p)
    else:
        raise ValueError(f"Invalid mode {args.mode}")

if __name__ == '__main__':
    main()
