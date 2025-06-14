import argparse

def create_gennifer_program(args):
    lines = []
    
    for x in range(1, args.n+1):
        for y in range(1, args.m+1):
            if x == 1 and y == 1:
                lines.append("let conn_1_1 = true in")
            elif x == 1:
                lines.append(f"let conn_{x}_{y} = (conn_{x}_{y-1} && sample(Bernoulli({args.p}))) in")
            elif y == 1:
                lines.append(f"let conn_{x}_{y} = (conn_{x-1}_{y} && sample(Bernoulli({args.p}))) in")
            else:
                if args.mode == "2-flow":
                    lines.append(f"let conn_{x}_{y} = ((conn_{x}_{y-1} && sample(Bernoulli({args.p}))) || (conn_{x-1}_{y} && sample(Bernoulli({args.p})))) in")
                elif args.mode == "3-flow":
                    lines.append(f"let conn_{x}_{y} = ((conn_{x}_{y-1} && sample(Bernoulli({args.p}))) || (conn_{x-1}_{y} && sample(Bernoulli({args.p}))) || (conn_{x-1}_{y-1} && sample(Bernoulli({args.p})))) in")
                else:
                    raise ValueError(f"Unknown mode: {args.mode}")
    
    lines.append(f"conn_{args.n}_{args.m}")
    return "\n".join(lines)

def main():
    parser = argparse.ArgumentParser(description='Generate probabilistic grid')
    parser.add_argument('--mode', type=str, choices=['3-flow', "2-flow"])
    parser.add_argument('--n', type=int,   required=True, help='Number of rows')
    parser.add_argument('--m', type=int,   required=True, help='Number of columns')
    parser.add_argument('--p', type=float, required=True, help='Probability of a cell being occupied')
    args = parser.parse_args()

    text_program = create_gennifer_program(args)
    print(text_program)

if __name__ == '__main__':
    main()
