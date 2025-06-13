import argparse
import functools

parser = argparse.ArgumentParser()
parser.add_argument("--n", type=int, required=True)
parser.add_argument("--m", type=int, required=True)
parser.add_argument("--p-init", type=float, nargs=1, required=True)
parser.add_argument("--p-edge", type=float, nargs=2, required=True)
parser.add_argument("--p-inner", type=float, nargs=4, required=True)

args = parser.parse_args()


header = f"""\
"""

loop = ""
names = []

def line(i, j):
    if i == 0 and j == 0:
        return f"let x_0_0 = sample(Bernoulli({args.p_init[0]})) in"
    elif i == 0:
        return f"let x_{i}_{j} = if x_{i}_{j-1} then sample(Bernoulli({args.p_edge[0]})) else sample(Bernoulli({args.p_edge[1]})) in"
    elif j == 0:
        return f"let x_{i}_{j} = if x_{i-1}_{j} then sample(Bernoulli({args.p_edge[0]})) else sample(Bernoulli({args.p_edge[1]})) in"
    else:
        return f"let x_{i}_{j} = if x_{i}_{j-1} then (if x_{i-1}_{j} then sample(Bernoulli({args.p_inner[0]})) else sample(Bernoulli({args.p_inner[1]}))) else (if x_{i-1}_{j} then sample(Bernoulli({args.p_inner[2]})) else sample(Bernoulli({args.p_inner[3]}))) in"


for i in range(args.n):
    for j in range(args.m):
        names.append(f"x_{i}_{j}")
        loop += f"""\
{line(i,j)}
"""

footer = f"""\
x_{args.n-1}_{args.m-1}
"""

print(header)
print(loop)
print(footer)

