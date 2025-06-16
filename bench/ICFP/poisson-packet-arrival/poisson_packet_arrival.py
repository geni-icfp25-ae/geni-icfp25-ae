import argparse
import time

import pyro
import pyro.distributions as dist
import torch
from pyro.infer import MCMC, NUTS, Importance, SMCFilter

def model(args):
    # Sample n from Poisson(10)
    n = pyro.sample("n", dist.Poisson(args.r)).int()
    # Accumulate results
    sum_x = torch.zeros(1)

    for i in range(n):
        if args.combined:
            x_3_3 = pyro.sample("x_3_3", dist.Bernoulli(args.combined)).bool()
        else:
            # Construct the boolean chain
            x_1_1 = True
            x_1_2 = x_1_1 and pyro.sample(f"x_1_2_{i}", dist.Bernoulli(0.5)).bool()
            x_1_3 = x_1_2 and pyro.sample(f"x_1_3_{i}", dist.Bernoulli(0.5)).bool()

            x_2_1 = x_1_1 and pyro.sample(f"x_2_1_{i}", dist.Bernoulli(0.5)).bool()
            x_2_2 = (x_2_1 and pyro.sample(f"x_2_2_{i}_1", dist.Bernoulli(0.5)).bool()) or \
                    (x_1_2 and pyro.sample(f"x_2_2_{i}_2", dist.Bernoulli(0.5)).bool()) or \
                    (x_1_1 and pyro.sample(f"x_2_2_{i}_3", dist.Bernoulli(0.5)).bool())

            x_2_3 = (x_2_2 and pyro.sample(f"x_2_3_{i}_1", dist.Bernoulli(0.5)).bool()) or \
                    (x_1_3 and pyro.sample(f"x_2_3_{i}_2", dist.Bernoulli(0.5)).bool()) or \
                    (x_1_2 and pyro.sample(f"x_2_3_{i}_3", dist.Bernoulli(0.5)).bool())

            x_3_1 = x_2_1 and pyro.sample(f"x_3_1_{i}", dist.Bernoulli(0.5)).bool()
            x_3_2 = (x_3_1 and pyro.sample(f"x_3_2_{i}_1", dist.Bernoulli(0.5)).bool()) or \
                    (x_2_2 and pyro.sample(f"x_3_2_{i}_2", dist.Bernoulli(0.5)).bool()) or \
                    (x_2_1 and pyro.sample(f"x_3_2_{i}_3", dist.Bernoulli(0.5)).bool())

            x_3_3 = (x_3_2 and pyro.sample(f"x_3_3_{i}_1", dist.Bernoulli(0.5)).bool()) or \
                    (x_2_3 and pyro.sample(f"x_3_3_{i}_2", dist.Bernoulli(0.5)).bool()) or \
                    (x_2_2 and pyro.sample(f"x_3_3_{i}_3", dist.Bernoulli(0.5)).bool())

        if args.model == "default":
            pass
        elif args.model == "observation":
            # Observe x_3_3 as evidence
            pyro.sample(f"obs_{i}", dist.Delta(x_3_3), obs=torch.tensor(1.0))

        sum_x += x_3_3

    return sum_x

def sampling(args):
    assert args.model == "default"
    assert args.inference == "sampling"
    sum = 0
    print("nsamples,approx,l1,time")
    start = time.time()
    exact = args.r * args.p
    step = args.num_samples / 100
    for i in range(args.num_samples + 1):
        if i > 0 and i % step == 0:
            now = time.time()
            approx = sum/i
            error = (approx - exact).pow(2).sqrt()
            print(f"{i},{approx.item()},{error.item()},{now-start}")
        sum += model(args)

def inference_IS(args):

    start = time.time()
    importance = Importance(model, num_samples=args.num_samples)
    importance.run(args)
    now = time.time()

    cost_per_sample = (now-start)/args.num_samples
    step = args.num_samples / 100
    exact = args.r * args.p

    log_weights = torch.tensor(importance.log_weights)
    values  = torch.tensor([t.nodes["_RETURN"]["value"].item() for t in importance.exec_traces])

    print("nsamples,approx,l1,time")
    for i in range(args.num_samples + 1):
        if i > 0 and i % step == 0:
            x = values[:i]
            log_w = log_weights[:i]
            weights = (log_w - torch.logsumexp(log_w, 0)).exp()
            approx = (weights * x).sum()
            error = (approx - exact).pow(2).sqrt()
            print(f"{i},{approx.item()},{error.item()},{cost_per_sample * i}")


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, required=True, choices=["default", "observation"])
    parser.add_argument("--inference", type=str, required=True, choices=["sampling", "IS", "mcmc", "smc"])
    parser.add_argument("--combined", type=bool)
    parser.add_argument("--p", type=float, required=True, default=0.6170807)
    parser.add_argument("--r", type=float, required=True, default=10)
    parser.add_argument("--num-samples", type=int, required=True)
    parser.add_argument("--seed", type=int,nargs="?")
    args = parser.parse_args()

    if args.seed is not None:
        pyro.set_rng_seed(args.seed)

    if args.inference == "sampling":
        sampling(args)
    elif args.inference == "IS":
        inference_IS(args)
    else:
        raise ValueError("Invalid mode")
