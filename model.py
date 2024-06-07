import numpy as np
import pandas as pd
import logging
from openai import OpenAI
from tqdm import tqdm
import argparse
# import os

logger = logging.getLogger()
client = OpenAI()

prior_info = "You will read a fact about the world and are asked to assess how likely an event is, given that fact. Please provide a rating from 0 to 1, where 0 means impossible and 1 means definitely. "
projection_info = "In this experiment, we ask you to imagine that you are at a party. You walk into the kitchen and overhear somebody ask another person a question. In addition to the question, we also tell you a fact to consider. You will assess whether the speaker believes in something, given what the speaker asks. Please provide a rating from 0 to 1, where 0 means the speakers definitely doesn't believe it and 1 means the speaker definitely believes it. "
# projection_info = "You will read a sentence in which a person is asking about something. In addition to the question, we also tell you a fact to consider. You will assess whether the speaker believes in something, given what the speaker asks. Please provide a rating from 0 to 1, where 0 means the speakers definitely doesn't believe it and 1 means the speaker definitely believes it. "


# add a system prompt: only provide numerical ratings
def softmax(x):
    return np.exp(x)/sum(np.exp(x))

def get_prediction(prompt, model):
    # prob = []
    prediction = client.chat.completions.create(
        model = model,
        messages = prompt,
        temperature = 0,
        max_tokens = 5,
        logprobs=True,
        top_logprobs=5
    )
    generated_answer = prediction.choices[0].message.content # text
    # generated_answer = prediction.choices[0].text # text - 2afc
    raw_probs = prediction.choices[0].logprobs.content # probability of all answers
    # check if this is a number between 0 and 1, if not re-run it
    likelihood = raw_probs[0].logprob # should combine the probability of the two tokens? e.g., defintely
    # print(likelihood)

    return generated_answer, likelihood, raw_probs

# 2afc: definitely is divided into two tokens
# system_prompt = "You are a helpful assisant. Your task is to follow the instruction and choose between [impossilbe] and [defintely]. "

# rating elicitation
system_prompt = "You are a helpful assisant. Your task is to follow the instruction and provide a rating between 0 and 1. Your response should only be a number between 0 and 1, with a precision of 0.01."

# models = ["gpt-3.5-turbo","gpt-4","gpt-4o"]

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="prior belief and projection inference on gpt3.5-turbo")
    parser.add_argument("--model", "-m", type=str, default="gpt-3.5-turbo")
    parser.add_argument("--task", "-t", type=str, default="prior")
    parser.add_argument("--input", "-i", type=str, default="stimuli/prior_rate.csv")
    # parser.add_argument("--output_dir", "-o", type=str, default="data/prior")
    args = parser.parse_args()

    prompts = pd.read_csv(args.input, header=0)
    # if args.task == "prior":
    #     prompts = pd.read_csv("prior_rate.csv", header=0)
    # elif args.task == "projection":
    #     prompts = pd.read_csv("projection_rate.csv", header=0)
    # print(prompts.head())

# for model in models:
    for i, row in tqdm(prompts.iterrows()):
        prompt = row.prompt
        if args.task == "prior":
            prompt = prior_info + prompt
        elif args.task == "projection":
            prompt = projection_info + prompt
            # print(prompt)
        else:
            print("wrong task")
        generated_answer, likelihood, raw_probs = get_prediction(
            prompt=[{"role" : "system", "content": system_prompt},
                    {"role": "user", "content": prompt}],
            model=args.model
        )
        prompts.loc[i, "rating"] = generated_answer.strip()
        prompts.loc[i, "distribution"] = str(likelihood)
        prompts.loc[i, "raw_probs"] = str(raw_probs)

        prompts.to_csv(f"{args.task}_generate_system_{args.model}.csv", index=False)

        # prompts.to_csv(os.path.join(args.output_dir,f"{args.task}_generate_system_{args.model}.csv"), index=False)