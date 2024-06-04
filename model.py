from typing import List
import numpy as np
import pandas as pd
import logging
from openai import OpenAI
from tqdm import tqdm
import argparse

logger = logging.getLogger()
client = OpenAI()

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
        logprobs=True
    )
    generated_answer = prediction.choices[0].message.content # text
    # generated_answer = prediction.choices[0].text # text - 2afc
    raw_probs = prediction.choices[0].logprobs.content # probability of all answers
    # answer_logprobs = [raw_probs[answer] for answer in raw_probs]
    # check if this is a number between 0 and 1, if not re-run it
    print(raw_probs)
    likelihood = raw_probs[0].logprob # should combine the probability of the two tokens? e.g., defintely
    # print(likelihood)

    return generated_answer, likelihood, raw_probs

# 2afc: definitely is divided into two tokens
# system_prompt = "You are a helpful assisant. Your task is to follow the instruction and choose between [impossilbe] and [defintely]. "

# rating elicitation
system_prompt = "You are a helpful assisant. Your task is to follow the instruction and provide a rating between 0 and 1. Your response should only be a number between 0 and 1, with a precision of 0.01."

# models = ["gpt-3.5-turbo","gpt-4","gpt-4o"]

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="prior belief task on gpt3.5-turbo")
    parser.add_argument("--model", "-m", type=str, default="gpt-3.5-turbo")
    args = parser.parse_args()

    prompts = pd.read_csv("prior_rate.csv", header=0)
    # print(prompts.head())

# for model in models:
    for i, row in tqdm(prompts.iterrows()):
        prompt = row.prompt
        generated_answer, likelihood, raw_probs = get_prediction(
            prompt=[{"role" : "system", "content": system_prompt},
                    {"role": "user", "content": prompt}],
            model=args.model
        )
        prompts.loc[i, "rating"] = generated_answer.strip()
        prompts.loc[i, "distribution"] = str(likelihood)
        prompts.loc[i, "raw_probs"] = str(raw_probs)

        prompts.to_csv(f"output_generate_system_{args.model}.csv", index=False)
