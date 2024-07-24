import pandas as pd
from tqdm import tqdm
import random
import argparse
from collections import defaultdict
import os

verbs = ["acknowledge","admit","announce","annoyed","right","confess","confirm","demonstrate","discover",
         "establish","hear","inform","know","pretend","prove","reveal","say","see","suggest","think","polar"]

speaker_names = ["Christopher","Daniel","Tyler","Paul","George","Steven","Kenneth","Edward","Brian","Kevin","Larry","Scott",
                 "Jennifer","Dorothy","Karen","Nancy","Betty","Lisa","Sandra","Ashley","Donna","Kimberly","Cynthia","Kathleen"]
ah_names = ["Ronald","Timothy","Jason","Jeffrey","Gary","Ryan","Nicholas","Eric","Jacob","Jonathan",
            "Carol","Michelle","Emily","Amanda","Melissa","Deborah","Laura","Stephanie","Rebecca","Sharon"]


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="constructing the stimulus set for the projection task")
    parser.add_argument("--input", "-i", type=str, default="stimuli/projection_stimuli.csv")
    parser.add_argument("--output", "-o", type=str, default="stimuli/projection_rate")
    parser.add_argument("--prompt_type", "-p", type=str, default="")
    
    args = parser.parse_args()

    stimuli = pd.read_csv(args.input, header=0)
    full_stimuli = []

    for i, row in tqdm(stimuli.iterrows()):
        embedded_content = row.embedded_content
        if type(embedded_content) == float:
            continue
        negated_content = row.negated
        embedded_type = row.embedded
        prior_type = row.prior_type
        item = row["item"]
        prior = row.prior

        for verb in verbs:
            speaker_name = random.choice(speaker_names)
            ah_name = random.choice(ah_names)
            stimulus = defaultdict()

            if verb == "polar":
                target = speaker_name + " asks: " + row.polar
                question = "Does " + speaker_name + " believe that " + embedded_content + "?"
                if args.prompt_type == "certainty":
                    question = "Is " + speaker_name + " certain that " + embedded_content + "?"
                if args.prompt_type == "adv":
                    continue
                else:
                    alt_question = None
            elif verb == "inform": # need additional person as the obj of inform
                target = speaker_name + " asks: " + "Did " + ah_name + " " + verb + " Jane that " + embedded_content + "?"
                question = "Does " + speaker_name + " believe that " + embedded_content + "?"
                alt_question = "Does " + speaker_name + " believe that " + negated_content + "?"
                if args.prompt_type == "certainty":
                    question = "Is " + speaker_name + " certain that " + embedded_content + "?"
                    alt_question = "Is " + speaker_name + " certain that " + negated_content + "?"
            else:
                aux = "Does" if verb in ["know","think"] else "Is" if verb in ["annoyed","right"] else "Did"
                target = speaker_name + " asks: " + aux + " " + ah_name + " " + verb + " that " + embedded_content + "?"
                question = "Does " + speaker_name + " believe that " + embedded_content + "?"
                alt_question = "Does " + speaker_name + " believe that " + negated_content + "?"
                if args.prompt_type == "certainty":
                    question = "Is " + speaker_name + " certain that " + embedded_content + "?"
                    alt_question = "Is " + speaker_name + " certain that " + negated_content + "?"

            stimulus["verb"] = verb
            stimulus["embedded_type"] = embedded_type
            stimulus["prior_type"] = prior_type
            stimulus["item"] = item
            stimulus["prior"] = prior

            if args.prompt_type == "adv":
                stimulus["prompt"] = "Fact: " + prior + " Sentence: " + target + " Question: " + alt_question
            else:
                stimulus["prompt"] = "Fact: " + prior + " Sentence: " + target + " Question: " + question
            stimulus["target"] = target
            stimulus["question"] = question
            stimulus["alt_question"] = alt_question
            full_stimuli.append(stimulus)

    
    # stimuli.to_csv("test.csv", index=False)
    df = pd.DataFrame(full_stimuli)
    df.to_csv(os.path.join(f"{args.output}_{args.prompt_type}.csv"), index=False)
                    
