from transformers import GPT2Tokenizer, AutoModelForCausalLM
import numpy as np
import re

# adapted from the documentation https://huggingface.co/docs/transformers/main/en/main_classes/text_generation#transformers.GenerationMixin.compute_transition_scores

# text = "You will read a fact about the world and are asked to assess whether an event is likely, given that fact. Fact: Mary is a middle school student. Question: Is it likely that Mary is pregnant? Answer:"
text = "Fact: Mary is taking a prenatal yoga class. Sentence: Tyler asks: Did Jeffrey confess that Mary is pregnant? Question: Does Tyler believe that Mary is pregnant, yes or no?"
tokenizer = GPT2Tokenizer.from_pretrained("gpt2")
model = AutoModelForCausalLM.from_pretrained("gpt2")
tokenizer.pad_token_id = tokenizer.eos_token_id
inputs = tokenizer([text], return_tensors="pt")

#  Print the scores for each token generated with Greedy Search
outputs = model.generate(**inputs, max_new_tokens=5, return_dict_in_generate=True, output_scores=True)
transition_scores = model.compute_transition_scores(
    outputs.sequences, outputs.scores, normalize_logits=True
)
# input_length is the length of the input prompt for decoder-only models, like the GPT family, and 1 for
# encoder-decoder models, like BART or T5.
input_length = 1 if model.config.is_encoder_decoder else inputs.input_ids.shape[1]
generated_tokens = outputs.sequences[:, input_length:]
print(transition_scores[0])
print(tokenizer.decode(generated_tokens[0]))
for tok, score in zip(generated_tokens[0], transition_scores[0]):
    if re.match(r'.*\byes|no\b.*', tokenizer.decode(tok), re.IGNORECASE) is not None:
      print(tokenizer.decode(tok), score.numpy())