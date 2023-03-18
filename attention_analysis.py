import torch
from transformers import GPT2Config, GPT2Tokenizer, GPT2LMHeadModel
from transformers import BertTokenizer, BertModel
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import entropy
import sklearn
from scipy import spatial
import math


#from Bertviz
def format_attention(attention):
   squeezed = []
   for layer_attention in attention:
      if len(layer_attention.shape) != 4:
         raise ValueError("The attention tensor does not have the correct number of dimensions. Make sure you set output_attentions= True when initializing your model.")
      squeezed.append(layer_attention.squeeze(0))
   return torch.stack(squeezed)
# num_layers x num_heads x seq_len x seq_len


#from Bertviz
def format_special_chars(tokens):
   return [t.replace('Ġ', '').replace('▁', ' ').replace('</w>', '') for t in tokens]
'''
def c_similarity(l1,l2):
   #return 1 - spatial.distance.cosine(l1, l2)
   '''
def c_similarity(vector_1,vector_2):
   unit_vector_1 = vector_1 / np.linalg.norm(vector_1)
   unit_vector_2 = vector_2 / np.linalg.norm(vector_2)
   dot_product = np.dot(unit_vector_1, unit_vector_2)
   angle = np.arccos(dot_product)
   return angle

class attention():
   def __init__(self,text,model,tokenizer):
      self.text = text
      self.model = model
      self.tokenizer = tokenizer

      inputs = self.tokenizer.encode_plus(text, return_tensors = 'pt', add_special_tokens = False)
      input_ids = inputs['input_ids']
      attention = model(input_ids)[-1]
      input_ids_list = input_ids[0].tolist()
      tokens = tokenizer.convert_ids_to_tokens(input_ids_list)
      self.tokens = format_special_chars(tokens)
      attn = format_attention(attention)
      
      #from Bertviz
      self.attn_data = {
         "all":{
            "attn":attn.tolist(),
            "left_text":self.tokens,
            "right_text":self.tokens
            }
         }
      self.attn = self.attn_data["all"]["attn"]



   def attn_to_dataframe(self):
      word_i = [i+'_i' for i in self.tokens]
      word_j = [i+'_j' for i in self.tokens]
      df = pd.DataFrame(index = word_i,columns = word_j,dtype=float)
      for word_i in range(len(self.tokens)):
         for word_j in range(len(self.tokens)):
            i_to_j = []
            for layer in range(len(self.attn)):
               for head in range(len(self.attn[0])):
                  i_to_j.append(self.attn[layer][head][word_i][word_j])
            df.iloc[word_i].iloc[word_j] = np.mean(i_to_j)
      return df



   def show_plot(self,df):
      sns.heatmap(df, cmap ='RdYlGn', linewidths = 0.30, annot = True)
      plt.show()

   def entropy_l_v1(self,df, normalized =False) :
      entropy_list = []
      for i in range(len(df)):
         if normalized == True and i!=0:
            entropy_list.append(entropy(df.iloc[i])/np.log(i+1))
         else:
            entropy_list.append(entropy(df.iloc[i]))
      return entropy_list


   def entropy_by_layers(self, normalized=True):
      entropy_by_layer =[]
      layer_len, head_len,token_len = len(self.attn), len(self.attn[0]), len(self.attn[0][0])
      if normalized ==True:
         for layer in range(layer_len):
            layer_entropy = [0]
            for token in range(1,token_len):
               token_entropy=[]
               for head in range(head_len):
                  token_entropy.append(entropy(self.attn[layer][head][token]/np.log(token+1)))
               layer_entropy.append(np.average(token_entropy))
            entropy_by_layer.append(layer_entropy)
      else:
         for layer in range(layer_len):
            layer_entropy = []
            for token in range(token_len):
               token_entropy = []
               for head in range(head_len):
                  token_entropy.append(entropy(self.attn[layer][head][token]))
               layer_entropy.append(np.average(token_entropy))
            entropy_by_layer.append(layer_entropy)

      return entropy_by_layer



   
   def entropy_by_heads(self, normalized=True, version='v1'):
        layer_len, head_len,token_len = len(self.attn), len(self.attn[0]), len(self.attn[0][0])
        entropy_dict = dict()
        if version =='v3' and normalized == True:
           for token in range(token_len):
              layer_dict=dict()
              for layer in range(layer_len):
                 head_list = []
                 for head in range(head_len):
                    head_list.append(entropy(self.attn[layer][head][token][1:])/np.log(token))
                 layer_dict[layer] = head_list
              entropy_dict[token]=layer_dict               
        else:
            for token in range(token_len):
                layer_dict=dict()
                for layer in range(layer_len):
                    head_list = []
                    for head in range(head_len):
                        head_list.append(entropy(self.attn[layer][head][token]/np.log(token+1)))
                    layer_dict[layer]=head_list    
                entropy_dict[token]=layer_dict

                
        return entropy_dict
        #entropy_dict = {"token1":{"layer1":[head1_e,head2_e,head3_e,...],"layer2":[head1_e,head2_e,head3_e,...]}, "token2":{}}                   
      

   #average attention
   def entropy_l_v2(self, normalized = False):
      attn = self.attn
      entropy_from_all =[]
      if normalized == False:
         for layer in range(len(attn)):
            for head in range(len(attn[0])):
               this_head = attn[layer][head]
               entropy_per_word = []
               for i in range(len(self.tokens)):
                  entropy_per_word.append(entropy(this_head[i]))
               entropy_from_all.append(entropy_per_word)
      else:
         for layer in range(len(attn)):
            for head in range(len(attn[0])):
               this_head =attn[layer][head]
               entropy_per_word = []
               for i in range(len(self.tokens)):
                  if i ==0:
                     entropy_per_word.append(0)
                  else:
                     entropy_per_word.append((entropy(this_head[i]))/np.log(i+1))
               entropy_from_all.append(entropy_per_word)
      retVal=[]         
      for k in range(len(self.tokens)):
         retVal.append(np.average([entropy_from_all[i][k] for i in range(len(entropy_from_all))]))
      return retVal



#removing first attention
   def entropy_l_v3(self, normalized = False):
      attn = self.attn
      entropy_from_all =[]
      if normalized == False:
         for layer in range(len(attn)):
            for head in range(len(attn[0])):
               this_head = attn[layer][head]
               entropy_per_word = []
               for i in range(len(self.tokens)):
                  entropy_per_word.append(entropy(this_head[i][1:]))
               entropy_from_all.append(entropy_per_word)
      else:
         for layer in range(len(attn)):
            for head in range(len(attn[0])):
               this_head =attn[layer][head]
               entropy_per_word = []
               for i in range(len(self.tokens)):
                  if i in [0,1]:
                     entropy_per_word.append(0)
                  else:
                     entropy_per_word.append((entropy(this_head[i][1:]))/np.log(i))
               entropy_from_all.append(entropy_per_word)
      retVal=[]         
      for k in range(len(self.tokens)):
         retVal.append(np.average([entropy_from_all[i][k] for i in range(len(entropy_from_all))]))
      return retVal


#removing first attention and standardized based on entropy bias
   def bias_similarity(self):
      attn = self.attn
      length = 'len'+str(len(self.tokens))
      bias = bias_head[length]['mean']
      similarity_from_all =[]
      for layer in range(12):
         for head in range(12):
            this_head, this_bias = attn[layer][head], bias[layer][head]
            similarity_per_word = []
            for i in range(len(self.tokens)):
               similarity = c_similarity(this_head[i], this_bias[i])
               similarity_per_word.append(similarity)
            similarity_from_all.append(similarity_per_word)
      retVal=[]         
      for k in range(len(self.tokens)):
         retVal.append(np.average([similarity_from_all[i][k] for i in range(len(similarity_from_all))]))
      return retVal



   def entropy_list(self,version,normalized=True):
      assert version == 'v1' or 'v2'
      if version =='v1':
         df=self.attn_to_dataframe()
         return self.entropy_l_v1(df=df,normalized = normalized)
      elif version == 'v2':
         return self.entropy_l_v2(normalized = normalized)
      elif version == 'v3':
         return self.entropy_l_v3(normalized = normalized)



   def get_head_attention(self,layer,head):
      #get attention by tokens in a specific head
      attn_set = self.attn[layer][head]
      this_head_frame = pd.DataFrame(attn_set,index = self.tokens, columns = self.tokens)
      return this_head_frame
      

   '''
   def entropy_plot(self,entropy_list):
      plt.plot(entropy_words,entropy_list)
      plt.show()
      '''


