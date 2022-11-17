#!/usr/bin/env python
# coding: utf-8

# In[1]:


# given function: display cards  
import ipywidgets as widgets
from IPython.display import Image, HTML, display


# open images files
def card_display(card_names):
    """
     - input:   card_names (a list, all names in low cases). 
                example: card_names = ["10_of_hearts", "jack_of_clubs", "ace_of_diamonds"]

     - images:  saved in your current directory/deck/
     - display: side by side
     - return:  none

    """    
    length = len(card_names)

    if length == 0:
        print(" There is no card!")
    else:
        
        # create a list for display
        card_list = []

        # read in cards one by one and store in card_list
        for i in range(length):
            # path + card + .png
            path_name = './deck/'+ card_names[i] + '.png'

            img = open(path_name, 'rb').read()
            img_resize = widgets.Image(value= img, format='png', width=60, height=40)
            card_list.append(img_resize)

        # Side by side widges in HBox widgets
        sidebyside = widgets.HBox(card_list)

        # display
        display(sidebyside)

#-------   TEST  ---------------------

# # for one card
# card_display(["queen_of_clubs"])

# # for two card
# card_display(["jack_of_clubs", "ace_of_diamonds"])

# for three card
# card_display(["10_of_hearts", "jack_of_clubs", "ace_of_diamonds"])

