# Semantic Distance Visualizer written in Haskell

This project visualizes the semantic distance between 2 words in a given hyponym by querying the Wordnet lexical database. Wordnet is a large lexical database of English. Nouns, verbs, adjectives and adverbs are grouped into sets of cognitive synonyms (synsets), each expressing a distinct concept.

Next, the program parses the Wordnet output into an adjacency list represented an undirected graph of the semantic relationships between the words in the given hyponym. 

Finally, the shortest path between the source word and target word is computed and visualized for the user (
see example screenshot below).


## Demo
<img src="static/demo.png" alt="ls /proc" width="900">

## Dependenices
- Tested on Ubuntu 20.0.4
- Wordnet: https://wordnet.princeton.edu/download/current-version
- Haskell: `$ sudo apt-get install haskell-platform`
- The Haskell Tool Stack: https://docs.haskellstack.org/en/stable/install_and_upgrade/
- Python 3.7+: https://www.python.org/downloads/source/

## Building and Executing

From the `semantic-viz` directory, run the command `stack build && stack exec semantic-viz-exe` to build and run the project.

Follow the prompts to run the program. It will prompt you for:

1. The word to query Wordnet for (hyponym)
2. The source word and target word you want to visualize the semantic distance between.
