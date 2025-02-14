/* Copyright Grant Rostig, License Boost 1.0
   Bad name, should be "permutations" (not "combinations"), also the selection is considered to be "with replacement"
*/

#include <cassert>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <unordered_map>

constexpr int STRING_LEN{49};

auto s(auto i) {return i/2.09;}  // s: Scale down the value by some factor with goal of having all requencies sum to 100% or probability of 1, or thereabouts.

std::unordered_map<char, double> char_occurance_freqs = {  // First 26 values are the letter frequencies in English (in percentage), but we added upper case and more, so number must be scaled down with s().
    {'a', s(8.167)}, {'b', s(1.492)}, {'c', s(2.782)}, {'d', s(4.253)}, {'e', s(12.702)},
    {'f', s(2.228)}, {'g', s(2.015)}, {'h', s(6.094)}, {'i', s(6.966)}, {'j', s(0.153)},
    {'k', s(0.772)}, {'l', s(4.025)}, {'m', s(2.406)}, {'n', s(6.749)}, {'o', s(7.507)},
    {'p', s(1.929)}, {'q', s(0.095)}, {'r', s(5.987)}, {'s', s(6.327)}, {'t', s(9.056)},
    {'u', s(2.758)}, {'v', s(0.978)}, {'w', s(2.360)}, {'x', s(0.150)}, {'y', s(1.974)},
    {'z', s(0.074)},
    {'A', s(8.167)}, {'B', s(1.492)}, {'C', s(2.782)}, {'D', s(4.253)}, {'E', s(12.702)},
    {'F', s(2.228)}, {'G', s(2.015)}, {'H', s(6.094)}, {'I', s(6.966)}, {'J', s(0.153)},
    {'K', s(0.772)}, {'L', s(4.025)}, {'M', s(2.406)}, {'N', s(6.749)}, {'O', s(7.507)},
    {'P', s(1.929)}, {'Q', s(0.095)}, {'R', s(5.987)}, {'S', s(6.327)}, {'T', s(9.056)},
    {'U', s(2.758)}, {'V', s(0.978)}, {'W', s(2.360)}, {'X', s(0.150)}, {'Y', s(1.974)},
    {'Z', s(0.074)},
    {'2', s(0.999)}, {'3', s(0.999)}, {'4', s(0.999)}, {'5', s(0.999)}, {'6', s(0.999)},
    {'7', s(0.999)},
};

int main() {
    //  std::unordered_map<char, double> const letterProbabilities;

    double character_set_radix{static_cast<double>(ssize(char_occurance_freqs))};
    double log_x{log2(character_set_radix)};
    double possible_permutations_with_replacement = pow(character_set_radix, STRING_LEN); // Total possible combinations without considering probabilities

    double total_frequency{0.0};
    for (auto const & pair : char_occurance_freqs) {
        total_frequency += pair.second;
    }

    double entropy{0.0};  // Shannon entropy to be calculated, apparently only an estimate.
    for (const auto& pair : char_occurance_freqs) {
        double probability = pair.second / total_frequency;
        // letterProbabilities[pair.first] = probability;
        assert(probability > 0.0);
        entropy -= probability * log2(probability);
    }
    double total_entropy = STRING_LEN * entropy;
    double effective_permutations = pow(2, total_entropy);

    std::cout << "Radix or number of chars in set:                             " << character_set_radix << std::endl;
    std::cout << "Total Frequency of chars:                                    " << total_frequency << ". Should 100." << std::endl;
    std::cout << "Bits needed for radix:                                       " << log_x << std::endl;
    std::cout << "Entropy per char in random English text frequency on average:" << entropy << " bits" << std::endl;
    std::cout << "Total entropy for char permutations of length "<< STRING_LEN <<": "<< total_entropy << " bits" << std::endl;
    std::cout << "Effective number of char permutations:                       " << std::scientific << std::setprecision(8) << effective_permutations << std::endl;

    std::cout << "Total possible permutations " << character_set_radix <<"^"<<STRING_LEN<<" is: " << possible_permutations_with_replacement << std::endl;
    std::cout << "Total possible permutations 256 bits: 2^256:       "                << std::pow(2,256) << std::endl;

    std::cout << "\nWARNING, my math could be completely wrong/naive. Feedback appreciated." << std::endl;
    std::cout << "###" << std::endl;
    return 0;
}
/* AI output: Understanding the Problem   https://copilot.microsoft.com/chats/XAKYk1ptFdGrKmz2Ghf3H

Prompt by grostig: Write a c++ program that calculates number of possible values created by concatenating 4 alphabetic characters
where the probability of each character is that found in common english words.

Answer: We aim to calculate the number of possible values created by concatenating 4 alphabetic characters, where each character's
probability corresponds to its frequency in common English words. This isn't just a straightforward combinatorial calculation due to
the varying probabilities of each letter.

Frequencies of letters in English. These frequencies determine the likelihood of each letter appearing:

Letter	Frequency (%)
A	8.167
B	1.492
C	2.782
D	4.253
E	12.702
F	2.228
G	2.015
H	6.094
I	6.966
J	0.153
K	0.772
L	4.025
M	2.406
N	6.749
O	7.507
P	1.929
Q	0.095
R	5.987
S	6.327
T	9.056
U	2.758
V	0.978
W	2.360
X	0.150
Y	1.974
Z	0.074
These frequencies can be converted into probabilities by normalizing them so that their sum equals 1.

Approach to the Problem: While there are  26^4 = 456,976
possible combinations of 4-letter strings using the English alphabet, the effective number of combinations considering the letter
probabilities is different. Some combinations are exceedingly rare due to the low probabilities of certain letters.

+Calculate the total number of unique 4-letter combinations, considering the probabilities.
+Assess the effective number of combinations, taking into account the frequency distribution.
+Implement a C++ program that performs these calculations.

The concept of the effective number of combinations can be approached using entropy.
Shannon entropy provides a measure of the uncertainty or surprise associated with random variables—in this case, the letters.

The formula for entropy 𝐻 is:
𝐻 = − ∑ (𝑝 of 𝑖) log(⁡2𝑝 of 𝑖) Where  𝑝 of 𝑖 is the probability of letter 𝑖.

For a sequence of 4 letters, the total entropy:
𝐻 of 𝑡𝑜𝑡𝑎𝑙 = 4 * (𝐻 of letters)
The effective number of combinations:
𝑁 effective = 2^(𝐻 of 𝑡𝑜𝑡𝑎𝑙)

Let's proceed to write a C++ program that calculates:
The entropy of the letter distribution.
The total entropy for 4-letter combinations.
The effective number of combinations.

see above fixed/supplemented/editied by Grant Rostig.

Interpretation of Results
Entropy per letter (~4.176 bits): On average, each letter contains about 4.176 bits of information.

Total entropy (~16.704 bits): A 4-letter combination contains about 16.704 bits of information.

Effective number of combinations (~98,109): This is the number of combinations that effectively carry unique information, considering
the letter probabilities.

Total possible combinations (456,976): This is the maximum number of combinations without considering letter frequencies.

This means that due to the uneven letter distribution in English, the effective number of combinations is significantly less than the
total possible combinations.

Visualizing the Concept
Imagine a forest where each tree represents a 4-letter combination. In a uniformly distributed forest, all trees are equally likely,
spread out evenly across the landscape. However, in our English letter frequency forest, certain areas (combinations with common
letters like 'E' and 'T') are densely packed, while others (combinations with rare letters like 'Q' and 'Z') are sparse.

This uneven distribution reduces the effective diversity of our forest, similar to how the effective number of combinations is less
than the total possible combinations.

Further Exploration
Most Probable Combinations: We can modify the program to list the most probable 4-letter combinations. This could be useful in
cryptography or linguistics.

Random Word Generation: Using the probabilities, we can simulate the generation of random 4-letter words that mimic English word
patterns.

Impact on Data Compression: Understanding letter frequencies is crucial in compression algorithms like Huffman coding, where common
letters are assigned shorter codes.

Implications and Reflections
Delving into letter probabilities isn't just an academic exercise—it has real-world applications in:

Cryptography: Frequency analysis can help decode encrypted messages by exploiting the commonality of certain letters.

Linguistics: Analyzing letter and word frequencies aids in understanding language patterns and evolution.

Information Theory: Concepts like entropy are foundational in measuring information content and designing efficient communication
systems.

Isn't it fascinating how the subtle nuances of letter frequencies can unveil such depth in our understanding of language and
information? This exploration scratches the surface of the intricate dance between probability, language, and computation.
*/
