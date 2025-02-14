/* https://claude.ai/chat/cbd4ffb7-727e-4fb6-94cc-b3084bb16492
   I think this is Claude AI is junk compared to "combinations_string_a" project's code.
*/
#include <cmath>
#include <chrono>
#include <iomanip>
#include <iostream>
#include <unordered_map>

using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; using std::string;  // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;

class CharacterProbabilityCalculator {  // https://claude.ai/chat/cbd4ffb7-727e-4fb6-94cc-b3084bb16492
    std::unordered_map<char, double> letter_frequencies = { // Frequency of letters in typical English text (based on comprehensive linguistic studies)
        {'e', 0.1202}, {'t', 0.0910}, {'a', 0.0812}, {'o', 0.0768},  {'i', 0.0731}, {'n', 0.0695}, {'s', 0.0628}, {'r', 0.0602},
        {'h', 0.0592}, {'d', 0.0432}, {'l', 0.0398}, {'u', 0.0288},  {'c', 0.0271}, {'m', 0.0261}, {'f', 0.0230}, {'y', 0.0211},
        {'w', 0.0209}, {'g', 0.0203}, {'p', 0.0182}, {'b', 0.0149},  {'v', 0.0111}, {'k', 0.0069}, {'x', 0.0017}, {'q', 0.0011},
        {'j', 0.0010}, {'z', 0.0007}
    };
public: // Calculate total unique combinations possible
    unsigned long long calculateUniqueCombinations() { return std::pow(26, 4); }
    double calculateProbabilisticCombinations() {                       // Calculate unique combinations based on letter frequencies
        double total_probability = 0.0;
        for (const auto& firstLetter : letter_frequencies) {            // Calculate unique combinations by considering individual letter probabilities
            for (const auto& secondLetter : letter_frequencies) {
                for (const auto& thirdLetter : letter_frequencies) {
                    for (const auto& fourthLetter : letter_frequencies) {
                        // Multiply individual letter probabilities
                        double combination_prob =
                            firstLetter.second *
                            secondLetter.second *
                            thirdLetter.second *
                            fourthLetter.second;

                        total_probability += combination_prob;
                    }
                }
            }
        }
        return total_probability;
    }
};

int main() {
    CharacterProbabilityCalculator calculator;
    unsigned long long             total_combinations         = calculator.calculateUniqueCombinations();
    double                         probabilistic_combinations = calculator.calculateProbabilisticCombinations();
    std::cout << "Character Probability Analysis for 4-Character Concatenations\n";
    std::cout << "----------------------------------------------------\n";
    std::cout << "Total Theoretical Combinations: " << total_combinations << std::endl;
    std::cout << "Probabilistic Unique Combinations: " << std::fixed << std::setprecision(4) << probabilistic_combinations
              << std::endl;
    std::cout << "Percentage of Likely Combinations: " << std::fixed << std::setprecision(10)
              << (probabilistic_combinations / total_combinations * 100.0) << "%" << std::endl;
    cout << "#" << endl;
    return 0;
}
