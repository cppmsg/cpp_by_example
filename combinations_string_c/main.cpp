/* https://chat.deepseek.com/a/chat/s/54f115eb-2d4e-4425-b9e0-ad67c0aa7c7f
 */
#include <iostream>
#include <cmath>
#include <array>
#include <iomanip>

using namespace std;

int main() {
    // Letter frequencies (a-z) based on English, as percentages
    array<double, 26> probabilities = {
        0.082, // A
        0.015, // B
        0.028, // C
        0.043, // D
        0.127, // E
        0.022, // F
        0.020, // G
        0.059, // H
        0.067, // I
        0.002, // J
        0.008, // K
        0.039, // L
        0.024, // M
        0.063, // N
        0.075, // O
        0.019, // P
        0.001, // Q
        0.051, // R
        0.061, // S
        0.091, // T
        0.028, // U
        0.010, // V
        0.024, // W
        0.002, // X
        0.020, // Y
        0.001  // Z
    };

           // Calculate sum of probabilities
    double sum = 0.0;
    for (double p : probabilities) {
        sum += p;
    }

           // Normalize probabilities to sum to 1
    for (double& p : probabilities) {
        p /= sum;
    }

           // Calculate entropy per character
    double entropy = 0.0;
    for (double p : probabilities) {
        if (p > 0.0) {
            entropy -= p * log2(p);
        }
    }

           // Total entropy for 4 characters
    double total_entropy = 4 * entropy;

           // Effective number of possibilities
    double effective = pow(2, total_entropy);

    cout << fixed << setprecision(0);
    cout << "Effective number of possible 4-character combinations: " << effective << endl;

    return 0;
}
/*  https://chat.deepseek.com/a/chat/s/54f115eb-2d4e-4425-b9e0-ad67c0aa7c7f
To solve this problem, we need to calculate the effective number of possible 4-letter combinations considering the probability distribution of each letter in common English words. This is done using information theory, specifically by computing the entropy of the letter distribution and then using it to determine the effective number of combinations.

### Approach
1. **Letter Frequencies**: Use the known probabilities of each letter in the English language.
2. **Normalization**: Ensure the probabilities sum to 1 for accurate entropy calculation.
3. **Entropy Calculation**: Compute the entropy per character using the formula \( H = -\sum (p_i \log_2 p_i) \).
4. **Total Entropy**: Multiply the per-character entropy by 4 (for 4 characters).
5. **Effective Combinations**: Calculate the effective number of combinations using \( 2^{4H} \), where \( H \) is the entropy per character.

### Solution Code
```cpp
#include <iostream>
#include <cmath>
#include <array>
#include <iomanip>

using namespace std;

int main() {
 // Letter frequencies (a-z) based on English, as percentages
 array<double, 26> probabilities = {
     0.082, // A
     0.015, // B
     0.028, // C
     0.043, // D
     0.127, // E
     0.022, // F
     0.020, // G
     0.059, // H
     0.067, // I
     0.002, // J
     0.008, // K
     0.039, // L
     0.024, // M
     0.063, // N
     0.075, // O
     0.019, // P
     0.001, // Q
     0.051, // R
     0.061, // S
     0.091, // T
     0.028, // U
     0.010, // V
     0.024, // W
     0.002, // X
     0.020, // Y
     0.001  // Z
 };

 // Calculate sum of probabilities
 double sum = 0.0;
 for (double p : probabilities) {
     sum += p;
 }

 // Normalize probabilities to sum to 1
 for (double& p : probabilities) {
     p /= sum;
 }

 // Calculate entropy per character
 double entropy = 0.0;
 for (double p : probabilities) {
     if (p > 0.0) {
         entropy -= p * log2(p);
     }
 }

 // Total entropy for 4 characters
 double total_entropy = 4 * entropy;

 // Effective number of possibilities
 double effective = pow(2, total_entropy);

 cout << fixed << setprecision(0);
 cout << "Effective number of possible 4-character combinations: " << effective << endl;

 return 0;
}
```

### Explanation
1. **Letter Frequencies**: The array `probabilities` holds the relative frequencies of each letter in English, converted from percentages to decimal form.
2. **Normalization**: The sum of the initial probabilities might not be exactly 1, so we normalize each probability to ensure their sum is 1.
3. **Entropy Calculation**: Entropy measures the uncertainty or randomness. For each letter, we compute \( p_i \log_2 p_i \) and sum these values to get the entropy.
4. **Effective Combinations**: The entropy for 4 characters is 4 times the per-character entropy. The effective number of combinations, which accounts for the likelihood of each combination, is \( 2^{4H} \).

This approach leverages information theory to provide a realistic estimate of the number of probable 4-letter combinations based on English letter frequencies.
*/
