#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>

using namespace std;

class GaloisField {
protected: 
    int m;
private:
    vector<int> irreducible;

public:
    GaloisField(int degree, const vector<int>& powers) : m(degree) {
        irreducible.resize(m + 1, 0);
        for (int p : powers) {
            if (p <= m) irreducible[p] = 1;
        }
    }

    vector<int> createElementByPowers(const vector<int>& powers) const {
        vector<int> element(m, 0);
        for (int p : powers) {
            if (p < m && p >= 0) element[p] = 1;
        }
        return element;
    }

    vector<int> createElementByBinaryString(const string& binStr) const {
        vector<int> element(m, 0);
        int len = binStr.length();
        for (int i = 0; i < len && i < m; ++i) {
            if (binStr[len - 1 - i] == '1') {
                element[i] = 1;
            }
        }
        return element;
    }

    vector<int> getZero() const {
        return vector<int>(m, 0);
    }
    vector<int> getOne() const {
        std::vector<int> one(m, 0);
        if (m > 0) {
            one[0] = 1;
        }
        return one;
    }

    vector<int> add(const vector<int>& a, const vector<int>& b) const {
        vector<int> result(m);
        for (int i = 0; i < m; ++i) {
            result[i] = a[i] ^ b[i];
        }
        return result;
    }

    vector<int> multiply(const vector<int>& a, const vector<int>& b) const {
        vector<int> product(2 * m, 0);
        for (int i = 0; i < m; ++i) {
            if (a[i] == 1) {
                for (int j = 0; j < m; ++j) {
                    if (b[j] == 1) product[i + j] ^= 1;
                }
            }
        }

        for (int i = (int)product.size() - 1; i >= m; --i) {
            if (product[i] == 1) {
                int shift = i - m;
                for (int j = 0; j <= m; ++j) {
                    if (irreducible[j] == 1) {
                        product[j + shift] ^= 1;
                    }
                }
            }
        }

        vector<int> result(product.begin(), product.begin() + m);
        return result;
    }

    vector<int> square(const vector<int>& a) const {
        vector<int> expanded(2 * m, 0);
        for (int i = 0; i < m; ++i) {
            if (a[i] == 1) {
                expanded[2 * i] = 1;
            }
        }

        for (int i = (int)expanded.size() - 1; i >= m; --i) {
            if (expanded[i] == 1) {
                int shift = i - m;
                for (int j = 0; j <= m; ++j) {
                    if (irreducible[j] == 1) {
                        expanded[j + shift] ^= 1;
                    }
                }
            }
        }

        return vector<int>(expanded.begin(), expanded.begin() + m);
    }

    int trace(const vector<int>& a) const {
        vector<int> accumulation = a;
        vector<int> current_square = a;

        for (int i = 1; i < m; ++i) {
            current_square = square(current_square);
            accumulation = add(accumulation, current_square);
        }
        
        return accumulation[0];
    }

    vector<int> power(const vector<int>& a, string n_bits) const {
        vector<int> res = getOne();

        for (char bit : n_bits) {
            res = square(res);

            if (bit == '1') {
                res = multiply(res, a);
            }
        }
        return res;
    }

    vector<int> inverse(const vector<int>& a) const {
        bool isZero = true;
        for (int bit : a) {
            if (bit == 1) {
                isZero = false;
                break;
            }
        }
        if (isZero) {
            cerr << "ERROR!!! Zero has no inverse" << endl;
            return getZero();
        }

        // 2^m - 2 -> у бітовий рядок
        string power_bits = "";
        for (int i = 0; i < m - 1; ++i) {
            power_bits += '1';
        }
        power_bits += '0';

        return power(a, power_bits);
    }

    void printAsBinaryString(const vector<int>& element) const {
        if (element.empty()) {
            cout << "0";
        }
        else {
            for (int i = (int)element.size() - 1; i >= 0; --i) {
                cout << element[i];
            }
        }
        cout << endl;
    }

    void printAsPowerList(const vector<int>& element) const {
        cout << "{";
        bool first = true;
        for (int i = (int)element.size() - 1; i >= 0; --i) {
            if (element[i] == 1) {
                if (!first) cout << ", ";
                cout << i;
                first = false;
            }
        }
        if (first) cout << " ";
        cout << "}" << endl;
    }
};

class ONBChecker {
public:
    static bool isPrime(int n) {
        if (n <= 1) return false;
        if (n <= 3) return true;
        if (n % 2 == 0 || n % 3 == 0) return false;
        for (int i = 5; i * i <= n; i = i + 6)
            if (n % i == 0 || n % (i + 2) == 0)
                return false;
        return true;
    }

    // min k: 2^k = 1 (mod p)
    static int getOrderOfTwo(int p) {
        if (p % 2 == 0) return -1;

        long long res = 2;
        int k = 1;
        while (res != 1) {
            res = (res * 2) % p;
            k++;
            if (k > p) return -1;
        }
        return k;
    }

    static void checkONB(int m) {
        int p = 2 * m + 1;
        cout << "Test for m = " << m << " (p = 2m + 1 = " << p << "):" << endl;

        if (!isPrime(p)) {
            cout << "p = " << p << " is not prime. ONB can not exist." << endl;
            return;
        }

        int k = getOrderOfTwo(p);
        cout << "k (min k: 2^k = 1 mod p): " << k << endl;

        bool conditionA = (k == 2 * m);
        bool conditionB = ((p % 4 == 3) && (k == m));

        if (conditionA) {
            cout << "k = 2m. ONB exist." << endl;
        }
        else if (conditionB) {
            cout << "p = 3 (mod 4) and k = m. ONB exist." << endl;
        }
        else {
            cout << "Any conditions are not met. ONB can not exist." << endl;
        }
    }
};

class NormalBasisGF : public GaloisField {
private:
    vector<vector<int>> lambda;

    void precomputeLambda() {
        int p = 2 * m + 1;
        lambda.assign(m, vector<int>(m, 0));

        vector<int> powers(m);
        int current = 1;
        for (int i = 0; i < m; ++i) {
            powers[i] = current;
            current = (current * 2) % p;
        }

        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < m; ++j) {
                int vi = powers[i];
                int vj = powers[j];

                if ((vi + vj) % p == 1 ||
                    (vi - vj + p) % p == 1 ||
                    (-vi + vj + p) % p == 1 ||
                    (-vi - vj + 2 * p) % p == 1) {
                    lambda[i][j] = 1;
                }
            }
        }
    }
public:
    NormalBasisGF(int m) : GaloisField(m, {}) {}

    vector<int> getOne() const {
        return vector<int>(m, 1);
    }

    vector<int> multiply(const vector<int>& a, const vector<int>& b) const {
        if (a.size() < (size_t)m || b.size() < (size_t)m) {
            return vector<int>(m, 0);
        }

        vector<int> c(m, 0);

        for (int k = 0; k < m; ++k) {
            int sum_k = 0;
            for (int i = 0; i < m; ++i) {
                int idx_a = (i + k) % m;
                if (a[idx_a] == 0) continue;

                for (int j = 0; j < m; ++j) {
                    int idx_b = (j + k) % m;

                    if (b[idx_b] == 1 && lambda[i][j] == 1) {
                        sum_k ^= 1;
                    }
                }
            }
            c[k] = sum_k;
        }
        return c;
    }

    vector<int> square(const vector<int>& a) const {
        vector<int> res(m);
        for (int i = 0; i < m; ++i) {
            res[(i + 1) % m] = a[i];
        }
        return res;
    }
};

int main()
{
    int m = 419;
    vector<int> irr = { 419, 21,14, 1, 0 };
    GaloisField gf(m, irr);
    cout << "--- GF(2^" << m << ") ---" << endl;
    vector<int> zero = gf.getZero();
    vector<int> one = gf.getOne();
    cout << "Zero for '+' (in powers): ";
    gf.printAsPowerList(zero);
    cout << "Zero for '+' (in bits): ";
    gf.printAsBinaryString(zero);
    cout << "One for '*' (in powers): ";
    gf.printAsPowerList(one);
    cout << "One for '*' (in bits): ";
    gf.printAsBinaryString(one);
    vector<int> A = gf.createElementByPowers({ 400, 125, 57, 14, 0 });
    vector<int> B = gf.createElementByPowers({ 286, 39, 14, 5, 0 });
    cout << "Element A (in powers): "; gf.printAsPowerList(A); cout << endl;
    cout << "Element A (in bits): "; gf.printAsBinaryString(A); cout << endl;
    cout << "Element B (in powers): "; gf.printAsPowerList(B); cout << endl;
    cout << "Element B (in bits): "; gf.printAsBinaryString(B); cout << endl;
    vector<int> sum = gf.add(A, B);
    cout << "Sum A + B (in powers): "; gf.printAsPowerList(sum); cout << endl;
    cout << "Sum A + B (in bits): "; gf.printAsBinaryString(sum); cout << endl;
    vector<int> mult = gf.multiply(A, B);
    cout << "Mult A * B (in powers): "; gf.printAsPowerList(mult); cout << endl;
    cout << "Mult A * B (in bits): "; gf.printAsBinaryString(mult); cout << endl;
    vector<int> A_2 = gf.square(A);
    cout << "A^2 (in powers): "; gf.printAsPowerList(A_2); cout << endl;
    cout << "A^2 (in bits): "; gf.printAsBinaryString(A_2); cout << endl;
    int tr_A = gf.trace(A);
    cout << "Trace(A) = " << tr_A << endl;
    vector<int> A_13 = gf.power(A, "1101");
    cout << endl;
    cout << "A^13 (in powers): "; gf.printAsPowerList(A_13); cout << endl;
    cout << "A^13 (in bits): "; gf.printAsBinaryString(A_13); cout << endl;
    vector<int> A_inv = gf.inverse(A);
    cout << "A^-1 (in powers): "; gf.printAsPowerList(A_inv); cout << endl;
    cout << "A^-1 (in bits): "; gf.printAsBinaryString(A_inv); cout << endl;
    // Перевірка: A * A_inv = 1
    vector<int> check = gf.multiply(A, A_inv);
    cout << "A * A^-1 (in powers): "; gf.printAsPowerList(check); cout << endl;
    cout << "A * A^-1 (in bits): "; gf.printAsBinaryString(check); cout << endl;
    ONBChecker::checkONB(m);
    NormalBasisGF nbField(m);
    cout << "--- ONBGF(2^" << m << ") ---" << endl;
    vector<int> zero_nb = nbField.getZero();
    vector<int> one_nb = nbField.getOne();
    cout << "Zero for '+' (in powers): ";
    nbField.printAsPowerList(zero_nb);
    cout << "Zero for '+' (in bits): ";
    nbField.printAsBinaryString(zero_nb);
    cout << "One for '*' (in powers): ";
    nbField.printAsPowerList(one_nb);
    cout << "One for '*' (in bits): ";
    nbField.printAsBinaryString(one_nb);
    vector<int> A_nb = nbField.createElementByPowers({ 400, 125, 57, 14, 0 });
    vector<int> B_nb = nbField.createElementByPowers({ 286, 39, 14, 5, 0 });
    cout << "Element A (in powers): "; nbField.printAsPowerList(A_nb); cout << endl;
    cout << "Element A (in bits): "; nbField.printAsBinaryString(A_nb); cout << endl;
    cout << "Element B (in powers): "; nbField.printAsPowerList(B_nb); cout << endl;
    cout << "Element B (in bits): "; nbField.printAsBinaryString(B_nb); cout << endl;
    vector<int> sum_nb = nbField.add(A_nb, B_nb);
    cout << "Sum A + B (in powers): "; nbField.printAsPowerList(sum_nb); cout << endl;
    cout << "Sum A + B (in bits): "; nbField.printAsBinaryString(sum_nb); cout << endl;
    vector<int> mult_nb = nbField.multiply(A_nb, B_nb);
    cout << "Mult A * B (in powers): "; nbField.printAsPowerList(mult_nb); cout << endl;
    cout << "Mult A * B (in bits): "; nbField.printAsBinaryString(mult_nb); cout << endl;
    vector<int> A_2_nb = nbField.square(A_nb);
    cout << "A^2 (in powers): "; nbField.printAsPowerList(A_2_nb); cout << endl;
    cout << "A^2 (in bits): "; nbField.printAsBinaryString(A_2_nb); cout << endl;
}
