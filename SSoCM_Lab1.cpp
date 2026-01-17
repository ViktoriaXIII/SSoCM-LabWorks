#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
#include <sstream>
#include <chrono>
#include <random>

using namespace std;
using namespace std::chrono;

class BigInt {
private:
    // 2048 = 64 блоки по 32 біт
    vector<uint32_t> digits;
    static const uint64_t BASE = 0x100000000ULL; // 2^32

    void remove_leading_zeros() {
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
    }

    uint32_t divInt(uint32_t v) {
        uint64_t rem = 0;
        for (int i = static_cast<int>(digits.size()) - 1; i >= 0; --i) {
            uint64_t cur = digits[i] + rem * BASE;
            digits[i] = static_cast<uint32_t>(cur / v);
            rem = cur % v;
        }
        remove_leading_zeros();
        return static_cast<uint32_t>(rem);
    }

    void multiplyInt(uint64_t v) {
        if (v == 0) { *this = 0; return; }
        if (v == 1) return;

        uint64_t carry = 0;
        for (size_t i = 0; i < digits.size() || carry; ++i) {
            if (i == digits.size()) digits.push_back(0);
            uint64_t cur = carry + digits[i] * 1ULL * v;
            digits[i] = static_cast<uint32_t>(cur & 0xFFFFFFFF);
            carry = cur >> 32;
        }
        remove_leading_zeros();
    }

    void addInt(uint32_t v) {
        uint64_t carry = v;
        for (size_t i = 0; i < digits.size() || carry; ++i) {
            if (i == digits.size()) digits.push_back(0);
            uint64_t cur = digits[i] * 1ULL + carry;
            digits[i] = static_cast<uint32_t>(cur & 0xFFFFFFFF);
            carry = cur >> 32;
        }
    }

public:
    BigInt() { digits.push_back(0); }
    BigInt(uint32_t n) { 
        digits.clear();
        digits.push_back(n); 
    }
    BigInt& operator=(uint32_t n) {
        digits.clear();
        digits.push_back(n);
        return *this;
    }

        bool operator==(const BigInt & other) const {
        return digits == other.digits;
    }

    bool operator>(const BigInt& other) const {
        if (digits.size() != other.digits.size()) {
            return digits.size() > other.digits.size();
        }
        for (int i = (int)digits.size() - 1; i >= 0; i--) {
            if (digits[i] != other.digits[i]) {
                return digits[i] > other.digits[i];
            }
        }
        return false;
    }
    
    bool operator<(const BigInt& other) const {
        return other > *this;
    }

    bool operator<=(const BigInt& other) const {
        return !(*this > other);
    }

    BigInt operator+(const BigInt& other) const {
        BigInt res;
        res.digits.clear();

        uint64_t carry = 0;
        size_t n = max(digits.size(), other.digits.size());

        for (size_t i = 0; i < n || carry; ++i) {
            uint64_t sum = carry +
                (i < digits.size() ? digits[i] : 0) +
                (i < other.digits.size() ? other.digits[i] : 0);

            res.digits.push_back(static_cast<uint32_t>(sum & 0xFFFFFFFF));
            carry = sum >> 32;
        }
        return res;
    }

    BigInt operator-(const BigInt& other) const {
        if (*this < other) {
            throw runtime_error("Underflow error: Result of subtraction would be negative.");
        }

        BigInt res = *this;
        uint64_t borrow = 0;

        for (size_t i = 0; i < other.digits.size() || borrow; ++i) {
            uint64_t sub = (i < other.digits.size() ? (uint64_t)other.digits[i] : 0) + borrow;

            if ((uint64_t)res.digits[i] < sub) {
                res.digits[i] = static_cast<uint32_t>(0x100000000ULL + res.digits[i] - sub);
                borrow = 1;
            }
            else {
                res.digits[i] = static_cast<uint32_t>(res.digits[i] - sub);
                borrow = 0;
            }
        }
        res.remove_leading_zeros();
        return res;
    }

    BigInt operator*(const BigInt& other) const {
        if (this == 0 || other == 0) return BigInt(0);

        BigInt res;
        res.digits.assign(digits.size() + other.digits.size(), 0);

        for (size_t i = 0; i < digits.size(); ++i) {
            uint64_t carry = 0;
            for (size_t j = 0; j < other.digits.size() || carry; ++j) {
                uint64_t cur = res.digits[i + j] +
                    digits[i] * 1ULL * (j < other.digits.size() ? other.digits[j] : 0) +
                    carry;

                res.digits[i + j] = static_cast<uint32_t>(cur & 0xFFFFFFFF);
                carry = cur >> 32;
            }
        }

        res.remove_leading_zeros();
        return res;
    }

    static pair<BigInt, BigInt> divmod(BigInt a, BigInt b) {
        if (b == 0) throw runtime_error("Division by zero");
        if (a < b) return { BigInt(0), a };

        BigInt quotient, remainder;
        quotient.digits.assign(a.digits.size(), 0);

        for (int i = (int)a.digits.size() - 1; i >= 0; i--) {
            remainder.multiplyInt(0x100000000ULL); 
            remainder.addInt(a.digits[i]);
            
            uint64_t low = 0, high = 0xFFFFFFFFULL;
            uint32_t q = 0;

            while (low <= high) {
                uint64_t mid = low + (high - low) / 2;
                BigInt temp = b;
                temp.multiplyInt(mid);

                if (temp <= remainder) {
                    q = (uint32_t)mid;
                    low = mid + 1;
                }
                else {
                    high = mid - 1;
                }
            }

            quotient.digits[i] = q;
            BigInt subtractor = b;
            subtractor.multiplyInt(q);
            remainder = remainder - subtractor;
        }

        quotient.remove_leading_zeros();
        remainder.remove_leading_zeros();
        return { quotient, remainder };
    }

    BigInt operator/(const BigInt& other) const {
        return divmod(*this, other).first;
    }

    BigInt operator%(const BigInt& other) const {
        return divmod(*this, other).second;
    }

    BigInt square() const {
        if (*this == 0) return BigInt(0);

        BigInt res;
        size_t n = digits.size();
        res.digits.assign(2 * n, 0);

        for (size_t i = 0; i < n; i++) {
            uint64_t self_prod = digits[i] * 1ULL * digits[i];
            uint64_t carry = self_prod >> 32;

            uint64_t sum = (uint64_t)res.digits[2 * i] + (self_prod & 0xFFFFFFFF);
            res.digits[2 * i] = (uint32_t)(sum & 0xFFFFFFFF);
            carry += (sum >> 32);

            for (size_t k = 2 * i + 1; carry && k < res.digits.size(); k++) {
                uint64_t cur = (uint64_t)res.digits[k] + carry;
                res.digits[k] = (uint32_t)(cur & 0xFFFFFFFF);
                carry = cur >> 32;
            }

            uint64_t inner_carry = 0;
            for (size_t j = i + 1; j < n; j++) {
                uint64_t prod = digits[i] * 1ULL * digits[j];
                uint64_t prod_low = (prod << 1) & 0xFFFFFFFF;
                uint64_t prod_high = (prod >> 31);

                uint64_t current_sum = (uint64_t)res.digits[i + j] + prod_low + inner_carry;
                res.digits[i + j] = (uint32_t)(current_sum & 0xFFFFFFFF);
                inner_carry = prod_high + (current_sum >> 32);
            }

            if (inner_carry) {
                size_t k = i + n;
                while (inner_carry&& k < res.digits.size()) {
                    uint64_t cur = (uint64_t)res.digits[k] + inner_carry;
                    res.digits[k] = (uint32_t)(cur & 0xFFFFFFFF);
                    inner_carry = cur >> 32;
                    k++;
                }
            }
        }

        res.remove_leading_zeros();
        return res;
    }

    size_t bitLength() const {
        if (digits.empty() || (digits.size() == 1 && digits[0] == 0)) return 0;

        size_t bits = (digits.size() - 1) * 32;

        uint32_t lastBlock = digits.back();
        while (lastBlock > 0) {
            lastBlock >>= 1;
            bits++;
        }
        return bits;
    }

    bool testBit(size_t bitIdx) const {
        size_t blockIdx = bitIdx / 32;
        size_t bitInBlock = bitIdx % 32;
        if (blockIdx >= digits.size()) return false;
        return (digits[blockIdx] >> bitInBlock) & 1;
    }

    BigInt Gor(const BigInt& exponent) const {
        if (exponent == 0) return BigInt(1);
        if (*this == 0) return BigInt(0);
        if (*this == 1) return BigInt(1);

        BigInt res(1);
        size_t nBits = exponent.bitLength();

        for (int i = (int)nBits - 1; i >= 0; i--) {
            res = res.square();

            if (exponent.testBit(i)) {
                res = res * (*this);
            }
        }

        return res;
    }

    BigInt killLastDigits(size_t k) const { // = зсув вправо
        if (k >= digits.size()) return BigInt(0);
        BigInt res;
        res.digits.assign(digits.begin() + k, digits.end());
        return res;
    }
    static BigInt calculateMu(const BigInt& n) {
        size_t k = n.digits.size();

        BigInt beta2k;
        beta2k.digits.assign(2 * k + 1, 0);
        beta2k.digits[2 * k] = 1;

        return beta2k / n;
    }

    static BigInt barrettReduction(const BigInt& x, const BigInt& n, const BigInt& mu) {
        size_t k = n.digits.size();

        BigInt q = x.killLastDigits(k - 1);
        q = q * mu;
        q = q.killLastDigits(k + 1);

        BigInt r = x - (q * n);
        while (r > n || r == n) {
            r = r - n;
        }

        return r;
    }

    static BigInt gcd(BigInt a, BigInt b) {
        while (!(b == 0)) {
            a = a % b;
            swap(a, b);
        }
        return a;
    }

    static BigInt lcm(BigInt a, BigInt b) {
        if (a == 0 || b == 0) return BigInt(0);
        return (a / gcd(a, b)) * b;
    }

    BigInt addMod(const BigInt& other, const BigInt& n) const {
        BigInt res = *this + other;
        while (res > n || res == n) {
            res = res - n;
        }
        return res;
    }

    BigInt subMod(const BigInt& other, const BigInt& n) const {
        if (n == 0) throw runtime_error("Division by zero (modulus is 0)");

        BigInt a = *this % n;
        BigInt b = other % n;

        if (a > b || a == b) {
            return (a - b) % n;
        }
        else {
            return n - (b - a);
        }
    }

    BigInt mulMod(const BigInt& other, const BigInt& n) const {
        if (n == 0) throw runtime_error("Division by zero");
        if (n == 1) return BigInt(0);

        BigInt mu = BigInt::calculateMu(n);

        BigInt multiplicationResult = (*this) * other;

        return BigInt::barrettReduction(multiplicationResult, n, mu);
    }

    BigInt squareMod(const BigInt& n) const {
        if (n == 0) throw runtime_error("Division by zero");

        BigInt mu = BigInt::calculateMu(n);
        return BigInt::barrettReduction(this->square(), n, mu);
    }

    BigInt GorMod(const BigInt& d, const BigInt& n) const {
        if (n == BigInt(1)) return BigInt(0);
        if (d == BigInt(0)) return BigInt(1);

        BigInt mu = BigInt::calculateMu(n);

        BigInt res(1);
        BigInt a = *this % n; 
        size_t bits = d.bitLength();

        for (int i = (int)bits - 1; i >= 0; i--) {
            res = BigInt::barrettReduction(res.square(), n, mu);

            if (d.testBit(i)) { 
                res = BigInt::barrettReduction(res * a, n, mu);
            }
        }
        return res;
    }

    void fromHexString(string hexStr) {
        digits.clear();
        for (int i = (int)hexStr.length(); i > 0; i -= 8) {
            int start = max(0, i - 8);
            int len = i - start;
            string part = hexStr.substr(start, len);
            digits.push_back(stoul(part, nullptr, 16));
        }
        remove_leading_zeros();
    }

    string toHexString() const {
        if (digits.empty()) return "0";
        stringstream ss;
        ss << hex << digits.back();
        for (int i = static_cast<int>(digits.size()) - 2; i >= 0; --i) {
            ss << setfill('0') << setw(8) << hex << digits[i];
        }
        return ss.str();
    }

    void fromDecimalString(string s) {
        digits.clear();
        digits.push_back(0); 

        size_t start = 0;
        while (start < s.length()) {
            size_t len = min((size_t)9, s.length() - start);
            string part = s.substr(start, len);
            uint32_t v = stoul(part);

            uint32_t multiplier = 1;
            for (size_t i = 0; i < len; ++i) multiplier *= 10;

            multiplyInt(multiplier);
            addInt(v);

            start += len;
        }
        remove_leading_zeros();
    }

    string toDecimalString() const {
        if (digits.size() == 1 && digits[0] == 0) return "0";
        BigInt temp = *this;
        string res = "";
        while (!(temp.digits.size() == 1 && temp.digits[0] == 0)) {
            uint32_t rem = temp.divInt(1000000000); 
            string s = to_string(rem);
            if (!(temp.digits.size() == 1 && temp.digits[0] == 0)) {
                while (s.length() < 9) s = "0" + s;
            }
            res = s + res;
        }
        return res;
    }

    friend ostream& operator<<(ostream& os, const BigInt& bi) {
        os << bi.toHexString();
        return os;
    }

    /*uint32_t getDigit(size_t i) const {
        if (i >= digits.size()) return 0;
        return digits[i];
    }*/

    static BigInt generateRandom(size_t numBlocks) {
        BigInt res;
        res.digits.resize(numBlocks);

        random_device rd;
        mt19937 gen(rd());
        uniform_int_distribution<uint32_t> dis(0, 0xFFFFFFFF);

        for (size_t i = 0; i < numBlocks; ++i) {
            res.digits[i] = dis(gen);
        }

        res.remove_leading_zeros();
        return res;
    }

    void timeMeasure() {
        const int iterations = 500;
        const size_t size2048 = 64;

        vector<pair<BigInt, BigInt>> data;
        for (int i = 0; i < iterations; ++i) {
            data.push_back({ BigInt::generateRandom(size2048), BigInt::generateRandom(size2048) });
        }

        auto measure = [&](string name, auto func) {
            auto start = high_resolution_clock::now();
            for (int i = 0; i < iterations; ++i) {
                func(data[i].first, data[i].second);
            }
            auto end = high_resolution_clock::now();

            auto avg = duration_cast<nanoseconds>(end - start).count() / iterations;
            cout << left << setw(20) << name << ": " << avg << " ns" << endl;
            };

        cout << "--- Time measure (500) ---" << endl;

        measure("Addition (+)", [](BigInt& a, BigInt& b) { BigInt r = a + b; });
        measure("Multiplication (*)", [](BigInt& a, BigInt& b) { BigInt r = a * b; });
        measure("Division (/)", [](BigInt& a, BigInt& b) { BigInt r = a / b; });
        /*measure("Exponentiation (pow)", [](BigInt& a, BigInt& b) {
            BigInt exp(b.getDigit(0)); // лише перший блок, інакше занадто довго виконується код
            BigInt r = a.Gor(exp);
            });*/
    }
};

int main()
{
    BigInt firstNumber;

    string hexInput = "ffffffffffffffff";
    firstNumber.fromHexString(hexInput);
    cout << "--- Test 1: 2^64 - 1 in HEX ---" << endl;
    cout << "HEX-input: " << hexInput << endl;
    cout << "Result in HEX: " << firstNumber.toHexString() << endl;
    cout << "Result in DEC: " << firstNumber.toDecimalString() << endl;
    cout << "Expected result in DEC: 18446744073709551615" << endl << endl;

    BigInt secondNumber;
    string largeHex = "123456789abcdef0123456789abcdef0";
    secondNumber.fromHexString(largeHex);
    cout << "--- Test 2: 128-bit number ---" << endl;
    cout << "HEX-input: " << largeHex << endl;
    cout << "Result in HEX: " << secondNumber.toHexString() << endl;
    cout << "Result in DEC: " << secondNumber.toDecimalString() << endl << endl;

    BigInt zero(0);
    BigInt one(1);
    cout << "--- Test 3: Const ---" << endl;
    cout << "Zero HEX: " << zero.toHexString() << " | DEC: " << zero.toDecimalString() << endl;
    cout << "One HEX: " << one.toHexString() << " | DEC: " << one.toDecimalString() << endl << endl;

    BigInt thirdNumber;
    string decInput = "18446744073709551615";
    thirdNumber.fromDecimalString(decInput);
    cout << "--- Test 4: 2^64 - 1 in DEC ---" << endl;
    cout << "DEC-input: " << decInput << endl;
    cout << "Result in HEX: " << thirdNumber.toHexString() << endl;
    cout << "Result in DEC: " << thirdNumber.toDecimalString() << endl << endl;

    BigInt A;
    A.fromHexString("ffffffffffffffffffffffffffffffff");
    BigInt B(1);
    cout << "--- Test 5: Sum A + B ---" << endl;
    cout << "A in HEX: " << A.toHexString() << endl;
    cout << "B in HEX: " << B.toHexString() << endl;
    BigInt sum = A + B;
    cout << "Sum in HEX: " << sum.toHexString() << endl;
    cout << "Sum in DEC: " << sum.toDecimalString() << endl << endl;

    BigInt C;
    C.fromHexString("abc123456789");
    cout << "--- Test 6: Sum A + C ---" << endl;
    cout << "A in HEX: " << A.toHexString() << endl;
    cout << "A in DEC: " << A.toDecimalString() << endl;
    cout << "C in HEX: " << C.toHexString() << endl;
    cout << "C in DEC: " << C.toDecimalString() << endl;
    sum = A + C;
    cout << "Sum in HEX: " << sum.toHexString() << endl;
    cout << "Sum in DEC: " << sum.toDecimalString() << endl << endl;
    
    BigInt diff;
    cout << "--- Test 7: Diff A - C ---" << endl;
    diff = A - C;
    cout << "A - C in HEX: " << diff.toHexString() << endl;
    cout << "A - C in DEC: " << diff.toDecimalString() << endl << endl;

    BigInt mult;
    cout << "--- Test 8: Mult A * C ---" << endl;
    mult = A * C;
    cout << "A * C in HEX: " << mult.toHexString() << endl;
    cout << "A * C in DEC: " << mult.toDecimalString() << endl << endl;

    cout << "--- Test 9: Square A^2 ---" << endl;
    cout << "A^2 in HEX: " << A.square().toHexString() << endl;
    cout << "A^2 in DEC: " << A.square().toDecimalString() << endl << endl;

    BigInt X, Y;
    X.fromHexString("100000000000000000000000000000000");
    Y.fromHexString("10000000000000000");
    cout << "--- Test 10: Div X / Y, X % Y ---" << endl;
    BigInt rim = X / Y;
    BigInt sh = X % Y;

    cout << "X: " << X.toHexString() << endl;
    cout << "X: " << X.toDecimalString() << endl;
    cout << "Y: " << Y.toHexString() << endl;
    cout << "Y: " << Y.toDecimalString() << endl;
    cout << "Remainder (X/Y): " << rim.toHexString() << endl;
    cout << "Share (X%Y): " << sh.toHexString() << endl << endl;

    cout << "--- Test 11: 3^10 = ? ---" << endl;
    BigInt base1(3);
    BigInt exp1(10);
    BigInt res1 = base1.Gor(exp1);
    cout << "Result in DEC: " << res1.toDecimalString() << endl;
    cout << "Result in HEX: " << res1.toHexString() << endl;
    cout << "Expected:     59049" << endl << endl;

    cout << "--- Test 12: X^0 = 1? ---" << endl;
    BigInt base2;
    base2.fromHexString("ABCDEF123456789");
    BigInt exp2(0);
    BigInt res2 = base2.Gor(exp2);
    cout << "Test 2: A^0" << endl;
    cout << "Result (HEX): " << res2.toHexString() << endl;
    cout << "Expected:     1" << endl << endl;

    cout << "--- Test 13: 2^128 = ? ---" << endl;
    BigInt base3(2);
    BigInt exp3(128);
    BigInt res3 = base3.Gor(exp3);
    cout << "Result (HEX): " << res3.toHexString() << endl;
    cout << "Expected:     100000000000000000000000000000000" << endl << endl;

    cout << "--- Test 14: Point B checking ? ---" << endl;
    BigInt a;
    a.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
    BigInt b;
    b.fromHexString("ABCDEF1234567890ABCDEF1234567890");
    BigInt c;
    c.fromHexString("1234567890ABCDEF1234567890ABCDEF");

    // (a + b) * c == c * (b + a) ???
    BigInt left1 = (a + b) * c;
    BigInt right1 = c * (b + a);

    cout << "Commutativity: (a + b) * c == c * (b + a)" << endl;
    if (left1 == right1) {
        cout << "Results tally!" << endl;
    }
    else {
        cout << "ERROR!!! Error in commutativity!" << endl;
    }
    cout << "HEX: " << left1.toHexString().substr(0, 20) << "..." << endl << endl;

    // (a + b) * c == a * c + b * c ???
    BigInt left2 = (a + b) * c;
    BigInt right2 = (a * c) + (b * c);

    cout << "Distributivity: (a + b) * c == a * c + b * c" << endl;
    if (left2 == right2) {
        cout << " Results tally!" << endl;
    }
    else {
        cout << "ERROR!!! Error in Distributivity!" << endl;
    }
    cout << "HEX: " << left2.toHexString().substr(0, 20) << "..." << endl << endl;

    BigInt a1(48), b1(18);
    cout << "--- Test 15: (48, 18): ---" << endl;
    cout << "GCD: " << BigInt::gcd(a1, b1).toDecimalString() << endl;
    cout << "LCM: " << BigInt::lcm(a1, b1).toDecimalString() << endl << endl;

    BigInt factor;
    factor.fromHexString("ABCDEF123456");
    BigInt a3 = factor * BigInt(3);
    BigInt b3 = factor * BigInt(5);

    cout << "--- Test 16: GCD * LCM = a * b ? ---" << endl;
    BigInt propertyLeft = BigInt::gcd(a3, b3) * BigInt::lcm(a3, b3);
    BigInt propertyRight = a3 * b3;

    if (propertyLeft == propertyRight) {
        cout << "Property is confirmed!" << endl << endl;
    }
    else {
        cout << "ERROR!!! Property is not confirmed!" << endl << endl;
    }

    BigInt a2; 
    a2.fromHexString("FFFFFFFFFFFFFFFF");
    BigInt b2(1);
    BigInt n2;
    n2.fromHexString("FFFFFFFFFFFFFFFF");

    cout << "--- Test 17 (a + 1 mod a): ---" << endl;
    cout << "Result: " << a2.addMod(b2, n2).toDecimalString() << endl << endl;

    BigInt n3(10);

    cout << "--- Test 18 (18 - 3 mod 10): ---" << endl;
    BigInt a5(18), b5(3);
    cout << "Result: " << a5.subMod(b5, n3).toDecimalString() << endl << endl;

    BigInt n4; 
    n4.fromHexString("FFFFFFFFFFFFFFFF");
    BigInt a4(1), b4(2);

    cout << "--- Test 19 (1 - 2 mod (2^64-1)): ---" << endl;
    cout << "Result: " << a4.subMod(b4, n4).toHexString() << endl << endl;

    BigInt n6; 
    n6.fromHexString("100000000");
    BigInt a6 = n6 + BigInt(1);
    BigInt b6 = n6 + BigInt(2);

    cout << "--- Test 20 ((2^32+1)(2^32+2) mod (2^32)): ---" << endl;
    cout << "Result: " << a6.mulMod(b6, n6).toDecimalString() << endl << endl;

    BigInt x7(2), exp7(10), n7(1000);
    BigInt res7 = x7.GorMod(exp7, n7);

    cout << "--- Test 21 (2^10 mod 1000): ---" << endl;
    cout << "Result " << res7.toDecimalString() << endl << endl;

    cout << "--- Test 22: Point B checking ? ---" << endl;
    cout << "((a + b) * c) mod n == (a*c + b*c) mod n" << endl << endl;

    BigInt N;
    N.fromHexString("FFFFFFFFFFFFFFFF");
    BigInt K;
    K.fromHexString("A1B2C3D4E5F6");
    BigInt L;
    L.fromHexString("9876543210AB");
    BigInt M;
    M.fromHexString("ABCDEF123456");

    // ((K + L) mod N * M) mod N
    BigInt sum_kl_mod = K.addMod(L, N);
    BigInt leftSide = sum_kl_mod.mulMod(M, N);

    // (K*M mod N + L*M mod N) mod N
    BigInt km_mod = K.mulMod(M, N);
    BigInt lm_mod = L.mulMod(M, N);
    BigInt rightSide = km_mod.addMod(lm_mod, N);

    cout << "L-side result: " << leftSide.toHexString() << endl;
    cout << "R-side result: " << rightSide.toHexString() << endl;

    if (leftSide == rightSide) {
        cout << "\n Results tally!" << endl << endl;
    }
    else {
        cout << "\nERROR!!! Results do not tally!" << endl << endl;
    }

    /*BigInt tester;
    tester.timeMeasure();*/

    return 0;
}