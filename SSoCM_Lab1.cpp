#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
#include <sstream>

using namespace std;

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
    cout << "Test 3: 2^128" << endl;
    cout << "Result (HEX): " << res3.toHexString() << endl;
    cout << "Expected:     100000000000000000000000000000000" << endl << endl;

    cout << "--- Test 14: Point B checking ? ---" << endl;
    try {
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
            cout << "[OK] Results tally!" << endl;
        }
        else {
            cout << "[ERROR] Error in commutativity!" << endl;
        }
        cout << "HEX: " << left1.toHexString().substr(0, 20) << "..." << endl << endl;

        // (a + b) * c == a * c + b * c ???
        BigInt left2 = (a + b) * c;
        BigInt right2 = (a * c) + (b * c);

        cout << "Distributivity: (a + b) * c == a * c + b * c" << endl;
        if (left2 == right2) {
            cout << "[OK] Results tally!" << endl;
        }
        else {
            cout << "[ERROR] Error in Distributivity!" << endl;
        }
        cout << "HEX: " << left2.toHexString().substr(0, 20) << "..." << endl << endl;

    }
    catch (const exception& e) {
        cerr << "There is error: " << e.what() << endl;
    }

    return 0;
}