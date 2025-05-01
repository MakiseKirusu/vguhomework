#include <bits/stdc++.h>
using namespace std;
class BigInteger {
private:
    string digits;         
    bool   negative = false;

    static string strip(const string &s) {
        size_t p = s.find_first_not_of('0');
        return (p == string::npos) ? "0" : s.substr(p);
}

    static int cmpAbs(const string &a, const string &b) {
        if (a.size() != b.size()) return (a.size() < b.size()) ? -1 : 1;
        return a.compare(b);
}

    static string addAbs(const string &a, const string &b) {
        string res;
        int i = (int)a.size() - 1, j = (int)b.size() - 1, carry = 0;
        while (i >= 0 || j >= 0 || carry) {
            int sum = carry;
            if (i >= 0) sum += a[i--] - '0';
            if (j >= 0) sum += b[j--] - '0';
            res.push_back(char('0' + (sum % 10)));
            carry = sum / 10;
        }
        reverse(res.begin(), res.end());
        return strip(res);}

    static string subAbs(const string &a, const string &b) {
        string res;
        int i = (int)a.size() - 1, j = (int)b.size() - 1, borrow = 0;
        while (i >= 0) {
            int diff = (a[i] - '0') - borrow - (j >= 0 ? (b[j] - '0') : 0);
            if (diff < 0) { diff += 10; borrow = 1; }
            else borrow = 0;
            res.push_back(char('0' + diff));
            --i; --j;
        }
        while (res.size() > 1 && res.back() == '0') res.pop_back();
        reverse(res.begin(), res.end());
        return strip(res);
}

    static string mulAbs(const string &a, const string &b) {
        vector<int> prod(a.size() + b.size(), 0);
        for (int i = (int)a.size() - 1; i >= 0; --i) {
            for (int j = (int)b.size() - 1; j >= 0; --j) {
                int mul = (a[i] - '0') * (b[j] - '0');
                int p   = i + j + 1;
                int sum = prod[p] + mul;
                prod[p]     = sum % 10;
                prod[p - 1] += sum / 10;
            }
        }
        string res;
        for (int d : prod) res.push_back(char('0' + d));
        return strip(res);
}

    static string mulAbsByDigit(const string &a, int digit) {
        if (digit == 0) return "0";
        if (digit == 1) return a;
        string res;
        int carry = 0;
        for (int i = (int)a.size() - 1; i >= 0; --i) {
            int prod = (a[i] - '0') * digit + carry;
            res.push_back(char('0' + (prod % 10)));
            carry = prod / 10;
        }
        if (carry) res.push_back(char('0' + carry));
        reverse(res.begin(), res.end());
        return strip(res);
}

    static string divAbs(const string &a, const string &b, string &remainder) {
        if (b == "0") throw runtime_error("Division by zero");
        string cur;                 
        string quotient;             
        for (char ch : a) {
            if (!cur.empty() || ch != '0') cur.push_back(ch);
            else cur = "0";
            int qDigit = 0;
            if (cmpAbs(cur, b) >= 0) {
                int lo = 1, hi = 9;
                while (lo <= hi) {
                    int mid = (lo + hi) / 2;
                    string prod = mulAbsByDigit(b, mid);
                    int cmp = cmpAbs(prod, cur);
                    if (cmp <= 0) { qDigit = mid; lo = mid + 1; }
                    else hi = mid - 1;
                }
                cur = subAbs(cur, mulAbsByDigit(b, qDigit));
            }
            quotient.push_back(char('0' + qDigit));
        }
        quotient = strip(quotient);
        remainder = strip(cur);
        return quotient;
}

public:
    BigInteger() : digits("0"), negative(false) {}
    explicit BigInteger(string s) {
        if (s.empty()) s = "0";
        if (s[0] == '-') { negative = true; s.erase(0, 1); }
        digits = strip(s);
        if (digits == "0") negative = false;
}

    BigInteger operator+(const BigInteger &o) const {
        BigInteger r;
        if (negative == o.negative) {
            r.digits   = addAbs(digits, o.digits);
            r.negative = negative;
        } else {
            int cmp = cmpAbs(digits, o.digits);
            if (cmp == 0) {
                r.digits = "0"; r.negative = false;
            } else if (cmp > 0) {
                r.digits = subAbs(digits, o.digits);
                r.negative = negative;
            } else {
                r.digits = subAbs(o.digits, digits);
                r.negative = o.negative;
            }
        }
        if (r.digits == "0") r.negative = false;
        return r;
}

    BigInteger operator-(const BigInteger &o) const {
        BigInteger tmp = o;
        tmp.negative = !tmp.negative;
        return *this + tmp;
}

    BigInteger operator*(const BigInteger &o) const {
        BigInteger r;
        r.digits   = mulAbs(digits, o.digits);
        r.negative = negative ^ o.negative;
        if (r.digits == "0") r.negative = false;
        return r;
}

    BigInteger operator/(const BigInteger &o) const {
        string rem;
        string q = divAbs(digits, o.digits, rem);
        BigInteger r;
        r.digits   = q;
        r.negative = negative ^ o.negative;
        if (r.digits == "0") r.negative = false;
        return r;
}

    string str() const { return (negative ? "-" : "") + digits; }
    bool isZero() const { return digits == "0"; }
};

enum class TokType { NUM, OP, LPAR, RPAR };

struct Token {
    TokType type;
    string  val;
};

static bool isOp(char c) { return c == '+' || c == '-' || c == '*' || c == '/'; }

static int precedence(const string &op) {
    return (op == "+" || op == "-") ? 1 : 2;  
}

vector<Token> tokenize(const string &expr) {
    vector<Token> toks;
    for (size_t i = 0; i < expr.size();) {
        char c = expr[i];
        if (isspace(c)) { ++i; continue; }
        if (isdigit(c)) {
            size_t j = i;
            while (j < expr.size() && isdigit(expr[j])) ++j;
            toks.push_back({TokType::NUM, expr.substr(i, j - i)});
            i = j;
        } else if (isOp(c)) {
            toks.push_back({TokType::OP, string(1, c)});
            ++i;
        } else if (c == '(') {
            toks.push_back({TokType::LPAR, "("}); ++i;
        } else if (c == ')') {
            toks.push_back({TokType::RPAR, ")"}); ++i;
        } else {
            throw runtime_error("Invalid character in input");
        }
    }
    return toks;
}

vector<Token> infixToPostfix(const vector<Token> &infix) {
    vector<Token> out;
    stack<Token> opSt;
    for (const Token &tk : infix) {
        if (tk.type == TokType::NUM) out.push_back(tk);
        else if (tk.type == TokType::OP) {
            while (!opSt.empty() && opSt.top().type == TokType::OP &&
                   precedence(opSt.top().val) >= precedence(tk.val)) {
                out.push_back(opSt.top()); opSt.pop();
            }
            opSt.push(tk);
        } else if (tk.type == TokType::LPAR) opSt.push(tk);
        else if (tk.type == TokType::RPAR) {
            while (!opSt.empty() && opSt.top().type != TokType::LPAR) {
                out.push_back(opSt.top()); opSt.pop();
            }
            if (opSt.empty()) throw runtime_error("Mismatched parentheses");
            opSt.pop(); 
        }
    }
    while (!opSt.empty()) {
        if (opSt.top().type == TokType::LPAR) throw runtime_error("Mismatched parentheses");
        out.push_back(opSt.top()); opSt.pop();
    }
    return out;
}

BigInteger evalPostfix(const vector<Token> &post) {
    stack<BigInteger> st;
    for (const Token &tk : post) {
        if (tk.type == TokType::NUM) st.push(BigInteger(tk.val));
        else {
            if (st.size() < 2) throw runtime_error("Malformed expression");
            BigInteger b = st.top(); st.pop();
            BigInteger a = st.top(); st.pop();
            BigInteger r;
            if (tk.val == "+") r = a + b;
            else if (tk.val == "-") r = a - b;
            else if (tk.val == "*") r = a * b;
            else if (tk.val == "/") r = a / b;
            st.push(r);
        }
    }
    if (st.size() != 1) throw runtime_error("Malformed expression");
    return st.top();
}

string evaluate(const string &expr) {
    auto toks   = tokenize(expr);
    auto post   = infixToPostfix(toks);
    BigInteger r = evalPostfix(post);
    return r.str();
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " <tests.txt> <output_StudentID.txt>\n";
        return 1;}
    ifstream in(argv[1]);
    if (!in) { cerr << "Cannot open input file\n"; return 1; }

    ofstream out(argv[2]);
    if (!out) { cerr << "Cannot open output file\n"; return 1; }

    string line;
    while (getline(in, line)) {
        if (line.find_first_not_of(" \t\r\n") == string::npos) continue; 
        try {
            string res = evaluate(line);
            cout << res << '\n';
            out  << res << '\n';
        } catch (const exception &ex) {
            string msg = string("Error: ") + ex.what();
            cout << msg << '\n';
            out  << msg << '\n';
        }
    }
    return 0;
}
