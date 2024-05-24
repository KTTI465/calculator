#define _USE_MATH_DEFINES

#include <iostream>
#include <stack>
#include <sstream>
#include <fstream>
#include <vector>
#include <map>
#include <regex>
#include <cmath>

using namespace std;

map<string, double> define = {
    {"PI", M_PI}, {"e", M_E}
};

map<string, map<string, double>> tempDefine;
map<string, string> tempDefineCal;

map<string, vector<double>> tempVec;

bool isFunction(string fom) {
    if (fom[0] == '~') fom.erase(0, 1);

    if (fom == "sin" || fom == "cos" || fom == "tan" || fom == "log" || fom == "exp" || fom == "sqrt" ||
        fom == "abs" || fom == "floor" || fom == "ceil" || fom == "round" || fom == "rad")
        return true;

    return false;
}

map<string, int> opeLv = {
    {"+", 1}, {"-", 1}, {"*", 2}, {"/", 2}, {"^", 3},
    {"(", 0}, {")", 0}
};

vector<string> Split(string fom) {
    vector<string> temp;
    regex r("^~*([0-9]+(?:\\.[0-9]+)?)|^~*\\w+\\{\\w+(\\,+\\w+)*\\}|\\w+|[+*/()\\-^]|^~*[a-zA-Z]+");
    smatch tsm;

    fom.erase(remove(fom.begin(), fom.end(), ' '), fom.end());

    while (regex_search(fom, tsm, r)) {
        temp.push_back(tsm[0].str());
        fom = tsm.suffix();
    }

    return temp;
}

string RPN(vector<string> formula, bool flg = false) {
    stack<string> opeTemp;
    vector<string> temp;
    string rpn;

    regex rd("^~*\\d+");
    regex rk0("\\(");
    regex rk1("\\)");
    regex rst("^~*[a-zA-Z]+\#*");
    smatch smt;

    for (auto fom : formula) {
        if (regex_search(fom, smt, rd)) {
            temp.push_back(fom);
        }
        else if (regex_search(fom, smt, rst)) {
            auto it = define.find(fom);

            if (isFunction(fom))
                opeTemp.push(fom);
            else if (it != define.end() && flg) {
                stringstream ss;
                ss << define[fom];
                temp.push_back(ss.str());
            }
            else
                temp.push_back("$" + fom);
        }
        else if (regex_search(fom, smt, rk0)) {
            opeTemp.push(fom);
        }
        else if (regex_search(fom, smt, rk1)) {
            while (opeTemp.top() != "(") {
                temp.push_back(opeTemp.top());
                opeTemp.pop();
            }
            opeTemp.pop();
            if (!opeTemp.empty() && regex_search(opeTemp.top(), smt, rst)) {
                temp.push_back(opeTemp.top());
                opeTemp.pop();
            }
        }
        else {
            while (!opeTemp.empty() && (opeLv[opeTemp.top()] >= opeLv[fom])) {
                temp.push_back(opeTemp.top());
                opeTemp.pop();
            }
            if (!opeTemp.empty() && regex_search(opeTemp.top(), smt, rst)) {
                temp.push_back(opeTemp.top());
                opeTemp.pop();
            }
            opeTemp.push(fom);
        }
    }

    while (!opeTemp.empty()) {
        temp.push_back(opeTemp.top());
        opeTemp.pop();
    }

    for (auto fom : temp) {
        rpn += fom + " ";
    }
    rpn.pop_back();

    return rpn;
}

double calculation(string formula, string funcname = "", bool flgs = false) {
    stack<double> opes;
    stringstream ss(formula);

    regex rd("^~*([0-9]+(?:\\.[0-9]+)?)");
    regex ro("[+*/^\\-]");
    regex rst("^~*[a-z|A-Z]+");
    regex rfst(R"((^\$)(~*[a-zA-Z]+))");
    regex rfsst(R"((^\$)(~*(\w+\{\w+(\,+\w+)*\})))");
    regex rfssst(R"((\w+)\{(.*)\})");
    smatch smt, ssmt;

    while (!ss.eof()) {
        string fom;
        ss >> fom;

        if (regex_search(fom, smt, rd)) {
            double num;

            if (smt[0].str()[0] == '~') {
                fom.erase(0, 1);
                istringstream(fom) >> num;
                num *= -1;
            }
            else {
                istringstream(fom) >> num;
            }

            opes.push(num);
        }
        else if (regex_search(fom, smt, rfsst)) {
            auto st = smt[2].str();
            double id;
            vector<double> temp;

            regex_search(st, ssmt, rfssst);
            stringstream ss(ssmt[2]);

            while (ss >> id) {
                if (ss.peek() == ',') {
                    ss.ignore();
                }
                temp.push_back(id);
            }
            int i = 0;
            auto variable = tempDefine[ssmt[1].str()];

            for (auto& varia : variable) {

                tempDefine[ssmt[1].str()][varia.first] = temp[i++];
            }

            double ope = calculation(tempDefineCal[ssmt[1].str()], ssmt[1].str(), true);

            if (smt[2].str()[0] == '~') {
                ope *= -1;
            }

            opes.push(ope);
        }
        else if (regex_search(fom, smt, rst)) {

            double ope = opes.top();
            bool flg = false;
            opes.pop();

            if (fom[0] == '~') {
                fom.erase(0, 1);
                flg = true;
            }

            if (fom == "sin")   ope = sin(ope);
            if (fom == "cos")   ope = cos(ope);
            if (fom == "tan")   ope = tan(ope);
            if (fom == "log")   ope = log(ope);
            if (fom == "exp")   ope = exp(ope);
            if (fom == "sqrt")  ope = sqrt(ope);
            if (fom == "abs")   ope = abs(ope);
            if (fom == "floor") ope = floor(ope);
            if (fom == "ceil")  ope = ceil(ope);
            if (fom == "round") ope = round(ope);
            if (fom == "rad")   ope = ope * (M_PI / 180);

            if (flg)    opes.push(-ope);
            else        opes.push(ope);
        }
        else if (regex_search(fom, smt, rfst)) {
            bool flg = false;
            string temp = smt[2].str();

            if (temp[0] == '~') {
                temp.erase(0, 1);
                flg = true;
            }
            if (flgs) {
                if (flg)    opes.push(-tempDefine[funcname][funcname + '_' + temp]);
                else        opes.push(tempDefine[funcname][funcname + '_' + temp]);
            }
            else {
                if (flg)    opes.push(-define[temp]);
                else        opes.push(define[temp]);
            }
        }
        else if (regex_search(fom, smt, ro)) {
            double ope2 = opes.top();
            opes.pop();
            double ope1 = opes.top();
            opes.pop();

            switch (fom[0]) {
            case '+':
                opes.push(ope1 + ope2);
                break;
            case '-':
                opes.push(ope1 - ope2);
                break;
            case '*':
                opes.push(ope1 * ope2);
                break;
            case '/':
                opes.push(ope1 / ope2);
                break;
            case '^':
                opes.push(pow(ope1, ope2));
                break;
            }
        }
    }

    return opes.top();
}

bool isDefine(string fom) {
    vector<string> temp;
    regex r(R"(^(\w+)=(.*))");
    regex rsfst(R"((\w+\{\w+(\,+\w+)*\})=(.*))");
    regex rsfsst(R"((\w+)\{(.*)\}=(.*))");
    regex rsv(R"(^(\w+)=\<(.*)\>)");
    smatch tsm, ttsm;

    fom.erase(remove(fom.begin(), fom.end(), ' '), fom.end());

    if (regex_search(fom, tsm, rsv)) {
        stringstream ss(tsm[2]);
        double id;

        while (ss >> id) {
            tempVec[tsm[1].str()].push_back(id);

            if (ss.peek() == ',') {
                ss.ignore();
            }
        }

        cout << "true" << endl;
        return true;
    }
    else if (regex_search(fom, tsm, r)) {
        auto sf = Split(tsm[2].str());
        if (sf.size() > 1) {
            auto temp = RPN(sf);
            auto atemp = calculation(temp);
            define[tsm[1].str()] = atemp;
        }
        else {
            if (tsm[2].str()[0] == '~') {
                auto stt = tsm[2].str();
                stt.erase(0, 1);
                define[tsm[1].str()] = stod(stt) * -1;
            }
            else {
                define[tsm[1].str()] = stod(tsm[2].str());
            }
        }

        cout << "定義:\t\t" << tsm[1].str() << " = " << define[tsm[1].str()] << endl << endl;
        return true;
    }
    else if (regex_search(fom, tsm, rsfst)) {
        auto sf = Split(tsm[3].str());
        auto tempR = RPN(sf, true);
        char id;

        regex_search(fom, ttsm, rsfsst);
        stringstream ss(ttsm[2]);

        while (ss >> id) {
            if (ss.peek() == ',') {
                ss.ignore();
            }

            tempDefine[ttsm[1].str()][ttsm[1].str() + '_' + id] = 0;
        }

        tempDefineCal[ttsm[1].str()] = tempR;

        cout << "定義:\t\t" << tsm[1].str() << " = " << tsm[3].str() << endl << endl;
        return true;
    }
    else {
        return false;
    }
    return false;
}

void vecCal(string inputtxt = "") {
    string input;
    vector<string> st;
    if (inputtxt.size() < 1) {
        getline(cin, input);
        st = Split(input);
    }
    else {
        st = Split(inputtxt);
    }
    int i = 0;
    vector<double> temp;

    auto s1 = tempVec[st[0]];
    auto s2 = tempVec[st[2]];

    if (s1.size() == s2.size()) {
        if (st[1] == "^" && s1.size() == 3) {
            temp.push_back(s1[1] * s2[2] - s1[2] * s2[1]);
            temp.push_back(s1[2] * s2[0] - s1[0] * s2[2]);
            temp.push_back(s1[0] * s2[1] - s1[1] * s2[0]);
        }
        else {
            for (int i = 0; i < s1.size(); i++) {
                if (st[1] == "+") {
                    temp.push_back(s1[i] + s2[i]);
                }
                if (st[1] == "-") {
                    temp.push_back(s1[i] - s2[i]);
                }
                if (st[1] == "*") {
                    temp.push_back(s1[i] * s2[i]);
                }
            }
        }

        if (st[1] == "*") {
            double t = 0.0;
            for (auto s : temp) {
                t += s;
            }
            cout << "結果:\t\t" << t << endl;
        }
        else {
            cout << "結果:\t\t<";
            for (int i = 0; i < temp.size(); i++) {
                cout << temp[i] << ", ";
            }
            cout << "\b\b>" << endl;
        }
    }
    else cout << "次元数が異なります" << endl;

    cout << endl;
}

void Action(string input) {
    string rpn;
    vector<string> fom;

    if (!isDefine(input) || input == "vec") {

        fom = Split(input);

        rpn = RPN(fom);

        cout << "入力式:\t\t" << input << endl;

        cout << "逆ポ変換:\t" << rpn << endl;

        double result = calculation(rpn);

        cout << "結果:\t\t" << result << endl << endl;
    }
}

int main(int, char**) {
    string input;

    while (true) {
        getline(cin, input);

        if (input.size() < 1) continue;

        if (input == "end") return 0;

        if (input == "vec") vecCal();

        if (input == "file") {
            string path;
            cout << "Path : ";
            getline(cin, path);

            ifstream ifs(path);

            if (!ifs) {
                cout << "Could not open file" << endl;
            }
            else {
                string line, linei;
                while (getline(ifs, line)) {
                    if (line.size() < 1) continue;

                    if (line == "vec") {
                        while (getline(ifs, linei)) {
                            if (linei.size() < 1) {
                                continue;
                            }
                            else {
                                vecCal(linei);
                                break;
                            }
                        }
                    }
                    else Action(line);
                }
            }
            ifs.close();
        }
        else {
            Action(input);
        }
    }

    return 0;
}