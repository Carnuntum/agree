#include<vector>
#include<iostream>

std::vector<double> loop(std::vector<std::vector<double>> conv_mat, double M, double ni) {
          std::vector<double> above{};
          std::vector<double> out{};
          double wjk = 0.;
          double prod = 0.;
          double abv_temp = 0.;
          const double ni_const = (ni * (ni - 1));
          
          auto sum = [&](std::vector<double> input) {
            auto out = 0.;
            for(int i = 0; i < input.size(); ++i) {
              out += input.at(i);
            }
            return(out);
          };
          
          for(int i = 0; i < conv_mat.size(); ++i) {
            for(int j = 0; j < M; ++j) {
              for(int k = 0; k < M; ++k) {
                wjk = 1 - (std::abs(j - k) / (M - 1));
                prod = wjk * (conv_mat.at(i).at(j) * conv_mat.at(i).at(k));
                above.push_back(prod);
              }
            }
            abv_temp = sum(above) - ni;
            abv_temp = abv_temp / ni_const;
            out.push_back(abv_temp);
            abv_temp = 0.;
            above.clear();
          }
          return(out);
        }

int main() {


    std::vector<std::vector<double>> x{};
    std::vector<double> a{1,2,3};
    std::vector<double> b{4,5,6};
    x.push_back(a);
    x.push_back(b);
    double t = 3;
    double tt = 7;

    std::vector<double> out = loop(x, t, tt);
    for(int i = 0; i < out.size(); ++i) {
      std::cout << out.at(i);
    }
}


