#include<iostream>
//using std::cout;
//using std::endl;
//using std::ostream;
#include<stack>
//using std::stack;
#include<tuple>
//using std::pair;
//using std::make_pair;

class BSTree
{
public:
  BSTree(int k, int v, BSTree *left, BSTree *right) :
  key(k), value(v), _left(left), _right(right), _count(0) {}

  BSTree() : _left(NULL), _right(NULL), _count(0) {}

  void insert(int k, int v);

  BSTree *left() {
    return _left;
  }
  
  BSTree *right() {
    return _right;
  }
  
  int count() {
    return _count;
  }

  bool is_leaf() {
    return _left == NULL && _right == NULL;
  }

private:
  BSTree *_left, *_right;
  int key, value, _count;
};

bool is_balanced(BSTree *tree) {
  std::stack<std::pair<int, BSTree*>> st;
  st.push(std::make_pair(0,tree));
  int dmax = 0, dmin = tree->count();

  while (!st.empty()) {
    auto p = st.top();
    st.pop();
    int d = p.first;
    BSTree *t = p.second;
    if (t->is_leaf()) {
      dmin = std::min(dmin, d);
      dmax = std::max(dmax, d);
    } else {
      st.push(std::make_pair(d+1, t->left()));
      st.push(std::make_pair(d+1, t->right()));
    }
  }

  return abs(dmin - dmax) <= 1;
}

class Complex
{
public:
  Complex(double real, double imaginary = 0)
  : _real(real),_imaginary(imaginary) {}

  Complex operator+(Complex other) const
  {
    Complex temp = *this;
    temp._real += other._real;
    temp._imaginary += other._imaginary;

    return temp;
  }
  
  template <typename T>
  friend T& ::operator<<(T& os, Complex x)
  {
    os<<"("<<x._real<<","<<x._imaginary<<")";

    return os;
  }

  Complex operator++()
  {
    ++_real;
    return *this;
  }

  Complex operator++(int)
  {
    Complex temp = *this;
    ++_real;
    return temp;
  }
private:
  double _real, _imaginary;
};

int main() {
  const Complex x(1,2), y(2,3);
  std::cout << (y+x) << std::endl << abs(2-3) << std::endl;
}
