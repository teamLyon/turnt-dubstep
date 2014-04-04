#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

#define rep(i, n) for (int i = 0; i < n; ++i)

int N, M;
vector<string> I;

void
read_input() {
  cin >> N >> M;
  string buf;
  rep(i, N) {
    cin >> buf;
    I.push_back(buf);
  }
}

void
print_img(const vector<string>& img) {
  rep(i, img.size()) {
    cout << img[i] << endl;
  }
}

void
solve() {
  vector< pair<int, int> > res;
  rep(i, N) {
    rep(j, M) {
      if (I[i][j] == '#') {
        res.push_back(make_pair(i, j));
      }
    }
  }

  cout << res.size() << endl;
  rep(i, res.size()) {
    cout << "PAINTSQ " << res[i].first << " " << res[i].second << " 0" << endl;
  }
}

int
main() {
  read_input();
  print_img(I);

  solve();

  return 0;
}
