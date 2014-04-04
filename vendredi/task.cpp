#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

#define rep(i, n) for (int i = 0; i < n; ++i)

const int COTE = 2;

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

inline bool
check_square(int i, int j, int k) {
  for (int l = -k; l <= k && i+l < N; ++l) {
    for (int m = -k; m <= k && j+m < M; ++m) {
      if (I[i+l][j+m] != '#')
        return false;
    }
  }

  return true;
}

void
solve() {
  vector< pair< pair<int, int>, int> > res;
  for (int i = COTE; i < N; i += 2*COTE+1) {
    for (int j = COTE; j < M; j+= 2*COTE+1) {
      if (check_square(i, j, COTE)) {
        res.push_back(make_pair(make_pair(i, j), COTE));
      } else {
        for (int l = -COTE; l <= COTE && i+l < N; ++l) {
          for (int m = -COTE; m <= COTE && j+m < M; ++m) {
            if (I[i+l][j+m] == '#') {
              res.push_back(make_pair(make_pair(i+l, j+m), 0));
            }
          }
        }
      }
    }
  }

  cout << res.size() << endl;
  rep(i, res.size()) {
    cout << "PAINTSQ " << res[i].first.first << " " << res[i].first.second << " " << res[i].second << endl;
  }
}

int
main() {
  read_input();

  solve();

  return 0;
}
