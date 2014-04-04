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

// inline bool
// check_square(int i, int j, int k) {
//   for (int l = -k; l <= k && i+l < N; ++l) {
//     for (int m = -k; m <= k && j+m < M; ++m) {
//       if (I[i+l][j+m] != '#')
//         return false;
//     }
//   }

//   return true;
// }

inline bool
check_square(int i, int j, int k) {
  int cnt = 0;
  for (int l = 0; l < k && i+l < N; ++l)
    for (int m = 0; m < k && j+m < M; ++m)
      cnt += (I[i+l][j+m] == '#');

  return cnt > 3*(k*k)/4;
}

vector< pair< pair<int, int>, int> > res;
vector< pair<int, int> > erase;

void
solve(int tl_i, int tl_j, int k) {
  int kk = k/3;

  if (k == 1) {
    if (I[tl_i][tl_j] == '#') {
      res.push_back(make_pair(make_pair(tl_i, tl_j), 0));
    }
    return;
  }

  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      int x = tl_i + i*kk;
      int y = tl_j + j*kk;
      if (x >= N || y >= M) {
        continue;
      }
      if (check_square(x, y, kk)) {
        int c = (kk-1)/2;
        res.push_back(make_pair(make_pair(x + c, y + c), c));
        for (int _i = 0; _i < kk; ++_i)
          for (int _j = 0; _j < kk; ++_j) {
            if (I[x+_i][y+_j] == '.') {
              erase.push_back(make_pair(x+_i, y+_j));
            }
          }
      } else {
        solve(x, y, kk);
      }
    }
  }
}

void
solve() {
  for (int i = 0; i < 50; ++i)
    for (int j = 0; j < 50; ++j) {
      solve(i, j, 2187);
      cout << i << " " << j << endl;
      cout << res.size() + erase.size() << endl;
      // rep(i, res.size()) {
      //   cout << "PAINTSQ " << res[i].first.first << " " << res[i].first.second << " " << res[i].second << endl;
      // }
      // rep(i, erase.size()) {
      //   cout << "ERASECELL " << erase[i].first << " " << erase[i].second << endl;
  }

  // vector<string> J(I);

  // for (int i = 0; i < N; ++i)
  //   for (int j = 0; j < M; ++j)
  //     J[i][j] = '.';

  // for (int i = 0; i < res.size(); ++i) {
  //   int cote = res[i].second;
  //   int c_i = res[i].first.first;
  //   int c_j = res[i].first.second;
  //   for (int k = c_i - cote; k <= c_i + cote; ++k) 
  //     for (int l = c_j - cote; l <= c_j + cote; ++l) 
  //       J[k][l] = '#';
  // }

  // print_img(J);
}

int
main() {
  read_input();

  solve();

  return 0;
}
