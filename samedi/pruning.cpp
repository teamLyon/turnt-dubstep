#include <iostream>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <set>
#include <map>
#include <queue>
#include <cmath>

using namespace std;

typedef vector< pair<int, int> > Path;

#define rep(i, n) for (int i = 0; i < n; ++i) 

struct Vertex {
  int lat, lng;

  Vertex(int lat = 0, int lng = 0) : lat(lat), lng(lng) {}
};

struct Edge {
  int A, B;
  int D;
  int C;
  int L;
  bool traversed;

  Edge(int A = 0, int B = 0, int D = 0, int C = 0, int L = 0) 
    : A(A), B(B), D(D), C(C), L(L), traversed(false) {}
  bool oneway() {
    return (D == 1);
  }
};

int N, M, T, C, S;
vector< vector<int> > Adj;
vector<Vertex> V;
vector<Edge> E;
vector< vector< pair<int, int> > > Solution;
int Depth;

inline int
endpoint(int v, int e) {
  if (E[e].A == v) {
    return E[e].B;
  } else if (E[e].B == v) {// && !E[e].oneway()) {
    return E[e].A;
  } else {
    cerr << "ATTENTION endpoint()" << endl;
    return -1;
  }
}

inline double 
score_path(const Path& p) {
  if (p.size() == 0) {
    return 0.0;
  }
  set<int> S;
  int C = 0, L = 0;
  rep(i, p.size()) {
    int edge_ind = p[i].second;
    C += E[edge_ind].C;
    if (S.find(edge_ind) == S.end() && !E[edge_ind].traversed) {
      L += E[edge_ind].L;
      S.insert(edge_ind);
    }
  }

  return (double)L / (double)C;
}

inline bool
good_path(const Path& p) {
  map<int, int> S;
  rep(i, p.size()) {
    int edge_ind = p[i].second;
    int n = (S.find(edge_ind) != S.end()) ? S[edge_ind] : 0;
    if (n > 1) {
      return false;
    }
    S[edge_ind] = n+1;
  }

  return true;
}

inline void
prune_path(int car, int v, int t, int k, Path& p, Path& best_p) {
  if (k == 0) {
    if (score_path(p) > score_path(best_p) 
        || best_p.size() == 0) {
      best_p = p;
    } 
    return;
  }

  random_shuffle(Adj[v].begin(), Adj[v].end());

  rep(i, Adj[v].size()) {
    int edge_ind = Adj[v][i];
    int next_v = endpoint(v, edge_ind);
    if (t - E[edge_ind].C < 0) {
      continue;
    }
    
    p.push_back(make_pair(next_v, edge_ind));
    if (good_path(p)) {
      prune_path(car, next_v, t - E[edge_ind].C, k-1, p, best_p);
    }
    p.pop_back();
  }
}

inline void
traverse(int car) {
  int pos = S;
  int t = T;

  for (;;) {
    Path p, pp;
    prune_path(car, pos, t, Depth, pp, p);
    if (p.size() == 0) {
      break;
    }

    for (int i = 0; i < p.size(); ++i) {
      int edge_ind = p[i].second;
      E[edge_ind].traversed = true;
      t -= E[edge_ind].C;
      Solution[car].push_back(p[i]);
    }

    pos = p[p.size()-1].first;
  }
}

inline void
solve() {
  Solution.resize(C);

  rep(i, C) {
    Solution[i].push_back(make_pair(S, 0));
    traverse(i); 
  }
}

void
read_input() {
  cin >> N >> M >> T >> C >> S;
  
  Adj.resize(N);
  V.resize(N);
  E.resize(M);

  rep(i, N) {
    double lat, lng;
    cin >> lat >> lng;
    V[i] = Vertex(lat, lng);
  }

  rep(i, M) {
    int A, B, D, C, L;
    cin >> A >> B >> D >> C >> L;
    E[i] = Edge(A, B, D, C, L);
    Adj[A].push_back(i);
    if (D == 2) {
      Adj[B].push_back(i);
    }
  }
}

void
print_output() {
  cout << C << endl;
  rep(i, C) {
    cout << Solution[i].size() << endl;
    rep(j, Solution[i].size()) {
      cout << Solution[i][j].first << endl;
    }
  }
}

inline int
score() {
  set<int> edges;
  int res = 0;
  rep(i, C) {
    rep(j, Solution[i].size()) {
      if (j == 0) continue;
      edges.insert(Solution[i][j].second);
    }
  }

  for (set<int>::iterator it = edges.begin(); it != edges.end(); ++it) {
    res += E[*it].L;
  }

  return res;
}

void 
print_stats() {
  int score = 0;
  int missed_edges = 0;
  int missed_edges_length = 0;
  rep(i, M) {
    if (E[i].traversed) {
      score += E[i].L;
    } else {
      ++missed_edges;
      missed_edges_length += E[i].L;
    }
  }

  cerr << "Score: " << score << endl;
  cerr << "Missed edges: " << missed_edges 
       << "(" << missed_edges_length << "m)" << endl;
  cerr << "Total length: " << (score + missed_edges_length) << endl;
}

int
main(int argc, char *argv[]) {
  read_input();

  if (argc == 2) {
    Depth = atoi(argv[1]);
    solve();
    print_output();
    print_stats();
  } else {
    cerr << "Usage : ./a.out depth" << endl;
  }

  return 0;
}
