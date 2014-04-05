#include <iostream>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <set>
#include <queue>
#include <cmath>

using namespace std;

typedef pair<double, double> couple;
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

  Edge(int A = 0, int B = 0, int D = 0, int C = 0, int L = 0) : A(A), B(B), D(D), C(C), L(L), traversed(false) {}
  bool oneway() {
    return (D == 1);
  }
};

int N, M, T, C, S;
vector< vector<int> > Adj;
vector<Vertex> V;
vector<Edge> E;
vector< vector< pair<int, int> > > Solution;
vector<int> Start;
vector< pair<int, int> > prev;

inline double 
gain(int car, int e, int t) {
  if (E[e].traversed) {
    return 0.0;
  }

  return ((double)E[e].L / (double)E[e].C);
}

inline int 
chose_edge(int car, int vertex, int t) {
  double best_gain = -1.0;
  int best_edge = -1;
  rep(i, Adj[vertex].size()) {
    int edge_ind = Adj[vertex][i];
    double g = gain(car, edge_ind, t);
    if (g > best_gain && E[edge_ind].C <= t) {
      best_gain = g;
      best_edge = edge_ind;
    }
  }

  // Si tout est deja traverse, on chose en random
  if (best_gain == 0.0) {
    vector<int> good_edges;
    rep(i, Adj[vertex].size()) {
      if (E[Adj[vertex][i]].C <= t) {
        good_edges.push_back(Adj[vertex][i]);
      }
    }
    int r = rand() % good_edges.size();
    return good_edges[r];
  }

  return best_edge;
}

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

inline void
traverse(int _s, int car, vector< pair<int, int> >& solution, int _t) {
  int pos = _s;
  int t = _t;
  int next_edge = -1;

  //  solution.push_back(make_pair(pos, _e));

  while ((next_edge = chose_edge(car, pos, t)) != -1) {
    E[next_edge].traversed = true;
    t -= E[next_edge].C;
    pos = endpoint(pos, next_edge);
    solution.push_back(make_pair(pos, next_edge));
  }
}

struct Node {
  int v;
  int d;
  int e;

  bool operator < (const Node& other) const {
    return (d > other.d); // TAS MAX
  }

  Node(int v = 0, int d = 0, int e = 0) : v(v), d(d), e(e) {}
};

inline void
dijkstra(int start_v) {
  priority_queue<Node> Q;
  vector<bool> traversed(N, false);

  Q.push(Node(start_v, 0));

  while (!Q.empty()) {
    Node cur = Q.top();
    Q.pop();

    if (traversed[cur.v]) {
      continue;
    }
    
    traversed[cur.v] = true;

    if (cur.v != start_v) {
      prev[cur.v] = make_pair(endpoint(cur.v, cur.e), cur.e);
    }

    rep(i, Adj[cur.v].size()) {
      int edge_ind = Adj[cur.v][i];
      int next_v = endpoint(cur.v, edge_ind);
      if (!traversed[next_v]) {
        Q.push(Node(next_v, cur.d + E[edge_ind].C, edge_ind));
      }
    }
  }
}

inline int
dispatch(int car, int v) {
  int pos = v;
  int time = 0;
  while (pos != S) {
    Solution[car].push_back(make_pair(pos, prev[pos].second));
    time += E[prev[pos].second].C;
    E[prev[pos].second].traversed = true;
    pos = prev[pos].first;
  }

  Solution[car].push_back(make_pair(S, 0));

  reverse(Solution[car].begin(), Solution[car].end());

  return time;
}

inline void
solve() {
  Solution.resize(C);

  prev.resize(N);
  dijkstra(S);

  rep(i, C) {
    int t = dispatch(i, Start[i]);
    traverse(Start[i], i, Solution[i], T - t); 
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

inline void
clear() {
  prev.clear();
  rep (i, C) {
    Solution[i].clear();
  }
  rep (i, M) {
    E[i].traversed = false;
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

int
main(int argc, char *argv[]) {
  read_input();

  // srand ?
  // Send to random 8 start points
  Start.push_back(3709);
  Start.push_back(10066);
  Start.push_back(6839);
  Start.push_back(11278);
  Start.push_back(9404);
  Start.push_back(5086);
  Start.push_back(6036);
  Start.push_back(9731);

  rep(i, C) {
    cerr << "(" << Start[i] << ")" << endl;
  }

  if (argc == 3) {
    int from = atoi(argv[1]);
    int to = atoi(argv[2]);

    int best_score = 0;

    for (int i = from; i <= to; ++i) {
      srand(i);
      solve();
      int s = score();
      if (s > best_score) {
        cout << s << " (" << i << ")" << endl;
        best_score = s;
      }
      clear();
    }
  } else if (argc == 2) {
    srand(atoi(argv[1]));
    solve();
    print_output();
  } else {
    cerr << "Usage : one arg -> run, two args -> optim_seed" << endl;
  }

  return 0;
}
