#include <iostream>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <set>
#include <cmath>
using namespace std;

#define rep(i, n) for (int i = 0; i < n; ++i) 

typedef pair<double,double> couple;

struct Vertex {
  double lat, lng;

  Vertex(double lat = 0, double lng = 0) : lat(lat), lng(lng) {}
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

inline int
endpoint(int v, int e) {
  if (E[e].A == v) {
    return E[e].B;
  } else if (E[e].B == v && !E[e].oneway()) {
    return E[e].A;
  } else {
    return -1;
  }
}


inline couple vect_dir (int car)
{
  double sq2 = sqrt(2);
  if (car == 0) {
    return couple(0.0,-1.0);
  }if (car == 1) {
    return couple(0.0,-1.0);
  }if (car == 2) {
    return couple(0.0,-1.0);
  }if (car == 1) {
    return couple(sq2,-sq2);
  }if (car == 1) {
    return couple(0.0,-1.0);
  }if (car == 1) {
    return couple(-sq2,-sq2);
  }if (car == 1) {
    return couple(-1.0,0.0);
  }if (car == 1) {
    return couple(-1.0,0.0);
  }

}



inline double
direction (int car, int e, int A) {
  int B = endpoint(A,e);
  double norme = sqrt(( V[A].lat- V[B].lat)*( V[A].lat- V[B].lat) + ( V[A].lng- V[B].lng)*( V[A].lng- V[B].lng));
  double y  = (V[B].lat-V[A].lat) / norme;
  double x  = (V[B].lng-V[A].lng) / norme;
  couple dir = vect_dir(car);
  return x*dir.first + y*dir.second + 2.0;
  //  return (V[B].lat - V[A].lat) +10.0;
}

inline double 
gain(int car, int e, int vertex, int t) {

  if (E[e].traversed) {
    return 0.0;
  }
  int time = 25000;
  if (t < time)
    return (((double)E[e].L / (double)E[e].C)*direction(car, e, vertex));  

  return ((double)E[e].L / (double)E[e].C );
}

inline int 
chose_edge(int car, int vertex, int t) {
  double best_gain = -1.0;
  int best_edge = -1;
  rep(i, Adj[vertex].size()) {
    int edge_ind = Adj[vertex][i];
    double g = gain(car, edge_ind, vertex, t);
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


inline void
traverse(int car, vector< pair<int, int> >& solution) {
  int pos = S;
  int t = T;
  int next_edge = -1;

  solution.push_back(make_pair(S, -1));

  while ((next_edge = chose_edge(car, pos, t)) != -1) {
    E[next_edge].traversed = true;
    t -= E[next_edge].C;
    pos = endpoint(pos, next_edge);
    solution.push_back(make_pair(pos, next_edge));
  }
}

void
solve() {
  Solution.resize(C);  
  rep(i, C) {
    traverse(i, Solution[i]); 
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

double dist(double lat,double lng,int i) {
    double norme = sqrt(( lat- V[i].lat)*( lat- V[i].lat) + ( lng- V[i].lng)*( lng- V[i].lng));
}

int closest_point (double lat, double lng)
{
  int point = 0;
  double dist_temp = 0.0;
  double dist_min = dist(lat,lng,point);
  rep(i,V.size()) {
    dist_temp = dist(lat,lng,i); 
    if (dist_temp < dist_min)
      {
	point = i;
	dist_min = dist_temp;
      }
  }
  return point;
}

double epsilon(){
  double res = ((double) rand() / (double) RAND_MAX) / (double) 10.0;
  return res;
}

vector<int> findpoints()
{
  double lat_min,lat_max,lng_min,lng_max;
  lat_min = V[0].lat;  lat_max = V[0].lat;  lng_max = V[0].lng;  lng_min = V[0].lng;
  rep(i,V.size()) {
    lat_min = (V[i].lat < lat_min) ? V[i].lat : lat_min;
    lat_max = (V[i].lat > lat_max) ? V[i].lat : lat_max;
    lng_min = (V[i].lng < lng_min) ? V[i].lng : lng_min;
    lng_max = (V[i].lng > lng_max) ? V[i].lng : lng_max;
  }
  vector<couple> t;
  t.push_back(couple(0.15,0.5));
  t.push_back(couple(0.2,0.3));
  t.push_back(couple(0.2,0.7));
  t.push_back(couple(0.5,0.8));
  t.push_back(couple(0.5,0.2));
  t.push_back(couple(0.8,0.5));
  t.push_back(couple(0.5,0.5));
  t.push_back(couple(0.8,0.2));
  vector<int> points;
  rep(i,8){
    double lat = lat_min + (t[i].first + epsilon()) * (lat_max - lat_min);
    double lng = lng_min + (t[i].second + epsilon()) * (lng_max - lng_min);
    points.push_back(closest_point(lat,lng));
  }
  return points;
}

int
main(int argc, char *argv[]) {
  read_input();
  findpoints();

  return 0;
}
