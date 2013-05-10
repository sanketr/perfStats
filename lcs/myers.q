//calculate shortest edit path from a to b
// Returns an int encoding shortest number of edits (insertions and deletions) to 
// transform a to b
// If the return is null, this function is wrong - reject and investigate!
ses:{[a;b;f]
  maxv:max(0;(n:count a)+m:count b);
  v: (1+2*maxv)#0; //initialize array of V
  offset: maxv;
  x:0;y:0;
  ddict:(enlist 0)!enlist v;
  d:0;while[maxv >= d+:1; //Constraint: when d is maxv, kmin is 0, kmax is 2*maxv => array access is safe since array is 1+2*maxv length
          kmin:(neg d) + offset; kmax: d + offset; k:-2 + kmin;
          while[kmax >= k+:2;
            $[(k = kmin) or ((k < kmax) and v[-1+k] < v[k+1]);
                x: v[k+1];
                x: 1 + v[-1+k]];
            y:x-(k - offset); //note "original diagonal k" is always (k - offset) because of array offsets here - to calculate y, we need original diagonal k
            //now find matches for a[x:] and b[y:]
            while[(x<n) and (y<m) and f[a[x+1];b[y+1]];x+:1;y+:1]; //f is boolean function on a[i],b[j]
            v[k]:x;
            0N!(x;y;k-offset;d);
            if[(x >= n) and (y >= m);ddict,:(enlist d)!enlist v;:(ddict;d)];
            ];
          ddict,:(enlist d)!enlist v; //save V after each d - we will use it to build longest common subsequence
        ];
    :(()!();0N) //shouldn't ever get here - it is a bug if it gets here
  }

//Add code to calculate longest common subsequence backwards
path:{[a;b;vl]
  x:count a; y:count b;
  offset:max(0;x+y);
  k: -1;
  d: count vl; /number of iterations 
  //at most d diagonals - so, we need to store only that many start and end points of diagonals => 2*d elements
  paths: (2*d) # enlist(-1;-1);
  while[(x>0) and (y>0) and -1 < d-:1;
    v: vl d;
    k: x-y; //calculate which diagonal we are on - we intialize with N-M, and backtrack to diagonal 0
    paths[d]: (v[k + offset];v[k+offset] - k); /end point of diagonal
    down: (k = neg d) or ((k < d) and (v[k+offset-1] < v[k+offset+1]));
    kprev: $[down;k+1;k-1];
    xstart: v[kprev + offset];ystart: xstart - kprev;
    xmid: $[down;xstart;xstart+1]; ymid: xmid - k; //mid point is on the diagonal - this is starting point of the diagonal
    paths[d-1]:(xmid;ymid);
    x: xstart;
    y: ystart;
    ];
  }
