//Implementation of Eugene Myer's "O(ND) Difference Algorithm and Its Variations"
//D is number of differences between two sequences - when the sequences are mostly
//similar, this algorithm is fast

//Note: Call lcs function to calculate longest common subsequence

//calculate shortest edit sequence from a to b
// Returns a list whose count is D, shortest number of edits 
// (insertions and deletions) to transform a to b

// If the return is empty list, things are very, very wrong!
findLCS:{[a;b;f]
  offset:maxv:(n:count a)+m:count b;
  v: (1+2*maxv)#0; //initialize array of V
  x:0;y:0;
  darr:(); //at most maxv entries since D in Myer's diff algorithm can't exceed maxv
  d:-1;while[maxv >= d+:1; //Constraint: when d is maxv, kmin is 0, kmax is 2*maxv => array access of v is safe since v is 1+2*maxv length
          kmin:(neg d) + offset; kmax: d + offset; k:-2 + kmin;
          while[kmax >= k+:2;
            $[(k = kmin) or ((k < kmax) and v[-1+k] < v[k+1]);
                x: v[k+1];
                x: 1 + v[-1+k]];
            y:x-(k - offset); //note "original diagonal k" is always (k - offset) because of array offsets here - to calculate y, we need original diagonal k
            //now find matches for a[x:] and b[y:]
            while[(x<n) and (y<m) and f[a[x];b[y]];x+:1;y+:1]; //f is boolean function on a[i],b[j]
            v[k]:x;
            if[(x >= n) and (y >= m);:darr,:enlist v];
            ];
          darr,:enlist v; //save V after each d - we will use it to build longest common subsequence
        ];
    :() //shouldn't ever get here - it is a logical bug if it gets here
  }

lcs2:{[a;b;f]
      snakearr: snakes[a;b;f];
      @[`.;`paths;:;()];
      {[d;i] if[d[i][3]>0;@[`.;`paths;,;enlist (d[i][1];d[i][2];d[i][3])]]; d[i][0]}[snakearr;]/[-1<;-1+ count snakearr];
      idx: { (x[0]+i;x[1]+i:til x[2])} each reverse paths;
      ![`.;();0b;enlist `paths];
      :(raze idx[;0];raze idx[;1]);}

//calculate shortest edit sequence from a to b
// Returns a list of snakes

// If the return is empty list, things are very, very wrong!
snakes:{[a;b;f]
  offset:maxv:(n:count a)+m:count b;
  v: (1+2*maxv)#0; //initialize array of V
  x:0;y:0;
  snodes:(1+2*maxv)#-1; 
  snakearr:(); /array of snakes - at most maxv entries since D in Myer's diff algorithm can't exceed maxv
  d:-1;
  while[maxv >= d+:1; //Constraint: when d is maxv, kmin is 0, kmax is 2*maxv => array access of v is safe since v is 1+2*maxv length
          kmin:(neg d) + offset; kmax: d + offset; k:-2 + kmin;
          while[kmax >= k+:2;
            $[(k = kmin) or ((k < kmax) and v[-1+k] < v[k+1]);
                x: v[kp:k+1];
                x: 1 + v[kp:-1+k]];
            yp:y:(xp:x)-(k - offset); //note "original diagonal k" is always (k - offset) because of array offsets here - to calculate y, we need original diagonal k
            //now find matches for a[x:] and b[y:]
            while[(x<n) and (y<m) and f[a[x];b[y]];x+:1;y+:1]; //f is boolean function on a[i],b[j]
            v[k]:x;
            if[(x >= n) and (y >= m);:snakearr,:enlist (snodes[kp];xp;yp;(y-yp))];
            snakearr,:enlist (snodes[kp];xp;yp;(y-yp)); 
            snodes[k]:-1 + count snakearr;
            ];
        ];
    :() //shouldn't ever get here - it is a logical bug if it gets here
  }


//Function to calculate longest common subsequence backwards: as proven in Myer's O(ND)
// paper to find the LCS, it suffices to start on diagonal (n-m) and shortest edit 
//distance d, and traverse backwards (basically, inverse of path building, but with 
//only one particular path traversed back - that path has only one unique way of reaching 
//back to diagonal 0(remember diagonal k=x-y=0-0=0 at origin). All the diagonals in the
//path are matches and their indices in a and b provide us indices of respective longest 
//common subsequence. Horizontal and vertical paths are deltas
diags:{[a;b;vl]
  x:count a; y:count b;
  offset:x+y;
  k: -1;
  d: count vl; /number of iterations 
  //at most d diagonals - so, we need to store only that many start and end points of diagonals => 2*d elements
  paths: (2*d) # enlist(-1;-1);
  while[(x>=0) and (y>=0) and -1 < d-:1;
    v: vl d;
    idx:d*2;
    k: x-y; //calculate which diagonal we are on - we intialize with N-M, and backtrack to diagonal 0
    paths[idx + 1]: (v[k + offset];v[k+offset] - k); /end point of diagonal
    down: (k = neg d) or ((k < d) and (v[k+offset-1] < v[k+offset+1]));
    kprev: $[down;k+1;k-1]; /at (0,0) where there is no down, kprev is -1 => y is -1 in first entry of idx
    xstart: v[kprev + offset];ystart: xstart - kprev;
    xmid: $[down;xstart;xstart+1]; ymid: xmid - k; //mid-point is on the diagonal - this is starting point of the diagonal
    paths[idx]:(xmid;ymid);
    x: xstart;
    y: ystart;
    ];
  :paths;
  }

//get indices in a and b for longest common sequence given equality function f
//Example: lcs["ABCABA";"CABBA";=]
lcs:{[a;b;f] 
      d: diags[a;b;] findLCS[a;b;f];
      //Lot of things are happening in this pipeline:
      // d contains indices (x;y) of LCS - index x for a, 
      // y for b. In d, there are two entries for every
      // match:
      // - start point of the match
      // - End point of the match
      // if a[1] and b[2] match and match for consecutive 3
      // elements, two entries in d will look like below:
      // (2,3) - indices for the match start - in the algorithm, there is offset of 1
      // (4,5) - indices at which last matching element for this subsequence is
      // Notice delta x = delta y when there is a match (this is a diagonal after all - 
      // x and y will have identical delta along the diagonal) 
      // - this is the trick we use to detect matching subsequences
      // deltas d[;0] = deltas d[;1] => where x and y deltas match - these are the diagonals if delta > 0
      // deltas d[;0] > 0 => take only the matches where number of matches > 0 - filter out 0-length diagonals
      // til 1 + x[1;0] - x[0;0] - how many matches in x - enumerate and add it to the starting index x[0;0]
      // subtract -1 from the indices since they are off by 1
      :(-1+) each raze each flip {(1 _ x[0;0] + til 1+x[1;0]-x[0;0];1 _ x[0;1] + til 1+x[1;1]-x[0;1] )} each d (-1 0) +/: where (d1 > 0) and (d1:deltas d[;0]) = deltas d[;1];
  }

//Optimized version of lcs function - chops off common suffix and prefix before running
//LCS algorithm
lcsopt:{[a;b;f] 
  //find common prefix and common suffixes first - this is an optimization to reduce 
  //time for LCS - you may discard this if you prefer simplicity
  psl:{[x;y;f] pi: where not f'[x;(count x)#y]; pl: (count x)^first pi;c1:neg (count x)-   pl; si: where not f'[c1#x;c1#y]; sl: (neg c1)-0^1 + last si;(pl;sl)} . $[(count a)>count b; (b;a;f);(a;b;f)];
  a1: (neg psl[1]) _ psl[0] _ a; /chop off common prefixes and suffixes
  b1: (neg psl[1]) _ psl[0] _ b;
  //if a1 or b1 is empty after prefix/suffix chop-off,  the empty one is subsequence
  //of other. The non-empty one is delta.
  //lcs is prefix+suffix - return - don't call findLCS as it works only on non-empty
  //sequences
  $[(0=count a1) or 0=(count b1);:({[p;s;x] (p#x),(neg s)#x}[psl[0];psl[1];] each (til count a;til count b));
    d: diags[a1;b1;] findLCS[a1;b1;f]];
 idx :((psl[0]-1)+) each raze each flip {(1 _ x[0;0] + til 1+x[1;0]-x[0;0];1 _ x[0;1] + til 1+x[1;1]-x[0;1] )} each d (-1 0) +/: where (d1 > 0) and (d1:deltas d[;0]) = deltas d[;1]; 
 :((pi, (first idx),((neg psl[1])+count a) + si);((pi:til psl[0]), (last idx),((neg psl[1])+count b) + si:til psl[1]));
  }
  
//Wrapper to find mutations between two tables given s in sym column. c columns are used
//for mutation check - for example, price and size columns have very good quality signal 
//for trade mutation check
//Example: diffTables[t1;t2;`ABC;`price`size]
diffTables:{[t1;t2;s;c]
  i1: exec i from t1 where sym in s;
  i2: exec i from t2 where sym in s;
  a:flip (t1 i1) c;
  b:flip (t2 i2) c;
  il:lcsopt[a;b;{[x;y] all x=y}]; /replace with call to lcs instead if not optimizing
  dela: (til count a) except il[0]; //return delta indices, i.e., not a common subsequence in a
  delb: (til count b) except il[1]; //return delta indices, i.e., not a common subsequence in b
  :(i1 dela; i2 delb) //return the delta indices in original table
 }
