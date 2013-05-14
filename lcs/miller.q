//Implementation of "An O(NP) Sequence Comparison Algorithm" 
//by Sun Wu, Udi Manber, Gene Myers and Web Miller
//Note: P=(D-(M-N))/2, D is shortest edit distance, M>=N

lcs:{[a;b;f] $[flipped;{(x[1];x[0])};(::)] $[flipped:(count a)>count b;lcsh[b;a;f];lcsh[a;b;f]]}

lcsh:{[a;b;f]
      n:count b;m:count a; delta:n-m; offset:m+1;
      @[`.;;:;(m+n+3)#(-1)] each `snodes`fp;p:-1; /snake nodes, further points, p value
      pdict:(); /array of fp values indexed on p
      @[`.;`snakearr;:;()]; /array of snakes. How does that sound?
      snakef:snakes[;;;f;];
      while[fp[delta+offset] < n;
        p+:1;
        ct:delta+p; k:neg p; 
        snakef[a;b;k;ct];
        ct:p;k:delta+p;
        snakef[a;b;k;ct];
        snakef[a;b;delta;1];
        pdict,:enlist fp;
       ];
    @[`.;`paths;:;()];
    //iterate backwards starting from last snake on delta (i.e., destination (n,m)) and work backwards to (0,0) where 
    //snode is -1. So, condition for iterate is set to check for snode value
    {[d;i] if[d[i][3]>0;@[`.;`paths;,;enlist (d[i][1];d[i][2];d[i][3])]]; d[i][0]}[snakearr;]/[-1<;-1+ count snakearr];
    idx: { (x[0]+i;x[1]+i:til x[2])} each reverse paths;
    ![`.;();0b;`fp`snodes`snakearr`paths]; /delete the global arrays
    :(raze idx[;0];raze idx[;1]);
  }
 
//snake are like snake in snake-and-ladders board. To get from top of the board
//to bottom of the board in shortest path, you need longest sequence of diagonals. 
//Hence, the term snake used by Myers et al
//Note: index x in the function corresponds to position x+1 in the figure 1 of paper - 
//so (3,2) is (4,3) in the paper
snakes:{[a;b;k;f;ct]
  m:count a;n:count b;offset:m+1;
  vert:$[k <(n-m);1b;0b]; /if below diagonal, vertical else horizontal
  do[ct;
    $[(fp[k+offset-1]+1)>fp(k+offset+1);[kp:k+offset-1;yp:fp[kp]+1];[kp:k+offset+1;yp:fp[kp]]];
    xp:x:(y:yp)-k; 
    while[(x<m) and (y<n) and f[a[x];b[y]]; x+:1; y+:1];
    fp[k+offset]::y;
    @[`.;`snakearr;,;enlist (snodes[kp];xp;yp;(y-yp))];
    snodes[k+offset]:: -1 + count snakearr;
    $[vert;k+:1;k-:1]];
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
  il:lcs[a;b;{[x;y] all x=y}]; /replace with call to lcs instead if not optimizing
  dela: (til count a) except il[0]; //return delta indices, i.e., not a common subsequence  in a
  delb: (til count b) except il[1]; //return delta indices, i.e., not a common subsequence  in b
  :(i1 dela; i2 delb) //return the delta indices in original table
 }

