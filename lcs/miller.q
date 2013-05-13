//Implementation of "An O(NP) Sequence Comparison Algorithm" 
//by Sun Wu, Udi Manber, Gene Myers and Web Miller
//Note: P=(D-(M-N))/2, D is shortest edit distance, M>=N

ses:{[a;b;f]
      //TODO: swap if n < m
      n:count b;m:count a; delta:n-m; offset:m+1;
      fp:(m+n+3)#(-1);p:-1;
      pdict:(); /array of fp values indexed on p
      snakef:snake[;;;;;;f];
      while[fp[delta+offset] < n;
        p+:1;
        ct:delta+p; k:neg p; do[ct; fp[k+offset]:snakef[a;b;m;n;k;max(fp[k+offset-1]+1;fp[k+offset+1])];k+:1];
        ct:p;k:delta+p;do[ct;fp[k+offset]:snakef[a;b;m;n;k;max(fp[k+offset-1]+1;fp[k+offset+1])];k-:1];
        fp[delta+offset]: snakef[a;b;m;n;k;max(fp[delta+offset-1]+1;fp[delta+offset+1])];
        pdict,:enlist fp;
       ];
    :pdict;
  }

//snake are like snake in snake-and-ladders board. To get from top of the board
//to bottom of the board in shortest path, you need longest sequence of diagonals. 
//Hence, the term snake used by Myers et al
//Note: index x in the function corresponds to position x+1 in the figure 1 of paper - 
//so (3,2) is (4,3) in the paper
snake:{[a;b;m;n;k;y;f]
  xp:x:(yp:y)-k; 
  while[(x<m) and (y<n) and f[a[x];b[y]]; x+:1; y+:1];
  0N!(xp;yp;x;y);y}

backsnake:{[a;b;m;n;k;y;f]
    x:(y:y-1)-k; 
    while[f[a[x];b[y]] and (0<x) and (0<y);0N!(x;y);x-:1;y-:1];:y+1}
